/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.cache;

import android.annotation.SuppressLint;
import android.content.Context;
import android.net.Uri;
import android.os.Build;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.github.luben.zstd.Zstd;
import com.github.luben.zstd.ZstdInputStream;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.datastream.MemoryDataStream;
import org.quantumbadger.redreader.common.datastream.SeekableFileInputStream;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

public final class CacheManager {

	private static final String TAG = "CacheManager";

	private static final String ext = ".rr_cache_data";
	private static final String tempExt = ".rr_cache_data_tmp";

	private static final AtomicBoolean isAlreadyInitialized = new AtomicBoolean(false);
	private final CacheDbManager dbManager;

	private final PriorityBlockingQueue<CacheRequest> requests
			= new PriorityBlockingQueue<>();

	private final PrioritisedDownloadQueue downloadQueue;
	private final PrioritisedCachedThreadPool mDiskCacheThreadPool
			= new PrioritisedCachedThreadPool(2, "Disk Cache");

	private final Context context;

	@SuppressLint("StaticFieldLeak") private static CacheManager singleton;

	public static synchronized CacheManager getInstance(final Context context) {
		if(singleton == null) {
			singleton = new CacheManager(context.getApplicationContext());
		}
		return singleton;
	}

	private CacheManager(final Context context) {

		if(!isAlreadyInitialized.compareAndSet(false, true)) {
			throw new RuntimeException("Attempt to initialize the cache twice.");
		}

		this.context = context;

		dbManager = new CacheDbManager(context);

		downloadQueue = new PrioritisedDownloadQueue(context);

		final RequestHandlerThread requestHandler = new RequestHandlerThread();
		requestHandler.start();
	}

	@Nullable
	private Long isCacheFile(@NonNull final File file) {

		final String name = file.getName();

		if(!name.endsWith(ext)) {
			return null;
		}

		final String[] nameSplit = name.split("\\.");
		if(nameSplit.length != 2) {
			return null;
		}

		try {
			return Long.parseLong(nameSplit[0]);
		} catch(final Exception e) {
			return null;
		}
	}

	private void getCacheFileList(final File dir, final HashSet<Long> currentFiles) {

		final File[] list = dir.listFiles();
		if(list == null) {
			return;
		}

		for(final File file : list) {

			if(file.isDirectory()) {
				getCacheFileList(file, currentFiles);

			} else {
				final Long cacheFileId = isCacheFile(file);

				if(cacheFileId != null) {
					currentFiles.add(cacheFileId);
				}
			}
		}
	}

	private static void pruneTemp(final File dir) {

		final String[] list = dir.list();
		if(list == null) {
			return;
		}

		for(final String file : list) {

			if(file.endsWith(tempExt)) {
				new File(dir, file).delete();
			}
		}
	}

	@NonNull
	public static ArrayList<File> getCacheDirs(final Context context) {

		final ArrayList<File> dirs = new ArrayList<>();

		dirs.add(context.getCacheDir());

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
			for(final File dir : context.getExternalCacheDirs()) {
				if(dir != null) {
					dirs.add(dir);
				}
			}

		} else {
			final File extDir = context.getExternalCacheDir();
			if(extDir != null) {
				dirs.add(extDir);
			}
		}

		return dirs;
	}

	public void pruneTemp() {
		final List<File> dirs = getCacheDirs(context);
		for(final File dir : dirs) {
			pruneTemp(dir);
		}
	}

	public synchronized void pruneCache() {
		pruneCache(PrefsUtility.pref_cache_maxage());
	}

	public synchronized void pruneCache(
			final boolean clearListings,
			final boolean clearThumbnails,
			final boolean clearImages) {

		if(!clearListings && !clearThumbnails && !clearImages) {
			return;
		}

		/*Use a maximum age of 0 to clear everything* in that category.
		Otherwise, use Long.MAX_VALUE as the maximum age to ensure that nothing is deleted.

		*May not clear everything if system time shenanigans have occurred.*/
		pruneCache(PrefsUtility.createFileTypeToLongMap(
				clearListings ? 0 : Long.MAX_VALUE,
				clearThumbnails ? 0 : Long.MAX_VALUE,
				clearImages ? 0 : Long.MAX_VALUE
		));
	}

	public synchronized void pruneCache(final HashMap<Integer, Long> maxAge) {

		try {

			final HashSet<Long> currentFiles = new HashSet<>(1024);

			final List<File> dirs = getCacheDirs(context);
			for(final File dir : dirs) {
				getCacheFileList(dir, currentFiles);
			}

			final ArrayList<Long> filesToDelete = dbManager.getFilesToPrune(
					currentFiles,
					maxAge,
					72);

			Log.i("CacheManager", "Pruning " + filesToDelete.size() + " files");

			for(final long id : filesToDelete) {
				final File file = getExistingCacheFile(id);
				if(file != null) {
					file.delete();
				}
			}

		} catch(final Throwable t) {
			BugReportActivity.handleGlobalError(context, t);
		}

	}

	public synchronized void emptyTheWholeCache() {
		dbManager.emptyTheWholeCache();
	}

	public synchronized HashMap<Integer, Long> getCacheDataUsages() {
		final HashMap<Integer, Long> dataUsagePerType = PrefsUtility.createFileTypeToLongMap();

		try {
			final HashSet<Long> currentFiles = new HashSet<>(128);

			final List<File> dirs = getCacheDirs(context);
			for(final File dir : dirs) {
				getCacheFileList(dir, currentFiles);
			}

			final HashMap<Long, Integer> filesToCheckWithTypes = dbManager.getFilesToSize();

			for(final HashMap.Entry<Long, Integer> fileEntry : filesToCheckWithTypes.entrySet()) {
				final long id = fileEntry.getKey();
				final int type = fileEntry.getValue();

				final File file = getExistingCacheFile(id);
				if(file != null && dataUsagePerType.containsKey(type)) {
					dataUsagePerType.put(
							type,
							Objects.requireNonNull(dataUsagePerType.get(type)) + file.length());
				}
			}

		} catch(final Throwable t) {
			BugReportActivity.handleGlobalError(context, t);
		}

		return dataUsagePerType;
	}

	public void makeRequest(final CacheRequest request) {
		requests.put(request);
	}

	public List<CacheEntry> getSessions(final URI url, final RedditAccount user) {
		return dbManager.select(url, user.username, null);
	}

	public File getPreferredCacheLocation() {
		return new File(
				PrefsUtility.pref_cache_location(context));
	}

	@NonNull
	public ReadableCacheFile getExistingCacheFileById(
			final long cacheId,
			@NonNull final CacheCompressionType cacheCompressionType) {
		return new ReadableCacheFile(cacheId, cacheCompressionType);
	}

	public class WritableCacheFile {

		private final OutputStream mOutStream;
		private ReadableCacheFile readableCacheFile = null;
		@NonNull final URI mUrl;
		@NonNull final RedditAccount mUser;
		final int mFileType;
		private final File location;
		private boolean mWriteExternally = false;

		@NonNull private final UUID mSession;
		@Nullable private final String mMimetype;

		@NonNull private final CacheCompressionType mCacheCompressionType;
		@NonNull private final File mTmpFile;

		private long mUncompressedLength = 0;
		private long mCompressedLength = 0;

		private WritableCacheFile(
				@NonNull final URI url,
				@NonNull final RedditAccount user,
				final int fileType,
				@NonNull final UUID session,
				@Nullable final String mimetype,
				@NonNull final CacheCompressionType cacheCompressionType) throws IOException {

			mUrl = url;
			mUser = user;
			mFileType = fileType;
			mSession = session;
			mMimetype = mimetype;
			mCacheCompressionType = cacheCompressionType;

			location = getPreferredCacheLocation();
			mTmpFile = new File(location, UUID.randomUUID().toString() + tempExt);

			mOutStream = new FileOutputStream(mTmpFile);
		}

		@NonNull
		public ReadableCacheFile getReadableCacheFile() {
			return Objects.requireNonNull(readableCacheFile);
		}

		public void writeWholeFile(
				final byte[] buf,
				final int offset,
				final int length) throws IOException {

			if(mCacheCompressionType == CacheCompressionType.NONE) {
				mOutStream.write(buf, offset, length);
				mCompressedLength += length;

			} else if(mCacheCompressionType == CacheCompressionType.ZSTD) {

				final long maxDestSize = Zstd.compressBound(length);

				if(maxDestSize > Integer.MAX_VALUE) {
					throw new IOException("Max output size is greater than MAX_INT");
				}

				final byte[] dst = new byte[(int)maxDestSize];

				final int size = (int)Zstd.compressByteArray(
						dst,
						0,
						dst.length,
						buf,
						offset,
						length,
						3);

				mOutStream.write(dst, 0, size);

				mCompressedLength += size;
			}

			mUncompressedLength += length;
		}

		public void onWriteFinished() throws IOException {

			if(mWriteExternally) {
				mCompressedLength = mTmpFile.length();
				mUncompressedLength = mCompressedLength;

			} else {
				mOutStream.flush();
				mOutStream.close();
			}

			final long cacheFileId = dbManager.newEntry(
					mUrl,
					mUser,
					mFileType,
					mSession,
					mMimetype,
					mCacheCompressionType,
					mCompressedLength,
					mUncompressedLength);

			final File subdir = getSubdirForCacheFile(location, cacheFileId);
			FileUtils.mkdirs(subdir);

			final File dstFile = new File(subdir, cacheFileId + ext);
			FileUtils.moveFile(mTmpFile, dstFile);

			dbManager.setEntryDone(cacheFileId);

			readableCacheFile = new ReadableCacheFile(cacheFileId, mCacheCompressionType);
		}

		public File writeExternally() throws IOException {
			mWriteExternally = true;
			mOutStream.close();
			return mTmpFile;
		}

		public void onWriteCancelled() {

			try {
				mOutStream.close();
				if(!mTmpFile.delete()) {
					Log.e(TAG, "Failed to delete temp cache file " + mTmpFile.delete());
				}

			} catch(final Exception e) {
				Log.e(TAG, "Exception during cancel", e);
			}
		}
	}

	@NonNull
	private static File getSubdirForCacheFile(
			@NonNull final File cacheRoot,
			final long cacheFileId) {


		return FileUtils.buildPath(
				cacheRoot,
				"rr_cache_files",
				String.format(Locale.US, "%02d", cacheFileId % 100),
				String.format(Locale.US, "%d", (cacheFileId / 100) % 10));
	}

	public class ReadableCacheFile {

		private final long mId;
		@NonNull private final CacheCompressionType mCacheCompressionType;

		@Nullable private Uri mCachedUri;

		private ReadableCacheFile(
				final long id,
				@NonNull final CacheCompressionType cacheCompressionType) {

			mId = id;
			mCacheCompressionType = cacheCompressionType;
		}

		public long getId() {
			return mId;
		}

		@NonNull
		public InputStream getInputStream() throws IOException {

			final InputStream result = getCacheFileInputStream(mId, mCacheCompressionType);

			if(result == null) {
				throw new FileNotFoundException("Stream was null for id " + mId);
			}

			return result;
		}

		@Nullable
		public Uri getUri() {

			if(mCachedUri == null) {
				mCachedUri = getCacheFileUri(mId);
			}

			return mCachedUri;
		}

		@NonNull
		public Optional<File> getFile() {
			return Optional.ofNullable(getExistingCacheFile(mId));
		}

		@NonNull
		public Optional<String> lookupMimetype() {

			final Optional<CacheEntry> result = dbManager.selectById(mId);

			if(result.isPresent()) {
				return Optional.of(result.get().mimetype);

			} else {
				return Optional.empty();
			}
		}

		@Override
		public String toString() {
			return String.format(Locale.US, "[ReadableCacheFile : id %d]", mId);
		}
	}

	@NonNull
	public WritableCacheFile openNewCacheFile(
			@NonNull final URI url,
			@NonNull final RedditAccount user,
			final int fileType,
			final UUID session,
			final String mimetype,
			@NonNull final CacheCompressionType cacheCompressionType) throws IOException {
		return new WritableCacheFile(url, user, fileType, session, mimetype, cacheCompressionType);
	}

	@Nullable
	private File getExistingCacheFile(final long id) {

		final List<File> dirs = getCacheDirs(context);

		// Try new format first
		for(final File dir : dirs) {
			final File f = new File(getSubdirForCacheFile(dir, id), id + ext);
			if(f.exists()) {
				return f;
			}
		}

		for(final File dir : dirs) {
			final File f = new File(dir, id + ext);
			if(f.exists()) {
				return f;
			}
		}

		return null;
	}

	@Nullable
	private SeekableInputStream getCacheFileInputStream(
			final long id,
			@NonNull final CacheCompressionType cacheCompressionType) throws IOException {

		final File cacheFile = getExistingCacheFile(id);

		if(cacheFile == null) {
			return null;
		}

		if(cacheCompressionType == CacheCompressionType.NONE) {
			return new SeekableFileInputStream(cacheFile);

		} else if(cacheCompressionType == CacheCompressionType.ZSTD) {

			try(InputStream is = new ZstdInputStream(new FileInputStream(cacheFile))) {
				return new MemoryDataStream(General.readWholeStream(is)).getInputStream();
			}

		} else {
			throw new RuntimeException("Unhandled compression type " + cacheCompressionType);
		}
	}

	@Nullable
	private Uri getCacheFileUri(final long id) {

		final File cacheFile = getExistingCacheFile(id);

		if(cacheFile == null) {
			return null;
		}

		return Uri.fromFile(cacheFile);
	}

	private class RequestHandlerThread extends Thread {

		public RequestHandlerThread() {
			super("Request Handler Thread");
		}

		@Override
		public void run() {

			try {

				CacheRequest request;
				while((request = requests.take()) != null) {
					handleRequest(request);
				}

			} catch(final InterruptedException e) {
				throw new RuntimeException(e);
			}
		}

		private void handleRequest(final CacheRequest request) {

			if(request.url == null) {
				request.notifyFailure(
						CacheRequest.REQUEST_FAILURE_MALFORMED_URL,
						new NullPointerException("URL was null"),
						null,
						"URL was null",
						Optional.empty());
				return;
			}

			if(request.downloadStrategy.shouldDownloadWithoutCheckingCache()) {
				queueDownload(request);

			} else {

				final List<CacheEntry> result = dbManager.select(
						request.url,
						request.user.username,
						request.requestSession);

				if(result.isEmpty()) {

					if(request.downloadStrategy.shouldDownloadIfNotCached()) {
						queueDownload(request);

					} else {
						request.notifyFailure(
								CacheRequest.REQUEST_FAILURE_CACHE_MISS,
								null,
								null,
								"Could not find this data in the cache",
								Optional.empty());
					}

				} else {

					final CacheEntry entry = mostRecentFromList(result);

					if(request.downloadStrategy.shouldDownloadIfCacheEntryFound(entry)) {
						queueDownload(request);
					} else {
						handleCacheEntryFound(entry, request);
					}
				}
			}
		}

		private CacheEntry mostRecentFromList(final List<CacheEntry> list) {

			CacheEntry entry = null;

			for(final CacheEntry e : list) {
				if(entry == null || entry.timestamp < e.timestamp) {
					entry = e;
				}
			}

			return entry;
		}

		private void queueDownload(final CacheRequest request) {
			request.notifyDownloadNecessary();

			try {
				downloadQueue.add(request, CacheManager.this);
			} catch(final Exception e) {
				request.notifyFailure(
						CacheRequest.REQUEST_FAILURE_MALFORMED_URL,
						e,
						null,
						e.toString(),
						Optional.empty());
			}
		}

		private void handleCacheEntryFound(
				final CacheEntry entry,
				final CacheRequest request) {

			final File cacheFile = getExistingCacheFile(entry.id);

			if(cacheFile == null) {

				request.notifyFailure(
						CacheRequest.REQUEST_FAILURE_STORAGE,
						new RuntimeException(),
						null,
						"A cache entry was found in the database, but"
								+ " the actual data couldn't be found. Press refresh to"
								+ " download the content again.",
						Optional.empty());

				dbManager.delete(entry.id);

				return;
			}

			mDiskCacheThreadPool.add(new PrioritisedCachedThreadPool.Task() {

				@NonNull
				@Override
				public Priority getPriority() {
					return request.priority;
				}

				@Override
				public void run() {

					final GenericFactory<SeekableInputStream, IOException> streamFactory = () -> {
						final SeekableInputStream stream
								= getCacheFileInputStream(entry.id, entry.cacheCompressionType);

						if(stream == null) {
							dbManager.delete(entry.id);
							throw new IOException("Failed to open file");
						}

						return stream;
					};

					request.notifyDataStreamAvailable(
							streamFactory,
							entry.timestamp,
							entry.session,
							true,
							entry.mimetype);

					request.notifyDataStreamComplete(
							streamFactory,
							entry.timestamp,
							entry.session,
							true,
							entry.mimetype);

					request.notifyCacheFileWritten(
							new ReadableCacheFile(entry.id, entry.cacheCompressionType),
							entry.timestamp,
							entry.session,
							true,
							entry.mimetype);
				}
			});
		}
	}
}
