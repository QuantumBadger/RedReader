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
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.datastream.SeekableFileInputStream;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;

import java.io.File;
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

	private static final String ext = ".rr_cache_data", tempExt = ".rr_cache_data_tmp";

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
		pruneCache(PrefsUtility.pref_cache_maxage(
				context,
				General.getSharedPrefs(context)));
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
				PrefsUtility.pref_cache_location(
						context,
						General.getSharedPrefs(context)));
	}

	@NonNull
	public ReadableCacheFile getExistingCacheFileById(final long cacheId) {
		return new ReadableCacheFile(cacheId);
	}

	public class WritableCacheFile {

		private final OutputStream mOutStream;
		private ReadableCacheFile readableCacheFile = null;
		private final CacheRequest mRequest;
		private final File location;

		@NonNull private final UUID mSession;
		@Nullable private final String mMimetype;

		@NonNull private final File mTmpFile;

		private WritableCacheFile(
				final CacheRequest request,
				@NonNull final UUID session,
				@Nullable final String mimetype) throws IOException {

			mRequest = request;
			mSession = session;
			mMimetype = mimetype;

			location = getPreferredCacheLocation();
			mTmpFile = new File(location, UUID.randomUUID().toString() + tempExt);

			mOutStream = new FileOutputStream(mTmpFile);
		}

		@NonNull
		public ReadableCacheFile getReadableCacheFile() {
			return Objects.requireNonNull(readableCacheFile);
		}

		public void writeChunk(
				@NonNull final byte[] dataReused,
				final int offset,
				final int length) throws IOException {

			mOutStream.write(dataReused, offset, length);
		}

		public void onWriteFinished() throws IOException {

			final long cacheFileId = dbManager.newEntry(
					mRequest.url,
					mRequest.user,
					mRequest.fileType,
					mSession,
					mMimetype);

			mOutStream.flush();
			mOutStream.close();

			final File subdir = getSubdirForCacheFile(location, cacheFileId);

			if(!subdir.mkdirs()) {
				throw new IOException("Failed to create dirs: " + subdir.getAbsolutePath());
			}

			final File dstFile = new File(subdir, cacheFileId + ext);
			FileUtils.moveFile(mTmpFile, dstFile);

			dbManager.setEntryDone(cacheFileId);

			readableCacheFile = new ReadableCacheFile(cacheFileId);
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
				String.format(Locale.US, "%02d", (cacheFileId / 100) % 100));
	}

	public class ReadableCacheFile {

		private final long id;
		@Nullable private Uri mCachedUri;

		private ReadableCacheFile(final long id) {
			this.id = id;
		}

		public long getId() {
			return id;
		}

		@NonNull
		public InputStream getInputStream() throws IOException {

			final InputStream result = getCacheFileInputStream(id);

			if(result == null) {
				throw new FileNotFoundException("Stream was null for id " + id);
			}

			return result;
		}

		@Nullable
		public Uri getUri() {

			if(mCachedUri == null) {
				mCachedUri = getCacheFileUri(id);
			}

			return mCachedUri;
		}

		@NonNull
		public Optional<File> getFile() {
			return Optional.ofNullable(getExistingCacheFile(id));
		}

		@NonNull
		public Optional<String> lookupMimetype() {

			final Optional<CacheEntry> result = dbManager.selectById(id);

			if(result.isPresent()) {
				return Optional.of(result.get().mimetype);

			} else {
				return Optional.empty();
			}
		}

		@Override
		public String toString() {
			return String.format(Locale.US, "[ReadableCacheFile : id %d]", id);
		}
	}

	@NonNull
	public WritableCacheFile openNewCacheFile(
			final CacheRequest request,
			final UUID session,
			final String mimetype) throws IOException {
		return new WritableCacheFile(request, session, mimetype);
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
	private SeekableFileInputStream getCacheFileInputStream(final long id) throws IOException {

		final File cacheFile = getExistingCacheFile(id);

		if(cacheFile == null) {
			return null;
		}

		return new SeekableFileInputStream(cacheFile);
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
						"URL was null");
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
								"Could not find this data in the cache");
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
						e.toString());
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
								+ " download the content again.");

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
						final SeekableFileInputStream stream
								= getCacheFileInputStream(entry.id);

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
							new ReadableCacheFile(entry.id),
							entry.timestamp,
							entry.session,
							true,
							entry.mimetype);
				}
			});
		}
	}
}
