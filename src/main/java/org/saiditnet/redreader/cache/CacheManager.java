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

package org.saiditnet.redreader.cache;

import android.content.Context;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Build;
import android.preference.PreferenceManager;
import android.util.Log;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.PrioritisedCachedThreadPool;
import org.saiditnet.redreader.jsonwrap.JsonValue;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

// TODO consider moving to service
public final class CacheManager {

	private static final String ext = ".rr_cache_data", tempExt = ".rr_cache_data_tmp";

	private static final AtomicBoolean isAlreadyInitialized = new AtomicBoolean(false);
	private final CacheDbManager dbManager;

	private final PriorityBlockingQueue<CacheRequest> requests = new PriorityBlockingQueue<>();

	private final PrioritisedDownloadQueue downloadQueue;
	private final PrioritisedCachedThreadPool mDiskCacheThreadPool = new PrioritisedCachedThreadPool(2, "Disk Cache");

	private final Context context;

	private static CacheManager singleton;

	public static synchronized CacheManager getInstance(final Context context) {
		if(singleton == null) singleton = new CacheManager(context.getApplicationContext());
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

	private Long isCacheFile(final String file) {

		if(!file.endsWith(ext)) return null;

		final String[] fileSplit = file.split("\\.");
		if(fileSplit.length != 2) return null;

		try {
			return Long.parseLong(fileSplit[0]);
		} catch(Exception e) {
			return null;
		}
	}

	private void getCacheFileList(final File dir, final HashSet<Long> currentFiles) {

		final String[] list = dir.list();
		if(list == null) return;

		for(final String file : list) {

			final Long cacheFileId = isCacheFile(file);

			if(cacheFileId != null) {
				currentFiles.add(cacheFileId);
			}
		}
	}

	private static void pruneTemp(final File dir) {

		final String[] list = dir.list();
		if(list == null) return;

		for(final String file : list) {

			if(file.endsWith(tempExt)) {
				new File(dir, file).delete();
			}
		}
	}

	public static List<File> getCacheDirs(Context context) {

		final ArrayList<File> dirs = new ArrayList<>();

		dirs.add(context.getCacheDir());

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
			for(final File dir : context.getExternalCacheDirs()) {
				if(dir != null) {
					dirs.add(dir);
				}
			}

		} else {
			final File extDir = context.getExternalCacheDir();
			if (extDir != null) {
				dirs.add(extDir);
			}
		}


		return dirs;
	}

	public void pruneTemp() {
		List<File> dirs = getCacheDirs(context);
		for (File dir : dirs) {
			pruneTemp(dir);
		}
	}

	public synchronized void pruneCache() {

		try {

			final HashSet<Long> currentFiles = new HashSet<>(128);

			List<File> dirs = getCacheDirs(context);
			for (File dir : dirs) {
				getCacheFileList(dir, currentFiles);
			}

			final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
			final HashMap<Integer, Long> maxAge = PrefsUtility.pref_cache_maxage(context, prefs);

			final ArrayList<Long> filesToDelete = dbManager.getFilesToPrune(currentFiles, maxAge, 72);

			Log.i("CacheManager", "Pruning " + filesToDelete.size() + " files");

			for(final long id : filesToDelete) {
				final File file = getExistingCacheFile(id);
				if(file != null) file.delete();
			}

		} catch(Throwable t) {
			BugReportActivity.handleGlobalError(context, t);
		}

	}

	public synchronized void emptyTheWholeCache() {
		dbManager.emptyTheWholeCache();
	}

	public void makeRequest(final CacheRequest request) {
		requests.put(request);
	}

	public LinkedList<CacheEntry> getSessions(URI url, RedditAccount user) {
		return dbManager.select(url, user.username, null);
	}

	public File getPreferredCacheLocation() {
		return new File(
				PrefsUtility.pref_cache_location(context, PreferenceManager.getDefaultSharedPreferences(context)));
	}

	public class WritableCacheFile {

		private final NotifyOutputStream os;
		private long cacheFileId = -1;
		private ReadableCacheFile readableCacheFile = null;
		private final CacheRequest request;
		private final File location;

		private WritableCacheFile(final CacheRequest request, final UUID session, final String mimetype) throws IOException {

			this.request = request;
			location = getPreferredCacheLocation();
			final File tmpFile = new File(location, UUID.randomUUID().toString() + tempExt);
			final FileOutputStream fos = new FileOutputStream(tmpFile);

			final OutputStream bufferedOs = new BufferedOutputStream(fos, 64 * 1024);

			final NotifyOutputStream.Listener listener = new NotifyOutputStream.Listener() {
				public void onClose() throws IOException {

					cacheFileId = dbManager.newEntry(request, session, mimetype);

					final File dstFile = new File(location, cacheFileId + ext);
					General.moveFile(tmpFile, dstFile);

					dbManager.setEntryDone(cacheFileId);

					readableCacheFile = new ReadableCacheFile(cacheFileId);
				}
			};

			this.os = new NotifyOutputStream(bufferedOs, listener);
		}

		public NotifyOutputStream getOutputStream() {
			return os;
		}

		public ReadableCacheFile getReadableCacheFile() throws IOException {

			if(readableCacheFile == null) {

				if(!request.isJson) {
					BugReportActivity.handleGlobalError(context, "Attempt to read cache file before closing");
				}

				try {
					os.flush();
					os.close();
				} catch(IOException e) {
					Log.e("getReadableCacheFile", "Error closing " + cacheFileId);
					throw e;
				}
			}

			return readableCacheFile;
		}
	}

	public class ReadableCacheFile {

		private final long id;

		private ReadableCacheFile(final long id) {
			this.id = id;
		}

		public InputStream getInputStream() throws IOException {
			return getCacheFileInputStream(id);
		}

		public Uri getUri() throws IOException {
			return getCacheFileUri(id);
		}

		@Override
		public String toString() {
			return String.format(Locale.US, "[ReadableCacheFile : id %d]", id);
		}

		public long getSize() {
			return getExistingCacheFile(id).length();
		}
	}

	public WritableCacheFile openNewCacheFile(final CacheRequest request, final UUID session, final String mimetype) throws IOException {
		return new WritableCacheFile(request, session, mimetype);
	}

	private File getExistingCacheFile(final long id) {
		List<File> dirs = getCacheDirs(context);
		for (File dir : dirs) {
			final File f = new File(dir, id + ext);
			if (f.exists())
				return f;
		}
		return null;
	}

	private InputStream getCacheFileInputStream(final long id) throws IOException {

		final File cacheFile = getExistingCacheFile(id);

		if(cacheFile == null) {
			return null;
		}

		return new BufferedInputStream(new FileInputStream(cacheFile), 8 * 1024);
	}

	private Uri getCacheFileUri(final long id) throws IOException {

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

			android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

			try {

				CacheRequest request;
				while((request = requests.take()) != null) {
					handleRequest(request);
				}

			} catch (InterruptedException e) {
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

				final LinkedList<CacheEntry> result = dbManager.select(request.url, request.user.username, request.requestSession);

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

		private CacheEntry mostRecentFromList(final LinkedList<CacheEntry> list) {

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
				request.notifyFailure(CacheRequest.REQUEST_FAILURE_MALFORMED_URL, e, null, e.toString());
			}
		}

		private void handleCacheEntryFound(final CacheEntry entry, final CacheRequest request) {

			final File cacheFile = getExistingCacheFile(entry.id);

			if(cacheFile == null) {

				request.notifyFailure(
						CacheRequest.REQUEST_FAILURE_STORAGE,
						null,
						null,
						"A cache entry was found in the database, but the actual data couldn't be found. Press refresh to download the content again.");

				dbManager.delete(entry.id);

				return;
			}

			mDiskCacheThreadPool.add(new PrioritisedCachedThreadPool.Task() {

				@Override
				public int getPrimaryPriority() {
					return request.priority;
				}

				@Override
				public int getSecondaryPriority() {
					return request.listId;
				}

				@Override
				public void run() {

					if(request.isJson) {

						InputStream cacheFileInputStream = null;

						try {
							cacheFileInputStream = getCacheFileInputStream(entry.id);

							if(cacheFileInputStream == null) {
								request.notifyFailure(CacheRequest.REQUEST_FAILURE_CACHE_MISS, null, null, "Couldn't retrieve cache file");
								return;
							}

							final JsonValue value = new JsonValue(cacheFileInputStream);
							request.notifyJsonParseStarted(value, entry.timestamp, entry.session, true);
							value.buildInThisThread();

						} catch(Throwable t) {

							if(cacheFileInputStream != null) {
								try {
									cacheFileInputStream.close();
								} catch(IOException e) {
									// Ignore
								}
							}

							dbManager.delete(entry.id);

							final File existingCacheFile = getExistingCacheFile(entry.id);
							if(existingCacheFile != null) {
								existingCacheFile.delete();
							}

							request.notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Error parsing the JSON stream");

							return;
						}
					}

					request.notifySuccess(new ReadableCacheFile(entry.id), entry.timestamp, entry.session, true, entry.mimetype);
				}
			});
		}
	}
}
