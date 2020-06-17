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

import android.content.Context;
import androidx.annotation.IntDef;
import androidx.annotation.NonNull;
import android.util.Log;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.net.URI;
import java.util.List;
import java.util.UUID;

public abstract class CacheRequest implements Comparable<CacheRequest> {

	public static final int DOWNLOAD_QUEUE_REDDIT_API = 0;
	public static final int DOWNLOAD_QUEUE_IMGUR_API = 1;
	public static final int DOWNLOAD_QUEUE_IMMEDIATE = 2;
	public static final int DOWNLOAD_QUEUE_IMAGE_PRECACHE = 3;

	public static final int REQUEST_FAILURE_CONNECTION = 0;
	public static final int REQUEST_FAILURE_REQUEST = 1;
	public static final int REQUEST_FAILURE_STORAGE = 2;
	public static final int REQUEST_FAILURE_CACHE_MISS = 3;
	public static final int REQUEST_FAILURE_CANCELLED = 4;
	public static final int REQUEST_FAILURE_MALFORMED_URL = 5;
	public static final int REQUEST_FAILURE_PARSE = 6;
	public static final int REQUEST_FAILURE_DISK_SPACE = 7;
	public static final int REQUEST_FAILURE_REDDIT_REDIRECT = 8;
	public static final int REQUEST_FAILURE_PARSE_IMGUR = 9;
	public static final int REQUEST_FAILURE_UPLOAD_FAIL_IMGUR = 10;
	public static final int REQUEST_FAILURE_CACHE_DIR_DOES_NOT_EXIST = 11;

	@IntDef({DOWNLOAD_QUEUE_REDDIT_API, DOWNLOAD_QUEUE_IMGUR_API, DOWNLOAD_QUEUE_IMMEDIATE,
		DOWNLOAD_QUEUE_IMAGE_PRECACHE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface DownloadQueueType {}

	@IntDef({REQUEST_FAILURE_CONNECTION, REQUEST_FAILURE_REQUEST, REQUEST_FAILURE_STORAGE,
		REQUEST_FAILURE_CACHE_MISS, REQUEST_FAILURE_CANCELLED, REQUEST_FAILURE_MALFORMED_URL,
		REQUEST_FAILURE_PARSE, REQUEST_FAILURE_DISK_SPACE, REQUEST_FAILURE_REDDIT_REDIRECT,
		REQUEST_FAILURE_PARSE_IMGUR, REQUEST_FAILURE_UPLOAD_FAIL_IMGUR, REQUEST_FAILURE_CACHE_DIR_DOES_NOT_EXIST})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RequestFailureType {}

	private static final PrioritisedCachedThreadPool JSON_NOTIFY_THREADS = new PrioritisedCachedThreadPool(2, "JSON notify");

	public final URI url;
	public final RedditAccount user;
	public final UUID requestSession;

	public final int priority;
	public final int listId;

	@NonNull public final DownloadStrategy downloadStrategy;

	public final int fileType;

	public final @DownloadQueueType int queueType;
	public final boolean isJson;
	public final List<HTTPBackend.PostField> postFields;

	public final boolean cache;

	private CacheDownload download;
	private boolean cancelled;

	public final Context context;

	// Called by CacheDownload
	synchronized boolean setDownload(final CacheDownload download) {
		if (cancelled) return false;
		this.download = download;
		return true;
	}

	// Can be called to cancel the request
	public synchronized void cancel() {

		cancelled = true;

		if (download != null) {
			download.cancel();
			download = null;
		}
	}

	protected CacheRequest(final URI url, final RedditAccount user, final UUID requestSession, final int priority,
						   final int listId, @NonNull final DownloadStrategy downloadStrategy, final int fileType,
						   final @DownloadQueueType int queueType, final boolean isJson, final boolean cancelExisting,
						   final Context context) {

		this(url, user, requestSession, priority, listId, downloadStrategy, fileType, queueType, isJson, null,
			true, cancelExisting, context);
	}

	// TODO remove this huge constructor, make mutable
	protected CacheRequest(final URI url, final RedditAccount user, final UUID requestSession, final int priority,
						   final int listId, @NonNull final DownloadStrategy downloadStrategy, final int fileType,
						   final @DownloadQueueType int queueType, final boolean isJson, final List<HTTPBackend.PostField> postFields,
						   final boolean cache, final boolean cancelExisting, final Context context) {

		this.context = context;

		if (user == null)
			throw new NullPointerException("User was null - set to empty string for anonymous");

		if (!downloadStrategy.shouldDownloadWithoutCheckingCache() && postFields != null)
			throw new IllegalArgumentException("Should not perform cache lookup for POST requests");

		if (!isJson && postFields != null)
			throw new IllegalArgumentException("POST requests must be for JSON values");

		if (cache && postFields != null)
			throw new IllegalArgumentException("Cannot cache a POST request");

		if (!cache && !isJson)
			throw new IllegalArgumentException("Must cache non-JSON requests");

		this.url = url;
		this.user = user;
		this.requestSession = requestSession;
		this.priority = priority;
		this.listId = listId;
		this.downloadStrategy = downloadStrategy;
		this.fileType = fileType;
		this.queueType = queueType;
		this.isJson = isJson;
		this.postFields = postFields;
		this.cache = cache;

		if (url == null) {
			notifyFailure(REQUEST_FAILURE_MALFORMED_URL, null, null, "Malformed URL");
			cancel();
		}
	}

	// Queue helpers

	public final boolean isHigherPriorityThan(final CacheRequest another) {

		if (priority != another.priority) {
			return priority < another.priority;
		} else {
			return listId < another.listId;
		}
	}

	public int compareTo(final CacheRequest another) {
		return isHigherPriorityThan(another) ? -1 : (another.isHigherPriorityThan(this) ? 1 : 0);
	}

	// Callbacks

	protected abstract void onCallbackException(Throwable t);

	protected abstract void onDownloadNecessary();

	protected abstract void onDownloadStarted();

	protected abstract void onFailure(@RequestFailureType int type, Throwable t, Integer httpStatus, String readableMessage);

	protected abstract void onProgress(boolean authorizationInProgress, long bytesRead, long totalBytes);

	protected abstract void onSuccess(CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, String mimetype);

	public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {
		throw new RuntimeException("CacheRequest method has not been overridden");
	}

	public final void notifyFailure(final @RequestFailureType int type, final Throwable t, final Integer httpStatus, final String readableMessage) {
		try {
			onFailure(type, t, httpStatus, readableMessage);
		} catch (Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onFailure", t1);

			try {
				onCallbackException(t1);
			} catch (Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {
		try {
			onProgress(authorizationInProgress, bytesRead, totalBytes);
		} catch (Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onProgress", t1);

			try {
				onCallbackException(t1);
			} catch (Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifySuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
		try {
			onSuccess(cacheFile, timestamp, session, fromCache, mimetype);
		} catch (Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onSuccess", t1);

			try {
				onCallbackException(t1);
			} catch (Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

		JSON_NOTIFY_THREADS.add(new PrioritisedCachedThreadPool.Task() {

			@Override
			public int getPrimaryPriority() {
				return priority;
			}

			@Override
			public int getSecondaryPriority() {
				return listId;
			}

			@Override
			public void run() {

				try {
					onJsonParseStarted(result, timestamp, session, fromCache);
				} catch (Throwable t1) {

					Log.e("CacheRequest", "Exception thrown by onJsonParseStarted", t1);

					try {
						onCallbackException(t1);
					} catch (Throwable t2) {
						Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
						BugReportActivity.addGlobalError(new RRError(null, null, t1));
						BugReportActivity.handleGlobalError(context, t2);
					}
				}
			}
		});
	}

	public final void notifyDownloadNecessary() {
		try {
			onDownloadNecessary();
		} catch (Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onDownloadNecessary", t1);

			try {
				onCallbackException(t1);
			} catch (Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyDownloadStarted() {
		try {
			onDownloadStarted();
		} catch (Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onDownloadStarted", t1);

			try {
				onCallbackException(t1);
			} catch (Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}
}
