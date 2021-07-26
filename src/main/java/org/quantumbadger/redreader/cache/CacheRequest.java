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
import android.util.Log;
import androidx.annotation.IntDef;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.body.HTTPRequestBody;

import java.io.IOException;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.net.URI;
import java.util.UUID;

public final class CacheRequest implements Comparable<CacheRequest> {

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

	@IntDef({
			DOWNLOAD_QUEUE_REDDIT_API, DOWNLOAD_QUEUE_IMGUR_API, DOWNLOAD_QUEUE_IMMEDIATE,
			DOWNLOAD_QUEUE_IMAGE_PRECACHE})
	@Retention(RetentionPolicy.SOURCE)
	public @interface DownloadQueueType {
	}

	@IntDef({
			REQUEST_FAILURE_CONNECTION,
			REQUEST_FAILURE_REQUEST,
			REQUEST_FAILURE_STORAGE,
			REQUEST_FAILURE_CACHE_MISS,
			REQUEST_FAILURE_CANCELLED,
			REQUEST_FAILURE_MALFORMED_URL,
			REQUEST_FAILURE_PARSE,
			REQUEST_FAILURE_DISK_SPACE,
			REQUEST_FAILURE_REDDIT_REDIRECT,
			REQUEST_FAILURE_PARSE_IMGUR,
			REQUEST_FAILURE_UPLOAD_FAIL_IMGUR,
			REQUEST_FAILURE_CACHE_DIR_DOES_NOT_EXIST})
	@Retention(RetentionPolicy.SOURCE)
	public @interface RequestFailureType {
	}

	public final URI url;
	public final RedditAccount user;
	public final UUID requestSession;

	@NonNull public final Priority priority;

	@NonNull public final DownloadStrategy downloadStrategy;

	public final int fileType;

	public final @DownloadQueueType int queueType;
	@NonNull public final Optional<HTTPRequestBody> requestBody;

	public final boolean cache;

	@Nullable private CacheDownload download;
	private boolean cancelled;

	public final Context context;

	private final CacheRequestCallbacks mCallbacks;

	// Called by CacheDownload
	synchronized boolean setDownload(final CacheDownload download) {
		if(cancelled) {
			return false;
		}
		this.download = download;
		return true;
	}

	// Can be called to cancel the request
	public synchronized void cancel() {

		cancelled = true;

		if(download != null) {
			download.cancel();
			download = null;
		}
	}

	public CacheRequest(
			final URI url,
			final RedditAccount user,
			final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final @DownloadQueueType int queueType,
			final boolean cache,
			final Context context,
			final CacheRequestCallbacks callbacks) {

		this(
				url,
				user,
				requestSession,
				priority,
				downloadStrategy,
				fileType,
				queueType,
				null,
				cache,
				context,
				callbacks);
	}

	public CacheRequest(
			final URI url,
			final RedditAccount user,
			final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final @DownloadQueueType int queueType,
			final Context context,
			final CacheRequestCallbacks callbacks) {

		this(
				url,
				user,
				requestSession,
				priority,
				downloadStrategy,
				fileType,
				queueType,
				true,
				context,
				callbacks);
	}

	public CacheRequest(
			final URI url,
			final RedditAccount user,
			final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final @DownloadQueueType int queueType,
			@Nullable final HTTPRequestBody requestBody,
			final Context context,
			final CacheRequestCallbacks callbacks) {

		this(
				url,
				user,
				requestSession,
				priority,
				downloadStrategy,
				fileType,
				queueType,
				requestBody,
				false,
				context,
				callbacks);
	}

	// TODO remove this huge constructor, make mutable
	private CacheRequest(
			final URI url,
			final RedditAccount user,
			final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final @DownloadQueueType int queueType,
			@Nullable final HTTPRequestBody requestBody,
			final boolean cache,
			final Context context,
			final CacheRequestCallbacks callbacks) {

		this.context = context;
		mCallbacks = callbacks;

		if(user == null) {
			throw new NullPointerException(
					"User was null - set to empty string for anonymous");
		}

		if(!downloadStrategy.shouldDownloadWithoutCheckingCache() && requestBody != null) {
			throw new IllegalArgumentException(
					"Should not perform cache lookup for POST requests");
		}

		this.url = url;
		this.user = user;
		this.requestSession = requestSession;
		this.priority = priority;
		this.downloadStrategy = downloadStrategy;
		this.fileType = fileType;
		this.queueType = queueType;
		this.requestBody = Optional.ofNullable(requestBody);
		this.cache = (requestBody == null) && cache;

		if(url == null) {
			notifyFailure(
					REQUEST_FAILURE_MALFORMED_URL,
					null,
					null,
					"Malformed URL",
					Optional.empty());
			cancel();
		}
	}

	// Queue helpers

	@Override
	public int compareTo(final CacheRequest another) {
		return priority.isHigherPriorityThan(another.priority)
				? -1
				: (another.priority.isHigherPriorityThan(priority) ? 1 : 0);
	}

	// Callbacks

	private void onCallbackException(@NonNull final Throwable t) {
		Log.e("CacheRequest", "Exception thrown from callback", t);
		BugReportActivity.handleGlobalError(context, t);
	}

	public void notifyDataStreamAvailable(
			@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory,
			final long timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		mCallbacks.onDataStreamAvailable(streamFactory, timestamp, session, fromCache, mimetype);
	}

	public void notifyDataStreamComplete(
			@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory,
			final long timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		mCallbacks.onDataStreamComplete(streamFactory, timestamp, session, fromCache, mimetype);
	}

	public void notifyFailure(
			final @RequestFailureType int type,
			final Throwable t,
			final Integer httpStatus,
			final String readableMessage,
			@NonNull final Optional<FailedRequestBody> body) {

		try {
			mCallbacks.onFailure(type, t, httpStatus, readableMessage, body);

		} catch(final Throwable t1) {
			onCallbackException(t1);
		}
	}

	public void notifyProgress(
			final boolean authorizationInProgress,
			final long bytesRead,
			final long totalBytes) {
		try {
			mCallbacks.onProgress(authorizationInProgress, bytesRead, totalBytes);
		} catch(final Throwable t) {
			onCallbackException(t);
		}
	}

	public void notifyCacheFileWritten(
			final CacheManager.ReadableCacheFile cacheFile,
			final long timestamp,
			final UUID session,
			final boolean fromCache,
			final String mimetype) {
		try {
			mCallbacks.onCacheFileWritten(cacheFile, timestamp, session, fromCache, mimetype);
		} catch(final Throwable t) {
			onCallbackException(t);
		}
	}

	public void notifyDownloadNecessary() {
		try {
			mCallbacks.onDownloadNecessary();
		} catch(final Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onDownloadNecessary", t1);

			try {
				onCallbackException(t1);
			} catch(final Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, true, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public void notifyDownloadStarted() {
		try {
			mCallbacks.onDownloadStarted();
		} catch(final Throwable t1) {

			Log.e("CacheRequest", "Exception thrown by onDownloadStarted", t1);

			try {
				onCallbackException(t1);
			} catch(final Throwable t2) {
				Log.e("CacheRequest", "Exception thrown by onCallbackException", t2);
				BugReportActivity.addGlobalError(new RRError(null, null, true, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}
}
