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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.body.HTTPRequestBody;

import java.io.IOException;
import java.util.UUID;

public final class CacheRequest implements Comparable<CacheRequest> {

	public enum DownloadQueueType {
		REDDIT_API,
		IMGUR_API,
		IMMEDIATE,
		IMAGE_PRECACHE,
		REDGIFS_API_V2
	}

	public enum RequestFailureType {
		CONNECTION,
		REQUEST,
		STORAGE,
		CACHE_MISS,
		CANCELLED,
		MALFORMED_URL,
		PARSE,
		DISK_SPACE,
		REDDIT_REDIRECT,
		PARSE_IMGUR,
		UPLOAD_FAIL_IMGUR,
		CACHE_DIR_DOES_NOT_EXIST
	}

	public final UriString url;
	public final RedditAccount user;
	public final UUID requestSession;

	@NonNull public final Priority priority;

	@NonNull public final DownloadStrategy downloadStrategy;

	public final int fileType;

	public final DownloadQueueType queueType;
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
			@NonNull final UriString url,
			@NonNull final RedditAccount user,
			@Nullable final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final DownloadQueueType queueType,
			final boolean cache,
			@NonNull final Context context,
			@NonNull final CacheRequestCallbacks callbacks) {

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
			@NonNull final UriString url,
			@NonNull final RedditAccount user,
			@Nullable final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final DownloadQueueType queueType,
			@NonNull final Context context,
			@NonNull final CacheRequestCallbacks callbacks) {

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
			@NonNull final UriString url,
			@NonNull final RedditAccount user,
			@Nullable final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final DownloadQueueType queueType,
			@Nullable final HTTPRequestBody requestBody,
			@NonNull final Context context,
			@NonNull final CacheRequestCallbacks callbacks) {

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
			@NonNull final UriString url,
			@NonNull final RedditAccount user,
			@Nullable final UUID requestSession,
			@NonNull final Priority priority,
			@NonNull final DownloadStrategy downloadStrategy,
			final int fileType,
			final DownloadQueueType queueType,
			@Nullable final HTTPRequestBody requestBody,
			final boolean cache,
			@NonNull final Context context,
			@NonNull final CacheRequestCallbacks callbacks) {

		this.context = context.getApplicationContext();
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
			notifyFailure(General.getGeneralErrorForFailure(
					this.context,
					RequestFailureType.MALFORMED_URL,
					null,
					null,
					null,
					Optional.empty()));
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
			final TimestampUTC timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		mCallbacks.onDataStreamAvailable(streamFactory, timestamp, session, fromCache, mimetype);
	}

	public void notifyDataStreamComplete(
			@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory,
			final TimestampUTC timestamp,
			@NonNull final UUID session,
			final boolean fromCache,
			@Nullable final String mimetype) {

		mCallbacks.onDataStreamComplete(streamFactory, timestamp, session, fromCache, mimetype);
	}

	public void notifyFailure(@NonNull final RRError error) {

		try {
			mCallbacks.onFailure(error);

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
			final TimestampUTC timestamp,
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
