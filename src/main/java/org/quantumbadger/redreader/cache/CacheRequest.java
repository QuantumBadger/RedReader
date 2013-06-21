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
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.CookieStore;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.net.URI;
import java.util.List;
import java.util.UUID;

public abstract class CacheRequest implements Comparable<CacheRequest> {

	public final URI url;
	public final RedditAccount user;
	public final UUID requestSession;

	public final int priority;
	private final int listId;

	public final DownloadType downloadType;

	public final int fileType;

	public final boolean isRedditApi;
	public final boolean isJson;
	public final List<NameValuePair> postFields;

	public final boolean cache;

	private CacheDownload download;
	private boolean cancelled;

	public final boolean cancelExisting;
	public final Context context;

	public enum DownloadType {
		NEVER, IF_NECESSARY, FORCE
	}

	// Called by CacheDownload
	synchronized boolean setDownload(final CacheDownload download) {
		if(cancelled) return false;
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

	protected CacheRequest(final URI url, final RedditAccount user, final UUID requestSession, final int priority,
						   final int listId, final DownloadType downloadType, final int fileType,
						   final boolean isRedditApi, final boolean isJson, final boolean cancelExisting,
						   final Context context) {

		this(url, user, requestSession, priority, listId, downloadType, fileType, isRedditApi, isJson, null,
				true, cancelExisting, context);
	}

	// TODO remove this huge constructor, make mutable
	protected CacheRequest(final URI url, final RedditAccount user, final UUID requestSession, final int priority,
						   final int listId, final DownloadType downloadType, final int fileType,
						   final boolean isRedditApi, final boolean isJson, final List<NameValuePair> postFields,
						   final boolean cache, final boolean cancelExisting, final Context context) {

		this.context = context;

		if(user == null) throw new NullPointerException("User was null - set to empty string for anonymous");
		if(downloadType == null) throw new NullPointerException("Download type was null");

		if(downloadType != DownloadType.FORCE && postFields != null)
			throw new IllegalArgumentException("Download type must be forced for POST requests");

		if(!isJson && postFields != null)
			throw new IllegalArgumentException("POST requests must be for JSON values");

		if(cache && postFields != null)
			throw new IllegalArgumentException("Cannot cache a POST request");

		if(!cache && !isJson)
			throw new IllegalArgumentException("Must cache non-JSON requests");

		this.url = url;
		this.user = user;
		this.requestSession = requestSession;
		this.priority = priority;
		this.listId = listId;
		this.downloadType = downloadType;
		this.fileType = fileType;
		this.isRedditApi = isRedditApi;
		this.isJson = isJson;
		this.postFields = postFields;
		this.cache = cache;
		this.cancelExisting = cancelExisting;

		if(url == null) {
			notifyFailure(RequestFailureType.MALFORMED_URL, null, null, "Malformed URL");
			cancel();
		}
	}

	public CookieStore getCookies() {
		return user.getCookies();
	}

	// Queue helpers

	public final boolean isHigherPriorityThan(final CacheRequest another) {

		if(priority != another.priority) {
			return priority < another.priority;
		} else {
			return listId < another.listId;
		}
	}

	public int compareTo(final CacheRequest another) {
		return isHigherPriorityThan(another) ? -1 : (another.isHigherPriorityThan(this) ? 1 : 0);
	}

	public final RequestIdentifier createIdentifier() {
		return new RequestIdentifier(url, user, requestSession, cache);
	}

	// Callbacks

	protected abstract void onCallbackException(Throwable t);

	protected abstract void onDownloadNecessary();
	protected abstract void onDownloadStarted();

	protected abstract void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage);
	protected abstract void onProgress(long bytesRead, long totalBytes);
	protected abstract void onSuccess(CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, String mimetype);

	public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {
		throw new RuntimeException("CacheRequest method has not been overridden");
	}

	public final void notifyFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
		try {
			onFailure(type, t, status, readableMessage);
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyProgress(final long bytesRead, final long totalBytes) {
		try {
			onProgress(bytesRead, totalBytes);
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifySuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
		try {
			onSuccess(cacheFile, timestamp, session, fromCache, mimetype);
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

		new Thread() {
			@Override
			public void run() {
				android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

				try {
					onJsonParseStarted(result, timestamp, session, fromCache);
				} catch(Throwable t1) {
					try {
						onCallbackException(t1);
					} catch(Throwable t2) {
						BugReportActivity.addGlobalError(new RRError(null, null, t1));
						BugReportActivity.handleGlobalError(context, t2);
					}
				}
			}
		}.start();
	}

	public final void notifyDownloadNecessary() {
		try {
			onDownloadNecessary();
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyDownloadStarted() {
		try {
			onDownloadStarted();
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}
}