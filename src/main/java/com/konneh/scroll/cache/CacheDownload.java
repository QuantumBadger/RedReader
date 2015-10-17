/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.cache;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.RedirectException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HTTP;
import org.apache.http.protocol.HttpContext;
import com.konneh.scroll.activities.BugReportActivity;
import com.konneh.scroll.common.PrioritisedCachedThreadPool;
import com.konneh.scroll.common.RRTime;
import com.konneh.scroll.jsonwrap.JsonValue;
import com.konneh.scroll.reddit.api.RedditOAuth;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.UUID;

public final class CacheDownload extends PrioritisedCachedThreadPool.Task {

	private final CacheRequest mInitiator;
	private final CacheManager manager;
	private final UUID session;

	private volatile boolean mCancelled = false;
	private final HttpRequestBase mHttpRequest;

	private final PrioritisedDownloadQueue mQueue;

	public CacheDownload(final CacheRequest initiator, final CacheManager manager, final PrioritisedDownloadQueue queue) {

		this.mInitiator = initiator;

		this.manager = manager;
		this.mQueue = queue;

		if(!initiator.setDownload(this)) {
			cancel();
		}

		if(initiator.requestSession != null) {
			session = initiator.requestSession;
		} else {
			session = UUID.randomUUID();
		}

		if(mInitiator.postFields != null) {
			final HttpPost httpPost = new HttpPost(mInitiator.url);
			mHttpRequest = httpPost;
			try {
				httpPost.setEntity(new UrlEncodedFormEntity(mInitiator.postFields, HTTP.UTF_8));
			} catch(UnsupportedEncodingException e) {
				BugReportActivity.handleGlobalError(initiator.context, e);
			}

		} else {
			mHttpRequest = new HttpGet(mInitiator.url);
		}
	}

	public synchronized void cancel() {

		mCancelled = true;

		new Thread() {
			public void run() {
				mHttpRequest.abort();
				mInitiator.notifyFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			}
		}.start();
	}

	public void doDownload() {

		if(mCancelled) {
			return;
		}

		try {
			performDownload(mQueue.getHttpClient(), mHttpRequest);

		} catch(Throwable t) {
			BugReportActivity.handleGlobalError(mInitiator.context, t);
		}
	}

	private void performDownload(final HttpClient httpClient, final HttpRequestBase httpRequest) {

		if(mInitiator.isJson) httpRequest.setHeader("Accept-Encoding", "gzip");

		final HttpContext localContext = new BasicHttpContext();
		localContext.setAttribute(ClientContext.COOKIE_STORE, mInitiator.getCookies());

		if(mInitiator.isRedditApi) {

			RedditOAuth.AccessToken accessToken = mInitiator.user.getMostRecentAccessToken();

			if(accessToken == null || accessToken.isExpired()) {

				mInitiator.notifyProgress(true, 0, 0);

				final RedditOAuth.FetchAccessTokenResult result;

				if(mInitiator.user.isAnonymous()) {
					result = RedditOAuth.fetchAnonymousAccessTokenSynchronous(mInitiator.context);

				} else {
					result = RedditOAuth.fetchAccessTokenSynchronous(mInitiator.context, mInitiator.user.refreshToken);
				}

				if(result.status != RedditOAuth.FetchAccessTokenResultStatus.SUCCESS) {
					mInitiator.notifyFailure(RequestFailureType.REQUEST, result.error.t, result.error.httpStatus, result.error.title + ": " + result.error.message);
					return;
				}

				accessToken = result.accessToken;
				mInitiator.user.setAccessToken(accessToken);
			}

			httpRequest.addHeader("Authorization", "bearer " + accessToken.token);

		}

		mInitiator.notifyDownloadStarted();

		final HttpResponse response;
		final StatusLine status;

		try {
			if(mCancelled) {
				mInitiator.notifyFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
				return;
			}
			response = httpClient.execute(httpRequest, localContext);
			status = response.getStatusLine();

		} catch(Throwable t) {

			if(t.getCause() != null
					&& t.getCause() instanceof RedirectException
					&& httpRequest.getURI().getHost().endsWith("reddit.com")) {

				mInitiator.notifyFailure(RequestFailureType.REDDIT_REDIRECT, t, null, "Unable to open a connection");
			} else {
				mInitiator.notifyFailure(RequestFailureType.CONNECTION, t, null, "Unable to open a connection");
			}
			return;
		}

		if(status.getStatusCode() != 200 && status.getStatusCode() != 202) {
			mInitiator.notifyFailure(RequestFailureType.REQUEST, null, status, String.format("HTTP error %d (%s)", status.getStatusCode(), status.getReasonPhrase()));
			return;
		}

		if(mCancelled) {
			mInitiator.notifyFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			return;
		}

		final HttpEntity entity = response.getEntity();

		if(entity == null) {
			mInitiator.notifyFailure(RequestFailureType.CONNECTION, null, status, "Did not receive a valid HTTP response");
			return;
		}

		final InputStream is;

		final String mimetype;
		try {
			is = entity.getContent();
			mimetype = entity.getContentType() == null ? null : entity.getContentType().getValue();
		} catch (Throwable t) {
			t.printStackTrace();
			mInitiator.notifyFailure(RequestFailureType.CONNECTION, t, status, "Could not open an input stream");
			return;
		}

		final NotifyOutputStream cacheOs;
		final CacheManager.WritableCacheFile cacheFile;
		if(mInitiator.cache) {
			try {
				cacheFile = manager.openNewCacheFile(mInitiator, session, mimetype);
				cacheOs = cacheFile.getOutputStream();
			} catch (IOException e) {
				e.printStackTrace();
				mInitiator.notifyFailure(RequestFailureType.STORAGE, e, null, "Could not access the local cache");
				return;
			}
		} else {
			cacheOs = null;
			cacheFile = null;
		}

		final long contentLength = entity.getContentLength();

		if(mInitiator.isJson) {

			final InputStream bis;

			if(mInitiator.cache) {

				bis = new BufferedInputStream(new CachingInputStream(is, cacheOs, new CachingInputStream.BytesReadListener() {
					public void onBytesRead(final long total) {
						mInitiator.notifyProgress(false, total, contentLength);
					}
				}), 8 * 1024);

			} else {
				bis = new BufferedInputStream(is, 8 * 1024);
			}

			final JsonValue value;

			try {
				value = new JsonValue(bis);

				synchronized(this) {
					mInitiator.notifyJsonParseStarted(value, RRTime.utcCurrentTimeMillis(), session, false);
				}

				value.buildInThisThread();

			} catch (Throwable t) {
				t.printStackTrace();
				mInitiator.notifyFailure(RequestFailureType.PARSE, t, null, "Error parsing the JSON stream");
				return;
			}

			if(mInitiator.cache && cacheFile != null) {
				try {
					mInitiator.notifySuccess(cacheFile.getReadableCacheFile(), RRTime.utcCurrentTimeMillis(), session, false, mimetype);
				} catch(IOException e) {
					if(e.getMessage().contains("ENOSPC")) {
						mInitiator.notifyFailure(RequestFailureType.DISK_SPACE, e, null, "Out of disk space");
					} else {
						mInitiator.notifyFailure(RequestFailureType.STORAGE, e, null, "Cache file not found");
					}
				}
			}

		} else {

			if(!mInitiator.cache) {
				BugReportActivity.handleGlobalError(mInitiator.context, "Cache disabled for non-JSON request");
				return;
			}

			try {
				final byte[] buf = new byte[8 * 1024];

				int bytesRead;
				long totalBytesRead = 0;
				while((bytesRead = is.read(buf)) > 0) {
					totalBytesRead += bytesRead;
					cacheOs.write(buf, 0, bytesRead);
					mInitiator.notifyProgress(false, totalBytesRead, contentLength);
				}

				cacheOs.flush();
				cacheOs.close();

				try {
					mInitiator.notifySuccess(cacheFile.getReadableCacheFile(), RRTime.utcCurrentTimeMillis(), session, false, mimetype);
				} catch(IOException e) {
					if(e.getMessage().contains("ENOSPC")) {
						mInitiator.notifyFailure(RequestFailureType.DISK_SPACE, e, null, "Out of disk space");
					} else {
						mInitiator.notifyFailure(RequestFailureType.STORAGE, e, null, "Cache file not found");
					}
				}

			} catch(IOException e) {

				if(e.getMessage() != null && e.getMessage().contains("ENOSPC")) {
					mInitiator.notifyFailure(RequestFailureType.STORAGE, e, null, "Out of disk space");

				} else {
					e.printStackTrace();
					mInitiator.notifyFailure(RequestFailureType.CONNECTION, e, null, "The connection was interrupted");
				}

			} catch(Throwable t) {
				t.printStackTrace();
				mInitiator.notifyFailure(RequestFailureType.CONNECTION, t, null, "The connection was interrupted");
			}
		}
	}

	@Override
	public int getPrimaryPriority() {
		return mInitiator.priority;
	}

	@Override
	public int getSecondaryPriority() {
		return mInitiator.listId;
	}

	@Override
	public void run() {
		doDownload();
	}
}