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

import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

public final class CacheDownload extends PrioritisedCachedThreadPool.Task {

	private final CacheRequest mInitiator;
	private final CacheManager manager;
	private final UUID session;

	private volatile boolean mCancelled = false;
	private static boolean resetUserCredentials = false;
	private final HTTPBackend.Request mRequest;


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

		mRequest = OKHTTPBackend.getHttpBackend().prepareRequest(
				initiator.context,
				new HTTPBackend.RequestDetails(mInitiator.url, mInitiator.postFields));
	}

	public synchronized void cancel() {

		mCancelled = true;

		new Thread() {
			public void run() {
				mRequest.cancel();
				mInitiator.notifyFailure(RequestFailureType.CANCELLED, null, null, "Cancelled");
			}
		}.start();
	}

	public void doDownload() {

		if(mCancelled) {
			return;
		}

		try {
			performDownload(mRequest);

		} catch(Throwable t) {
			BugReportActivity.handleGlobalError(mInitiator.context, t);
		}
	}

	public static void resetUserCredentialsOnNextRequest() {
		resetUserCredentials = true;
	}

	private void performDownload(final HTTPBackend.Request request) {

		if(mInitiator.queueType == CacheRequest.DownloadQueueType.REDDIT_API) {

			RedditOAuth.AccessToken accessToken = mInitiator.user.getMostRecentAccessToken();

			if(resetUserCredentials || accessToken == null || accessToken.isExpired()) {
				if(resetUserCredentials) {
					mInitiator.user.setAccessToken(null);
					resetUserCredentials = false;
				}

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

			request.addHeader("Authorization", "bearer " + accessToken.token);

		}

		if(mInitiator.queueType == CacheRequest.DownloadQueueType.IMGUR_API) {
			request.addHeader("Authorization", "Client-ID c3713d9e7674477");
		}

		mInitiator.notifyDownloadStarted();

		request.executeInThisThread(new HTTPBackend.Listener() {
			@Override
			public void onError(final RequestFailureType failureType, final Throwable exception, final Integer httpStatus) {
				if(mInitiator.queueType == CacheRequest.DownloadQueueType.REDDIT_API && BaseActivity.getTorStatus()) {
					OKHTTPBackend.recreateHttpBackend();
					resetUserCredentialsOnNextRequest();
				}
				mInitiator.notifyFailure(failureType, exception, httpStatus, "");
			}

			@Override
			public void onSuccess(final String mimetype, final Long bodyBytes, final InputStream is) {

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

				if(mInitiator.isJson) {

					final InputStream bis;

					if(mInitiator.cache) {

						bis = new BufferedInputStream(new CachingInputStream(is, cacheOs, new CachingInputStream.BytesReadListener() {
							public void onBytesRead(final long total) {
								if(bodyBytes != null) {
									mInitiator.notifyProgress(false, total, bodyBytes);
								}
							}
						}), 64 * 1024);

					} else {
						bis = new BufferedInputStream(is, 64 * 1024);
					}

					final JsonValue value;

					try {
						value = new JsonValue(bis);
						mInitiator.notifyJsonParseStarted(value, RRTime.utcCurrentTimeMillis(), session, false);
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
						final byte[] buf = new byte[64 * 1024];

						int bytesRead;
						long totalBytesRead = 0;
						while((bytesRead = is.read(buf)) > 0) {
							totalBytesRead += bytesRead;
							cacheOs.write(buf, 0, bytesRead);
							if(bodyBytes != null) {
								mInitiator.notifyProgress(false, totalBytesRead, bodyBytes);
							}
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
		});
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
