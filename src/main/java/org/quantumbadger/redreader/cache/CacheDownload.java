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

import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.common.datastream.MemoryDataStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.image.RedgifsAPIV2;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public final class CacheDownload extends PrioritisedCachedThreadPool.Task {

	private static final String TAG = "CacheDownload";

	private final CacheRequest mInitiator;
	private final CacheManager manager;
	private final UUID session;

	private volatile boolean mCancelled = false;
	private final AtomicBoolean mFinished = new AtomicBoolean(false);
	private static final AtomicBoolean resetUserCredentials = new AtomicBoolean(false);
	private final HTTPBackend.Request mRequest;

	public CacheDownload(
			final CacheRequest initiator,
			final CacheManager manager) {

		this.mInitiator = initiator;

		this.manager = manager;

		if(!initiator.setDownload(this)) {
			mCancelled = true;
		}

		if(initiator.requestSession != null) {
			session = initiator.requestSession;
		} else {
			session = UUID.randomUUID();
		}

		mRequest = HTTPBackend.getBackend().prepareRequest(
				initiator.context,
				new HTTPBackend.RequestDetails(
						mInitiator.url,
						mInitiator.requestBody.asNullable()));
	}

	// Returns true the first time it is called, so that only one outcome (success,
	// failure, or cancellation) is ever reported to the requester
	private boolean markFinished() {
		return mFinished.compareAndSet(false, true);
	}

	public synchronized void cancel() {

		mCancelled = true;

		if(!markFinished()) {
			return;
		}

		new Thread() {
			@Override
			public void run() {
				mRequest.cancel();
				mInitiator.notifyFailure(General.getGeneralErrorForFailure(
						mInitiator.context,
						CacheRequest.RequestFailureType.CANCELLED,
						null,
						null,
						mInitiator.url,
						Optional.empty()));
			}
		}.start();
	}

	public void doDownload() {

		if(mCancelled) {
			return;
		}

		try {
			performDownload(mRequest);

		} catch(final Throwable t) {
			BugReportActivity.handleGlobalError(mInitiator.context, t);
		}
	}

	// The server-reported Content-Length is used to size the buffer exactly, avoiding
	// the repeated reallocations (and the large discarded intermediate arrays) which
	// growing the buffer from a small initial size would otherwise cause. It is only a
	// hint: the buffer still grows if more data arrives than the server reported.
	//
	// When no length is reported (which is normal for Reddit API responses, as OkHttp
	// decompresses them transparently) the initial size is chosen per file type, from
	// the sizes observed in practice.
	//
	// Comment listings are bimodal: most are a few kilobytes, but a large minority are
	// several hundred kilobytes. They therefore start small, and jump straight to a size
	// which fits almost every large thread on the first reallocation (see
	// chooseGrowthPlan), rather than doubling their way up.
	private static final int DEFAULT_INITIAL_BUFFER_SIZE = 64 * 1024;
	private static final long MAX_PREALLOCATED_BUFFER_SIZE = 256L * 1024 * 1024;

	private static final int COMMENT_LIST_INITIAL_BUFFER_SIZE = 128 * 1024;
	private static final int COMMENT_LIST_SECOND_BUFFER_SIZE = 600 * 1024;

	// Growth plans are shared, never copied: MemoryDataStream only reads them
	private static final int[] NO_GROWTH_PLAN = new int[0];
	private static final int[] COMMENT_LIST_GROWTH_PLAN = {COMMENT_LIST_SECOND_BUFFER_SIZE};

	private static boolean isUsableContentLength(@Nullable final Long reportedContentLength) {
		return reportedContentLength != null
				&& reportedContentLength >= 1
				&& reportedContentLength <= MAX_PREALLOCATED_BUFFER_SIZE;
	}

	public static int chooseInitialBufferSize(
			@Nullable final Long reportedContentLength,
			final int fileType) {

		if(isUsableContentLength(reportedContentLength)) {
			return (int)(long)reportedContentLength;
		}

		switch(fileType) {
			case Constants.FileType.COMMENT_LIST:
				return COMMENT_LIST_INITIAL_BUFFER_SIZE;

			case Constants.FileType.POST_LIST:
			case Constants.FileType.INLINE_IMAGE_PREVIEW:
				return 256 * 1024;

			case Constants.FileType.IMAGE:
				return 2 * 1024 * 1024;

			default:
				return DEFAULT_INITIAL_BUFFER_SIZE;
		}
	}

	/**
	 * The buffer sizes to use on successive reallocations, if the initial buffer turns
	 * out to be too small. See {@link MemoryDataStream#MemoryDataStream(int, int[])}.
	 */
	@NonNull
	public static int[] chooseGrowthPlan(
			@Nullable final Long reportedContentLength,
			final int fileType) {

		if(isUsableContentLength(reportedContentLength)) {
			return NO_GROWTH_PLAN;
		}

		if(fileType == Constants.FileType.COMMENT_LIST) {
			return COMMENT_LIST_GROWTH_PLAN;
		}

		return NO_GROWTH_PLAN;
	}

	public static void resetUserCredentialsOnNextRequest() {
		resetUserCredentials.set(true);
	}

	private void performDownload(final HTTPBackend.Request request) {

		if(mInitiator.queueType == CacheRequest.DownloadQueueType.REDDIT_API) {

			if(resetUserCredentials.getAndSet(false)) {
				mInitiator.user.setAccessToken(null);
			}

			RedditOAuth.AccessToken accessToken
					= mInitiator.user.getMostRecentAccessToken();

			if(accessToken == null || accessToken.isExpired()) {

				mInitiator.notifyProgress(true, 0, 0);

				final RedditOAuth.FetchAccessTokenResult result;

				if(mInitiator.user.isAnonymous()) {
					result = RedditOAuth.fetchAnonymousAccessTokenSynchronous(mInitiator.context);

				} else {
					result = RedditOAuth.fetchAccessTokenSynchronous(
							mInitiator.context,
							mInitiator.user);
				}

				if(result.status != RedditOAuth.FetchAccessTokenResultStatus.SUCCESS) {
					if(markFinished()) {
						mInitiator.notifyFailure(result.error);
					}
					return;
				}

				accessToken = result.accessToken;
				mInitiator.user.setAccessToken(accessToken);
			}

			request.addHeader("Authorization", "bearer " + accessToken.token);
		}

		if(mInitiator.queueType == CacheRequest.DownloadQueueType.IMGUR_API) {
			request.addHeader("Authorization", "Client-ID c3713d9e7674477");

		} else if(mInitiator.queueType == CacheRequest.DownloadQueueType.REDGIFS_API_V2) {
			request.addHeader("Authorization", "Bearer " + RedgifsAPIV2.getLatestToken());
		}

		mInitiator.notifyDownloadStarted();

		request.executeInThisThread(new HTTPBackend.Listener() {
			@Override
			public void onError(
					@NonNull final CacheRequest.RequestFailureType failureType,
					final Throwable exception,
					final Integer httpStatus,
					@Nullable final FailedRequestBody body) {
				if(mInitiator.queueType == CacheRequest.DownloadQueueType.REDDIT_API
						&& TorCommon.isTorEnabled()) {
					HTTPBackend.getBackend().recreateHttpBackend();
					resetUserCredentialsOnNextRequest();
				}

				if(!markFinished()) {
					return;
				}

				mInitiator.notifyFailure(General.getGeneralErrorForFailure(
						mInitiator.context,
						failureType,
						exception,
						httpStatus,
						mInitiator.url,
						Optional.ofNullable(body)));
			}

			@Override
			public void onSuccess(
					final String mimetype,
					final Long bodyBytes,
					final InputStream is) {

				if(mCancelled) {
					Log.i(TAG, "Request cancelled at start of onSuccess()");
					General.closeSafely(is);
					return;
				}

				if(mInitiator.precache && mInitiator.cache) {
					downloadToCacheFile(mimetype, bodyBytes, is);
				} else {
					downloadToMemory(mimetype, bodyBytes, is);
				}
			}
		});
	}

	// For precache requests: the data is written straight to the cache file as it arrives,
	// without ever being held in memory. The data stream callbacks are not invoked, since
	// nothing is waiting to read the data -- the request exists only to populate the cache.
	private void downloadToCacheFile(
			@Nullable final String mimetype,
			@Nullable final Long bodyBytes,
			@NonNull final InputStream is) {

		final CacheManager.WritableCacheFile writableCacheFile = openCacheFile(mimetype);

		if(writableCacheFile == null) {
			// Failure already reported
			General.closeSafely(is);
			return;
		}

		long totalBytesRead = 0;

		try {
			final byte[] buf = new byte[64 * 1024];

			int bytesRead;

			while((bytesRead = tryReadFully(is, buf)) > 0) {

				totalBytesRead += bytesRead;

				writableCacheFile.writeChunk(buf, 0, bytesRead);

				if(bodyBytes != null) {
					mInitiator.notifyProgress(false, totalBytesRead, bodyBytes);
				}

				if(mCancelled) {
					Log.i(TAG, "Request cancelled during read loop");
					writableCacheFile.onWriteCancelled();
					return;
				}
			}

			writableCacheFile.onWriteFinished();

			if(markFinished()) {
				mInitiator.notifyCacheFileWritten(
						writableCacheFile.getReadableCacheFile(),
						TimestampUTC.now(),
						session,
						false,
						mimetype);
			}

		} catch(final Throwable t) {

			writableCacheFile.onWriteCancelled();

			// This covers both network and disk errors. The distinction is not worth
			// tracking here, as a failed precache is only ever logged.
			if(markFinished()) {
				mInitiator.notifyFailure(General.getGeneralErrorForFailure(
						mInitiator.context,
						CacheRequest.RequestFailureType.CONNECTION,
						t,
						null,
						mInitiator.url,
						Optional.empty()));
			}

		} finally {
			General.closeSafely(is);
		}
	}

	// For normal requests: the data is read into memory, where the requester can start
	// consuming it while the download is still in progress, and is then written to the
	// cache afterwards.
	private void downloadToMemory(
			@Nullable final String mimetype,
			@Nullable final Long bodyBytes,
			@NonNull final InputStream is) {

		final MemoryDataStream stream = new MemoryDataStream(
				chooseInitialBufferSize(bodyBytes, mInitiator.fileType),
				chooseGrowthPlan(bodyBytes, mInitiator.fileType));

		mInitiator.notifyDataStreamAvailable(
				stream::getInputStream,
				TimestampUTC.now(),
				session,
				false,
				mimetype);

		try {

			final byte[] buf = new byte[64 * 1024];

			int bytesRead;
			long totalBytesRead = 0;

			while((bytesRead = tryReadFully(is, buf)) > 0) {

				totalBytesRead += bytesRead;

				stream.writeBytes(buf, 0, bytesRead);

				if(bodyBytes != null) {
					mInitiator.notifyProgress(
							false,
							totalBytesRead,
							bodyBytes);
				}

				if(mCancelled) {
					Log.i(TAG, "Request cancelled during read loop");
					stream.setFailed(new IOException("Download cancelled"));
					return;
				}
			}

			stream.setComplete();

			if(!markFinished()) {
				return;
			}

			mInitiator.notifyDataStreamComplete(
					stream::getInputStream,
					TimestampUTC.now(),
					session,
					false,
					mimetype);

		} catch(final Throwable t) {

			stream.setFailed(t instanceof IOException
					? (IOException)t
					: new IOException("Got exception during download", t));

			if(markFinished()) {
				mInitiator.notifyFailure(General.getGeneralErrorForFailure(
						mInitiator.context,
						CacheRequest.RequestFailureType.CONNECTION,
						t,
						null,
						mInitiator.url,
						Optional.empty()));
			}

			return;

		} finally {
			General.closeSafely(is);
		}

		// Save it to the cache

		if(mInitiator.cache) {

			final CacheManager.WritableCacheFile writableCacheFile = openCacheFile(mimetype);

			if(writableCacheFile == null) {
				// Failure already reported
				return;
			}

			try {
				stream.getUnderlyingByteArrayWhenComplete(
						writableCacheFile::writeWholeFile);

				writableCacheFile.onWriteFinished();

				mInitiator.notifyCacheFileWritten(
						writableCacheFile.getReadableCacheFile(),
						TimestampUTC.now(),
						session,
						false,
						mimetype);

			} catch(final IOException e) {

				writableCacheFile.onWriteCancelled();

				mInitiator.notifyFailure(General.getGeneralErrorForFailure(
						mInitiator.context,
						CacheRequest.RequestFailureType.STORAGE,
						e,
						null,
						mInitiator.url,
						Optional.empty()));
			}
		}
	}

	@NonNull
	private static CacheCompressionType getCacheCompressionType(final int fileType) {

		switch(fileType) {
			case Constants.FileType.CAPTCHA:
			case Constants.FileType.IMAGE:
			case Constants.FileType.INLINE_IMAGE_PREVIEW:
			case Constants.FileType.NOCACHE:
			case Constants.FileType.THUMBNAIL:
				// Image saving/sharing relies the file on disk being "raw"
				return CacheCompressionType.NONE;

			case Constants.FileType.COMMENT_LIST:
			case Constants.FileType.IMAGE_INFO:
			case Constants.FileType.INBOX_LIST:
			case Constants.FileType.MULTIREDDIT_LIST:
			case Constants.FileType.POST_LIST:
			case Constants.FileType.SUBREDDIT_ABOUT:
			case Constants.FileType.SUBREDDIT_LIST:
			case Constants.FileType.USER_ABOUT:
				return CacheCompressionType.ZSTD;

			default:
				Log.e(TAG, "Unhandled filetype: " + fileType);
				return CacheCompressionType.NONE;
		}
	}

	// Opens a new cache file for this request. On failure, the requester is notified and
	// null is returned.
	@Nullable
	private CacheManager.WritableCacheFile openCacheFile(@Nullable final String mimetype) {

		try {
			return manager.openNewCacheFile(
					mInitiator.url,
					mInitiator.user,
					mInitiator.fileType,
					session,
					mimetype,
					getCacheCompressionType(mInitiator.fileType));

		} catch(final IOException e) {

			Log.e(TAG, "Exception opening cache file for write", e);

			final CacheRequest.RequestFailureType failureType;

			if(manager.getPreferredCacheLocation().exists()) {
				failureType = CacheRequest.RequestFailureType.STORAGE;
			} else {
				failureType = CacheRequest
						.RequestFailureType.CACHE_DIR_DOES_NOT_EXIST;
			}

			mInitiator.notifyFailure(General.getGeneralErrorForFailure(
					mInitiator.context,
					failureType,
					e,
					null,
					mInitiator.url,
					Optional.empty()));

			return null;
		}
	}

	@NonNull
	@Override
	public Priority getPriority() {
		return mInitiator.priority;
	}

	@Override
	public void run() {
		doDownload();
	}

	private static int tryReadFully(
			final InputStream src,
			final byte[] dst
	) throws IOException {
		int totalBytesRead = 0;

		while(true) {
			final int bytesRead = src.read(dst, totalBytesRead, dst.length - totalBytesRead);

			if (bytesRead <= 0) {
				return totalBytesRead;
			}

			totalBytesRead += bytesRead;

			if (totalBytesRead >= dst.length) {
				return totalBytesRead;
			}
		}
	}
}
