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

package org.quantumbadger.redreader.reddit.prepared;

import android.graphics.Bitmap;
import android.graphics.Rect;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.UiThread;

import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.DisplayUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.image.ScaledBitmapDecoder;

import java.io.IOException;
import java.util.UUID;

/**
 * Downloads and decodes the inline image preview for a single post.
 *
 * <p>The decoded bitmap is held here, rather than by the view which displays it, so that it
 * survives that view being recycled. The post listing keeps every post within a few
 * positions of the visible area active (see
 * {@link org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter#setPreloadWindow}),
 * which lets the user scroll back and forth over the same posts without any preview having
 * to be downloaded or decoded again.
 *
 * <p>When a post leaves that window, its preview is released and any download still in
 * progress for it is cancelled, which bounds the number of previews held in memory at once.
 *
 * <p>Every method must be called on the UI thread.
 */
public final class InlinePreviewLoader {

	private static final String TAG = "InlinePreviewLoader";

	public enum State {
		NOT_LOADED,
		LOADING,
		LOADED,
		FAILED
	}

	public interface Listener {
		@UiThread
		void onInlinePreviewStateChanged(@NonNull InlinePreviewLoader loader);
	}

	/**
	 * The preview image to show for a post, together with the size of the box it will be
	 * displayed in.
	 */
	public static final class PreviewDetails {

		@NonNull public final UriString url;
		public final int boxWidthPx;
		public final int boxHeightPx;

		private PreviewDetails(
				@NonNull final UriString url,
				final int boxWidthPx,
				final int boxHeightPx) {

			this.url = url;
			this.boxWidthPx = boxWidthPx;
			this.boxHeightPx = boxHeightPx;
		}
	}

	/**
	 * Returns null if there is no inline preview to show for this post.
	 */
	@Nullable
	@UiThread
	public static PreviewDetails calculatePreviewDetails(
			@NonNull final BaseActivity activity,
			@NonNull final RedditPreparedPost post) {

		if(!post.shouldShowInlinePreview()) {
			return null;
		}

		final Rect windowVisibleDisplayFrame
				= DisplayUtils.getWindowVisibleDisplayFrame(activity);

		final int screenWidth
				= Math.min(1080, Math.max(720, windowVisibleDisplayFrame.width()));
		final int screenHeight
				= Math.min(2000, Math.max(400, windowVisibleDisplayFrame.height()));

		final RedditParsedPost.ImagePreviewDetails preview
				= post.src.getPreview(screenWidth, 0);

		if(preview == null || preview.width < 10 || preview.height < 10) {
			return null;
		}

		final int boundedImageHeight = Math.max(1, Math.min(
				(screenHeight * 2) / 3,
				(int)(((long)preview.height * screenWidth) / preview.width)));

		return new PreviewDetails(preview.url, screenWidth, boundedImageHeight);
	}

	@NonNull private final BaseActivity mActivity;
	@NonNull private final RedditPreparedPost mPost;

	@Nullable private Listener mListener;

	private boolean mActive;

	@NonNull private State mState = State.NOT_LOADED;
	@Nullable private Bitmap mBitmap;
	@Nullable private RRError mError;

	@Nullable private CacheRequest mRequest;

	// Incremented whenever the current load is superseded or released, so that results
	// arriving later from a background thread can be discarded
	private int mGeneration;

	// The size of the display box which the current bitmap, or in-flight request, is for
	private int mLoadedForWidthPx;
	private int mLoadedForHeightPx;

	InlinePreviewLoader(
			@NonNull final BaseActivity activity,
			@NonNull final RedditPreparedPost post) {

		mActivity = activity;
		mPost = post;
	}

	@NonNull
	public State getState() {
		return mState;
	}

	@Nullable
	public Bitmap getBitmap() {
		return mBitmap;
	}

	@Nullable
	public RRError getError() {
		return mError;
	}

	/**
	 * Called when the post enters or leaves the preload window.
	 */
	@UiThread
	public void setActive(final boolean active) {
		General.checkThisIsUIThread();
		mActive = active;
		update();
	}

	/**
	 * Attaches the view which is currently displaying this post, which will be notified of
	 * the current state before this method returns, and again whenever it changes. Pass
	 * null when the view is recycled.
	 */
	@UiThread
	public void setListener(@Nullable final Listener listener) {

		General.checkThisIsUIThread();

		mListener = listener;

		update();

		// Deliver the current state, in case the call above didn't change it
		notifyListener();
	}

	private void update() {

		// A preview is held in memory while the post is either near the visible area of the
		// list, or is actually bound to a view. The latter can happen first, during a fast
		// scroll, as the preload window is only recalculated once per frame.
		if(!mActive && mListener == null) {
			release();
			return;
		}

		final PreviewDetails details = calculatePreviewDetails(mActivity, mPost);

		if(details == null) {
			return;
		}

		if(mState != State.NOT_LOADED
				&& mLoadedForWidthPx >= details.boxWidthPx
				&& mLoadedForHeightPx >= details.boxHeightPx) {

			// Already loading, or loaded at a sufficient resolution. A larger box than
			// before means the screen has been rotated, and we need a sharper image.
			return;
		}

		startLoad(details);
	}

	private void release() {

		// Discard any result which is still on its way from a background thread
		mGeneration++;

		if(mRequest != null) {
			mRequest.cancel();
			mRequest = null;
		}

		mBitmap = null;
		mError = null;
		mState = State.NOT_LOADED;
		mLoadedForWidthPx = 0;
		mLoadedForHeightPx = 0;
	}

	private void startLoad(@NonNull final PreviewDetails details) {

		if(mRequest != null) {
			mRequest.cancel();
		}

		final int generation = ++mGeneration;

		mBitmap = null;
		mError = null;
		mState = State.LOADING;
		mLoadedForWidthPx = details.boxWidthPx;
		mLoadedForHeightPx = details.boxHeightPx;

		notifyListener();

		mRequest = new CacheRequest(
				details.url,
				RedditAccountManager.getAnon(),
				null,
				new Priority(Constants.Priority.INLINE_IMAGE_PREVIEW),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.INLINE_IMAGE_PREVIEW,
				CacheRequest.DownloadQueueType.IMMEDIATE,
				mActivity,
				new LoadCallbacks(generation, details));

		CacheManager.getInstance(mActivity).makeRequest(mRequest);
	}

	@UiThread
	private void onLoadSucceeded(final int generation, @NonNull final Bitmap bitmap) {

		if(generation != mGeneration) {
			// Superseded, or released since this load was started
			return;
		}

		mRequest = null;
		mBitmap = bitmap;
		mError = null;
		mState = State.LOADED;

		notifyListener();
	}

	@UiThread
	private void onLoadFailed(final int generation, @NonNull final RRError error) {

		if(generation != mGeneration) {
			return;
		}

		mRequest = null;
		mBitmap = null;
		mError = error;
		mState = State.FAILED;

		notifyListener();
	}

	private void notifyListener() {

		if(mListener != null) {
			mListener.onInlinePreviewStateChanged(this);
		}
	}

	private final class LoadCallbacks implements CacheRequestCallbacks {

		private final int mCallbackGeneration;
		@NonNull private final PreviewDetails mDetails;

		private LoadCallbacks(
				final int generation,
				@NonNull final PreviewDetails details) {

			mCallbackGeneration = generation;
			mDetails = details;
		}

		@Override
		public void onDataStreamComplete(
				@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory,
				final TimestampUTC timestamp,
				@NonNull final UUID session,
				final boolean fromCache,
				@Nullable final String mimetype) {

			final Bitmap bitmap;

			try(SeekableInputStream is = streamFactory.create()) {

				bitmap = ScaledBitmapDecoder.decodeToFitWithin(
						is,
						mDetails.boxWidthPx,
						mDetails.boxHeightPx);

			} catch(final Throwable t) {

				onFailure(General.getGeneralErrorForFailure(
						mActivity,
						CacheRequest.RequestFailureType.CONNECTION,
						t,
						null,
						mDetails.url,
						Optional.empty()));

				return;
			}

			AndroidCommon.runOnUiThread(
					() -> onLoadSucceeded(mCallbackGeneration, bitmap));
		}

		@Override
		public void onFailure(@NonNull final RRError error) {

			Log.e(TAG, "Failed to download image preview: " + error, error.t);

			AndroidCommon.runOnUiThread(() -> onLoadFailed(mCallbackGeneration, error));
		}
	}
}
