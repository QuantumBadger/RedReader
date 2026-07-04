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

package org.quantumbadger.redreader.common;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Rect;
import android.util.Log;
import android.view.View;
import android.view.LayoutInflater;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.RRAnimationShrinkHeight;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

public class ImagePreviewUtils {

	private static final String TAG = "ImagePreviewUtils";
	private static final String PROMPT_PREF_KEY = "inline_image_prompt_accepted";
	private static final AtomicInteger sInlinePreviewsShownThisSession = new AtomicInteger(0);

	public interface ImagePreviewListener {
		void setImageBitmap(Bitmap bitmap);
		void setLoadingSpinnerVisible(boolean visible);
		void setOuterViewVisible(boolean visible);
		void setPlayOverlayVisible(boolean visible);
		void setPreviewDimensions(String ratio);
		LinearLayout getFooterView();
		BaseActivity getActivity();
		boolean isUsageIdValid(int usageId);
		void addErrorView(ErrorView errorView);
		void setErrorViewLayout(View errorView);
	}

	public static void showPrefPrompt(final ImagePreviewListener listener) {
		final BaseActivity activity = listener.getActivity();
		final SharedPrefsWrapper sharedPrefs = General.getSharedPrefs(activity);

		LayoutInflater.from(activity).inflate(
				R.layout.inline_images_question_view,
				listener.getFooterView(),
				true);

		final FrameLayout promptView = listener.getFooterView()
				.findViewById(R.id.inline_images_prompt_root);
		final Button keepShowing = listener.getFooterView()
				.findViewById(R.id.inline_preview_prompt_keep_showing_button);
		final Button turnOff = listener.getFooterView()
				.findViewById(R.id.inline_preview_prompt_turn_off_button);

		keepShowing.setOnClickListener(v -> {
			new RRAnimationShrinkHeight(promptView).start();
			sharedPrefs.edit()
					.putBoolean(PROMPT_PREF_KEY, true)
					.apply();
		});

		turnOff.setOnClickListener(v -> {
			final String prefPreview = activity.getApplicationContext()
					.getString(R.string.pref_images_inline_image_previews_key);
			sharedPrefs.edit()
					.putBoolean(PROMPT_PREF_KEY, true)
					.putString(prefPreview, "never")
					.apply();
		});
	}

	public static void downloadInlinePreview(
			@NonNull final BaseActivity activity,
			@NonNull final RedditPreparedPost post,
			@NonNull final ImagePreviewListener listener,
			final int usageId) {

		final Rect windowVisibleDisplayFrame = DisplayUtils.getWindowVisibleDisplayFrame(activity);

		final int screenWidth = Math.min(1080, Math.max(720, windowVisibleDisplayFrame.width()));
		final int screenHeight = Math.min(2000, Math.max(400, windowVisibleDisplayFrame.height()));

		final RedditParsedPost.ImagePreviewDetails preview = post.src.getPreview(screenWidth, 0);

		if(preview == null || preview.width < 10 || preview.height < 10) {
			listener.setOuterViewVisible(false);
			listener.setLoadingSpinnerVisible(false);
			return;
		}

		final int boundedImageHeight = Math.min(
				(screenHeight * 2) / 3,
				(int)(((long)preview.height * screenWidth) / preview.width));

		listener.setPreviewDimensions(screenWidth + ":" + boundedImageHeight);
		listener.setOuterViewVisible(true);
		listener.setLoadingSpinnerVisible(true);

		CacheManager.getInstance(activity).makeRequest(new CacheRequest(
				preview.url,
				RedditAccountManager.getAnon(),
				null,
				new Priority(Constants.Priority.INLINE_IMAGE_PREVIEW),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.INLINE_IMAGE_PREVIEW,
				CacheRequest.DownloadQueueType.IMMEDIATE,
				activity,
				new CacheRequestCallbacks() {
					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException> stream,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						if(!listener.isUsageIdValid(usageId)) {
							return;
						}

						try(InputStream is = stream.create()) {
							final Bitmap data = BitmapFactory.decodeStream(is);

							if(data == null) {
								throw new IOException("Failed to decode bitmap");
							}

							// Avoid a crash on badly behaving Android ROMs (where the ImageView
							// crashes if an image is too big)
							// Should never happen as we limit the preview size to 3000x3000
							if(data.getByteCount() > 50 * 1024 * 1024) {
								throw new RuntimeException("Image was too large: "
										+ data.getByteCount()
										+ ", preview URL was "
										+ preview.url
										+ " and post was "
										+ post.src.getIdAndType());
							}

							final boolean alreadyAcceptedPrompt = General.getSharedPrefs(activity)
									.getBoolean(PROMPT_PREF_KEY, false);

							final int totalPreviewsShown
									= sInlinePreviewsShownThisSession.incrementAndGet();

							final boolean isVideoPreview = post.isVideoPreview();

							AndroidCommon.runOnUiThread(() -> {
								listener.setImageBitmap(data);
								listener.setLoadingSpinnerVisible(false);

								if(isVideoPreview) {
									listener.setPlayOverlayVisible(true);
								}

								// Show every 8 previews, starting at the second one
								if(totalPreviewsShown % 8 == 2 && !alreadyAcceptedPrompt) {
									showPrefPrompt(listener);
								}
							});

						} catch(final Throwable t) {
							onFailure(General.getGeneralErrorForFailure(
									activity,
									CacheRequest.RequestFailureType.CONNECTION,
									t,
									null,
									preview.url,
									Optional.empty()));
						}
					}

					@Override
					public void onFailure(@NonNull final RRError error) {
						Log.e(TAG, "Failed to download image preview: " + error, error.t);

						if(!listener.isUsageIdValid(usageId)) {
							return;
						}

						AndroidCommon.runOnUiThread(() -> {
							listener.setLoadingSpinnerVisible(false);
							listener.setOuterViewVisible(false);

							final ErrorView errorView = new ErrorView(
									activity,
									error);

							listener.addErrorView(errorView);
							listener.setErrorViewLayout(errorView);
						});
					}
				}
		));
	}
}
