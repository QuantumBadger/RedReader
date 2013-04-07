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

package org.quantumbadger.redreader.activities;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.widget.ImageView;
import com.polites.android.GestureImageView;
import org.apache.http.StatusLine;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.image.GifDecoderThread;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

public class ImageViewActivity extends Activity {

	private GifDecoderThread gifThread;
	private URI url;
	private ImageView imageView;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences);

		if(solidblack) getWindow().setBackgroundDrawable(new ColorDrawable(Color.BLACK));

		// TODO add actionbar with refresh/etc
		// TODO make fragment

		// TODO handle loading from saved instance
		url = General.uriFromString(getIntent().getDataString());

		final LoadingView loadingView = new LoadingView(this, R.string.download_loading, true, false);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(loadingView);

		CacheManager.getInstance(this).makeRequest(
				new CacheRequest(
						url,
						RedditAccountManager.getAnon(),
						null,
						Constants.Priority.IMAGE_VIEW,
						0,
						CacheRequest.DownloadType.IF_NECESSARY,
						Constants.FileType.IMAGE,
						false, false, false, this) {
					@Override
					protected void onCallbackException(Throwable t) {
						BugReportActivity.handleGlobalError(getBaseContext(), new RRError(null, null, t));
					}

					@Override
					protected void onDownloadNecessary() {
						loadingView.setIndeterminate(R.string.download_waiting);
					}

					@Override
					protected void onDownloadStarted() {
						loadingView.setIndeterminate(R.string.download_downloading);
					}

					@Override
					protected void onFailure(final RequestFailureType type, Throwable t, StatusLine status, final String readableMessage) {

						loadingView.setDone(R.string.download_failed);
						final RRError error = General.getGeneralErrorForFailure(ImageViewActivity.this, type, t, status);

						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								// TODO handle properly
								layout.addView(new ErrorView(ImageViewActivity.this, error));
							}
						});
					}

					@Override
					protected void onProgress(long bytesRead, long totalBytes) {
						loadingView.setProgress(R.string.download_downloading, (float)((double)bytesRead / (double)totalBytes));
					}

					@Override
					protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, final String mimetype) {

						if(!Constants.Mime.isImage(mimetype)) {
							revertToWeb();
							return;
						}

						if(Constants.Mime.isImageGif(mimetype)) {

							try {

								gifThread = new GifDecoderThread(cacheFile.getInputStream(), new GifDecoderThread.OnGifLoadedListener() {

									public void onGifLoaded() {
										new Handler(Looper.getMainLooper()).post(new Runnable() {
											public void run() {
												imageView = new ImageView(ImageViewActivity.this);
												imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
												setContentView(imageView);
												gifThread.setView(imageView);

												imageView.setOnClickListener(new View.OnClickListener() {
													public void onClick(View v) {
														finish();
													}
												});
											}
										});
									}

									public void onOutOfMemory() {
										General.quickToast(ImageViewActivity.this, R.string.imageview_oom);
										revertToWeb();
									}

									public void onGifInvalid() {
										General.quickToast(ImageViewActivity.this, R.string.imageview_invalid_gif);
										revertToWeb();
									}
								});

								gifThread.start();

							} catch(IOException e) {
								throw new RuntimeException(e);
							}

						} else {

							final Bitmap finalResult;

							try {

								final int maxTextureSize = 2048;
								final Bitmap imageOrig = BitmapFactory.decodeStream(cacheFile.getInputStream());

								if(imageOrig == null) {
									General.quickToast(ImageViewActivity.this, "Couldn't load the image. Trying internal browser.");
									revertToWeb();
									return;
								}

								final int maxDim = Math.max(imageOrig.getWidth(), imageOrig.getHeight());

								if(maxDim > maxTextureSize) {

									imageOrig.recycle();

									final double scaleFactorPowerOfTwo = Math.log((double)maxDim / (double)maxTextureSize) / Math.log(2);
									final int scaleFactor = (int)Math.round(Math.pow(2, Math.ceil(scaleFactorPowerOfTwo)));

									final BitmapFactory.Options options = new BitmapFactory.Options();
									options.inSampleSize = scaleFactor;
									finalResult = BitmapFactory.decodeStream(cacheFile.getInputStream(), null, options);
								} else {
									finalResult = imageOrig;
								}

							} catch(IOException e) {
								throw new RuntimeException(e);

							} catch (OutOfMemoryError e) {
								General.quickToast(ImageViewActivity.this, "Out of memory. Trying internal browser.");
								revertToWeb();
								return;
							}

							new Handler(Looper.getMainLooper()).post(new Runnable() {
								public void run() {
									imageView = new GestureImageView(ImageViewActivity.this);
									imageView.setImageBitmap(finalResult);
									imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
									setContentView(imageView);

									imageView.setOnClickListener(new View.OnClickListener() {
										public void onClick(View v) {
											finish();
										}
									});
								}
							});
						}
					}
				});

		setContentView(layout);
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		if(gifThread != null) gifThread.stopPlaying();
		if(imageView != null && imageView instanceof GestureImageView) {
			((GestureImageView)imageView).recycle();
		}
	}

	private void revertToWeb() {
		LinkHandler.onLinkClicked(ImageViewActivity.this, url.toString(), true);
		new Handler(Looper.getMainLooper()).post(new Runnable() {
			public void run() {
				finish();
			}
		});
	}
}
