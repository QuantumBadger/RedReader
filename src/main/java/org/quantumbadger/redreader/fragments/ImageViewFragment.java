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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import com.polites.android.GestureImageView;
import org.apache.http.StatusLine;
import org.holoeverywhere.LayoutInflater;
import org.holoeverywhere.app.Fragment;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.image.GifDecoderThread;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

public class ImageViewFragment extends Fragment implements RedditPostView.PostSelectionListener {

	private URI url;

	private ImageView imageView;
	private GifDecoderThread gifThread;

	public static ImageViewFragment newInstance(final URI url, final RedditPost post) {

		final ImageViewFragment f = new ImageViewFragment();

		final Bundle bundle = new Bundle(1);
		bundle.putString("url", url.toString());
		if(post != null) bundle.putParcelable("post", post);
		f.setArguments(bundle);

		return f;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		// TODO load position/etc?
		super.onCreate(savedInstanceState);
		url = General.uriFromString(getArguments().getString("url"));
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, final ViewGroup container, final Bundle savedInstanceState) {

		final Context context = inflater.getContext();

		final LoadingView loadingView = new LoadingView(context, R.string.download_loading, true, false);

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(loadingView);

		CacheManager.getInstance(context).makeRequest(
				new CacheRequest(
						url,
						RedditAccountManager.getAnon(),
						null,
						Constants.Priority.IMAGE_VIEW,
						0,
						CacheRequest.DownloadType.IF_NECESSARY,
						Constants.FileType.IMAGE,
						false, false, false, context) {

					private void setContentView(View v) {
						layout.removeAllViews();
						layout.addView(v);
						v.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
						v.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
					}

					@Override
					protected void onCallbackException(Throwable t) {
						BugReportActivity.handleGlobalError(context.getApplicationContext(), new RRError(null, null, t));
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
						final RRError error = General.getGeneralErrorForFailure(context, type, t, status);

						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								// TODO handle properly
								layout.addView(new ErrorView(getSupportActivity(), error));
							}
						});
					}

					@Override
					protected void onProgress(long bytesRead, long totalBytes) {
						loadingView.setProgress(R.string.download_downloading, (float)((double)bytesRead / (double)totalBytes));
					}

					@Override
					protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, final String mimetype) {

						if(mimetype == null || !Constants.Mime.isImage(mimetype)) {
							revertToWeb();
							return;
						}

						if(Constants.Mime.isImageGif(mimetype)) {

							try {

								gifThread = new GifDecoderThread(cacheFile.getInputStream(), new GifDecoderThread.OnGifLoadedListener() {

									public void onGifLoaded() {
										new Handler(Looper.getMainLooper()).post(new Runnable() {
											public void run() {
												imageView = new ImageView(context);
												imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
												setContentView(imageView);
												gifThread.setView(imageView);

												imageView.setOnClickListener(new View.OnClickListener() {
													public void onClick(View v) {
														getSupportActivity().finish();
													}
												});
											}
										});
									}

									public void onOutOfMemory() {
										General.quickToast(context, R.string.imageview_oom);
										revertToWeb();
									}

									public void onGifInvalid() {
										General.quickToast(context, R.string.imageview_invalid_gif);
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
									General.quickToast(context, "Couldn't load the image. Trying internal browser.");
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
								General.quickToast(context, "Out of memory. Trying internal browser.");
								revertToWeb();
								return;
							}

							new Handler(Looper.getMainLooper()).post(new Runnable() {
								public void run() {
									imageView = new GestureImageView(context);
									imageView.setImageBitmap(finalResult);
									imageView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
									setContentView(imageView);

									imageView.setOnClickListener(new View.OnClickListener() {
										public void onClick(View v) {
											getSupportActivity().finish();
										}
									});
								}
							});
						}
					}
				});

		final RedditPost src_post = getArguments().getParcelable("post");
		final RedditPreparedPost post = src_post == null ? null
				: new RedditPreparedPost(context, CacheManager.getInstance(context), 0, src_post, -1, false,
				new RedditSubreddit("/r/" + src_post.subreddit, src_post.subreddit, false),
				false, false, false, RedditAccountManager.getInstance(context).getDefaultAccount());

		final FrameLayout outerFrame = new FrameLayout(context);
		outerFrame.addView(layout);

		if(post != null) {

			final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(context);

			final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(context, new BezelSwipeOverlay.BezelSwipeListener() {

				public boolean onSwipe(BezelSwipeOverlay.SwipeEdge edge) {

					toolbarOverlay.setContents(post.generateToolbar(context, ImageViewFragment.this, toolbarOverlay));
					toolbarOverlay.show(edge == BezelSwipeOverlay.SwipeEdge.LEFT ?
							SideToolbarOverlay.SideToolbarPosition.LEFT : SideToolbarOverlay.SideToolbarPosition.RIGHT);
					return true;
				}

				public boolean onTap() {

					if(toolbarOverlay.isShown()) {
						toolbarOverlay.hide();
						return true;
					}

					return false;
				}
			});

			outerFrame.addView(bezelOverlay);
			outerFrame.addView(toolbarOverlay);

			bezelOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
			bezelOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

			toolbarOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
			toolbarOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

		}

		return outerFrame;
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		if(gifThread != null) gifThread.stopPlaying();
		if(imageView != null && imageView instanceof GestureImageView) {
			((GestureImageView)imageView).recycle();
		}
	}

	private void revertToWeb() {
		LinkHandler.onLinkClicked(getSupportActivity(), url.toString(), true);
		new Handler(Looper.getMainLooper()).post(new Runnable() {
			public void run() {
				getSupportActivity().finish();
			}
		});
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostSelected(post);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostCommentsSelected(post);
	}
}
