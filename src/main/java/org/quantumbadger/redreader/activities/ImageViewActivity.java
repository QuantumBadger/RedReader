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

import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Color;
import android.media.MediaPlayer;
import android.net.Uri;
import android.opengl.GLSurfaceView;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.VideoView;
import com.github.lzyzsd.circleprogress.DonutProgress;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.ImageInfoDialog;
import org.quantumbadger.redreader.image.GetAlbumInfoListener;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.GifDecoderThread;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.views.GIFView;
import org.quantumbadger.redreader.views.HorizontalSwipeProgressOverlay;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.glview.RRGLSurfaceView;
import org.quantumbadger.redreader.views.imageview.BasicGestureHandler;
import org.quantumbadger.redreader.views.imageview.ImageTileSource;
import org.quantumbadger.redreader.views.imageview.ImageTileSourceWholeBitmap;
import org.quantumbadger.redreader.views.imageview.ImageViewDisplayListManager;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.UUID;

public class ImageViewActivity extends BaseActivity implements RedditPostView.PostSelectionListener, ImageViewDisplayListManager.Listener {

	private static final String TAG = "ImageViewActivity";

	private GLSurfaceView surfaceView;
	private ImageView imageView;
	private GifDecoderThread gifThread;

	private String mUrl;

	private boolean mIsPaused = true, mIsDestroyed = false;
	private CacheRequest mRequest;

	private boolean mHaveReverted = false;

	private ImageViewDisplayListManager mImageViewDisplayerManager;

	private HorizontalSwipeProgressOverlay mSwipeOverlay;
	private boolean mSwipeCancelled;

	private RedditPost mPost;

	private ImageInfo mImageInfo;
	private ImgurAPI.AlbumInfo mAlbumInfo;
	private int mAlbumImageIndex;

	private FrameLayout mLayout;

	private int mGallerySwipeLengthPx;

	private LinearLayout mFloatingToolbar;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		setToolbarActionBarEnabled(false);
		super.onCreate(savedInstanceState);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);

		final int gallerySwipeLengthDp = PrefsUtility.pref_behaviour_gallery_swipe_length_dp(this, sharedPreferences);
		mGallerySwipeLengthPx = General.dpToPixels(this, gallerySwipeLengthDp);

		final Intent intent = getIntent();

		mUrl = intent.getDataString();

		if(mUrl == null) {
			finish();
			return;
		}

		mPost = intent.getParcelableExtra("post");

		if(intent.hasExtra("album")) {
			LinkHandler.getImgurAlbumInfo(
					this,
					intent.getStringExtra("album"),
					Constants.Priority.IMAGE_VIEW,
					0,
					new GetAlbumInfoListener() {

						@Override
						public void onFailure(
								final @CacheRequest.RequestFailureType int type,
								final Throwable t,
								final Integer status,
								final String readableMessage) {

							// Do nothing
						}

						@Override
						public void onSuccess(final ImgurAPI.AlbumInfo info) {
							AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
								@Override
								public void run() {
									mAlbumInfo = info;
									mAlbumImageIndex = intent.getIntExtra("albumImageIndex", 0);
								}
							});
						}
					}
			);
		}

		Log.i(TAG, "Loading URL " + mUrl);

		final DonutProgress progressBar = new DonutProgress(this);
		progressBar.setIndeterminate(true);
		progressBar.setFinishedStrokeColor(Color.rgb(200, 200, 200));
		progressBar.setUnfinishedStrokeColor(Color.rgb(50, 50, 50));
		final int progressStrokeWidthPx = General.dpToPixels(this, 15);
		progressBar.setUnfinishedStrokeWidth(progressStrokeWidthPx);
		progressBar.setFinishedStrokeWidth(progressStrokeWidthPx);
		progressBar.setStartingDegree(-90);
		progressBar.initPainters();

		final RelativeLayout progressLayout = new RelativeLayout(this);
		progressLayout.addView(progressBar);
		final int progressDimensionsPx = General.dpToPixels(this, 150);
		progressBar.getLayoutParams().width = progressDimensionsPx;
		progressBar.getLayoutParams().height = progressDimensionsPx;
		((RelativeLayout.LayoutParams)progressBar.getLayoutParams()).addRule(RelativeLayout.CENTER_IN_PARENT);

		mLayout = new FrameLayout(this);
		mLayout.addView(progressLayout);

		LinkHandler.getImageInfo(this, mUrl, Constants.Priority.IMAGE_VIEW, 0, new GetImageInfoListener() {

			@Override
			public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				revertToWeb();
			}

			@Override
			public void onSuccess(final ImageInfo info) {

				Log.i(TAG, "Got image URL: " + info.urlOriginal);

				mImageInfo = info;

				final URI uri = General.uriFromString(info.urlOriginal);

				if(uri == null) {
					revertToWeb();
					return;
				}

				CacheManager.getInstance(ImageViewActivity.this).makeRequest(
						mRequest = new CacheRequest(
								uri,
								RedditAccountManager.getAnon(),
								null,
								Constants.Priority.IMAGE_VIEW,
								0,
								CacheRequest.DOWNLOAD_IF_NECESSARY,
								Constants.FileType.IMAGE,
								CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
								false,
								false,
								ImageViewActivity.this) {

							@Override
							protected void onCallbackException(Throwable t) {
								BugReportActivity.handleGlobalError(context.getApplicationContext(), new RRError(null, null, t));
							}

							@Override
							protected void onDownloadNecessary() {
								AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										progressBar.setVisibility(View.VISIBLE);
										progressBar.setIndeterminate(true);
									}
								});
							}

							@Override
							protected void onDownloadStarted() {
							}

							@Override
							protected void onFailure(final @CacheRequest.RequestFailureType int type, Throwable t, Integer status, final String readableMessage) {

								final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());

								AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										// TODO handle properly
										mRequest = null;
										final LinearLayout layout = new LinearLayout(context);
										final ErrorView errorView = new ErrorView(ImageViewActivity.this, error);
										layout.addView(errorView);
										errorView.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
										setMainView(layout);
									}
								});
							}

							@Override
							protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {
								AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										progressBar.setVisibility(View.VISIBLE);
										progressBar.setIndeterminate(authorizationInProgress);
										progressBar.setProgress(((float)((1000 * bytesRead) / totalBytes)) / 1000);
									}
								});
							}

							@Override
							protected void onSuccess(
									final CacheManager.ReadableCacheFile cacheFile,
									long timestamp,
									UUID session,
									boolean fromCache,
									final String mimetype) {

								onImageLoaded(cacheFile, mimetype);
							}
						});
			}

			@Override
			public void onNotAnImage() {
				revertToWeb();
			}
		});

		final RedditPreparedPost post;

		if(mPost != null) {

			final RedditParsedPost parsedPost = new RedditParsedPost(mPost, false);

			post = new RedditPreparedPost(
					this,
					CacheManager.getInstance(this),
					0,
					parsedPost,
					-1,
					false,
					false);

		} else {
			post = null;
		}

		final FrameLayout outerFrame = new FrameLayout(this);
		outerFrame.addView(mLayout);

		{
			mFloatingToolbar = (LinearLayout) LayoutInflater.from(this).inflate(R.layout.floating_toolbar, outerFrame, false);
			outerFrame.addView(mFloatingToolbar);

			final ImageButton ib = (ImageButton) LayoutInflater.from(this).inflate(R.layout.flat_image_button, mFloatingToolbar, false);
			final int buttonPadding = General.dpToPixels(this, 10);
			ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding);
			ib.setImageResource(R.drawable.ic_action_info_dark);

			mFloatingToolbar.addView(ib);

			ib.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(final View view) {
					ImageInfoDialog.newInstance(mImageInfo).show(ImageViewActivity.this.getSupportFragmentManager(), null);
				}
			});
		}

		if(post != null) {

			final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(this);

			final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(this, new BezelSwipeOverlay.BezelSwipeListener() {
				@Override
				public boolean onSwipe(@BezelSwipeOverlay.SwipeEdge int edge) {

					toolbarOverlay.setContents(post.generateToolbar(ImageViewActivity.this, false, toolbarOverlay));
					toolbarOverlay.show(edge == BezelSwipeOverlay.LEFT ?
							SideToolbarOverlay.SideToolbarPosition.LEFT : SideToolbarOverlay.SideToolbarPosition.RIGHT);
					return true;
				}

				@Override
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

		setBaseActivityContentView(outerFrame);
	}

	private void setMainView(View v) {

		mLayout.removeAllViews();
		mLayout.addView(v);

		mSwipeOverlay = new HorizontalSwipeProgressOverlay(ImageViewActivity.this);
		mLayout.addView(mSwipeOverlay);

		v.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		v.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	private void onImageLoaded(
			final CacheManager.ReadableCacheFile cacheFile,
			final String mimetype) {

		if(mimetype == null || (!Constants.Mime.isImage(mimetype) && !Constants.Mime.isVideo(mimetype))) {
			revertToWeb();
			return;
		}

		final InputStream cacheFileInputStream;
		try {
			cacheFileInputStream = cacheFile.getInputStream();
		} catch(IOException e) {
			revertToWeb();
			return;
		}

		if(cacheFileInputStream == null) {
			revertToWeb();
			return;
		}

		if(mImageInfo != null
				&& ((mImageInfo.title != null && mImageInfo.title.length() > 0)
						|| (mImageInfo.caption != null && mImageInfo.caption.length() > 0))) {

			// TODO preference
			AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
				@Override
				public void run() {
					mFloatingToolbar.setVisibility(View.VISIBLE);
				}
			});
		}

		if(Constants.Mime.isVideo(mimetype)) {

			AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
				@Override
				public void run() {

					if(mIsDestroyed) return;
					mRequest = null;

					final PrefsUtility.VideoViewMode videoViewMode = PrefsUtility.pref_behaviour_videoview_mode(
							ImageViewActivity.this,
							PreferenceManager.getDefaultSharedPreferences(ImageViewActivity.this));

					if(videoViewMode == PrefsUtility.VideoViewMode.INTERNAL_BROWSER) {
						revertToWeb();

					} else if(videoViewMode == PrefsUtility.VideoViewMode.EXTERNAL_BROWSER) {

						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								LinkHandler.openWebBrowser(ImageViewActivity.this, Uri.parse(mUrl), false);
								finish();
							}
						});

					} else if(videoViewMode == PrefsUtility.VideoViewMode.EXTERNAL_APP_VLC) {

						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								Intent intent = new Intent(Intent.ACTION_VIEW);
								intent.setClassName(
										"org.videolan.vlc",
										"org.videolan.vlc.gui.video.VideoPlayerActivity");
								try {
									intent.setDataAndType(cacheFile.getUri(), mimetype);
								} catch(IOException e) {
									revertToWeb();
									return;
								}

								try {
									startActivity(intent);
								} catch(final Throwable t) {
									General.quickToast(ImageViewActivity.this, R.string.videoview_mode_app_vlc_launch_failed);
									Log.e(TAG, "VLC failed to launch", t);
								}
								finish();
							}
						});

					} else {

						try {
							final RelativeLayout layout = new RelativeLayout(ImageViewActivity.this);
							layout.setGravity(Gravity.CENTER);

							final VideoView videoView = new VideoView(ImageViewActivity.this);

							videoView.setVideoURI(cacheFile.getUri());
							videoView.setMediaController(null);

							layout.addView(videoView);
							setMainView(layout);

							videoView.requestFocus();

							layout.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
							layout.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
							videoView.setLayoutParams(new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));

							videoView.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
								@Override
								public void onPrepared(MediaPlayer mp) {
									mp.setLooping(true);
									videoView.start();
								}
							});

							videoView.setOnErrorListener(
									new MediaPlayer.OnErrorListener() {
										@Override
										public boolean onError(final MediaPlayer mediaPlayer, final int i, final int i1) {
											revertToWeb();
											return true;
										}
									});

							final BasicGestureHandler gestureHandler = new BasicGestureHandler(ImageViewActivity.this);
							videoView.setOnTouchListener(gestureHandler);
							layout.setOnTouchListener(gestureHandler);

						} catch(OutOfMemoryError e) {
							General.quickToast(ImageViewActivity.this, R.string.imageview_oom);
							revertToWeb();

						} catch(Throwable e) {
							General.quickToast(ImageViewActivity.this, R.string.imageview_invalid_video);
							revertToWeb();
						}
					}
				}
			});

		} else if(Constants.Mime.isImageGif(mimetype)) {

			final PrefsUtility.GifViewMode gifViewMode = PrefsUtility.pref_behaviour_gifview_mode(
					this,
					PreferenceManager.getDefaultSharedPreferences(this));

			if(gifViewMode == PrefsUtility.GifViewMode.INTERNAL_BROWSER) {
				revertToWeb();
				return;

			} else if(gifViewMode == PrefsUtility.GifViewMode.EXTERNAL_BROWSER) {
				openInExternalBrowser();
				return;
			}

			if(AndroidApi.isIceCreamSandwichOrLater()
					&& gifViewMode == PrefsUtility.GifViewMode.INTERNAL_MOVIE) {

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						if(mIsDestroyed) return;
						mRequest = null;

						try {
							final GIFView gifView = new GIFView(ImageViewActivity.this, cacheFileInputStream);
							setMainView(gifView);
							gifView.setOnTouchListener(new BasicGestureHandler(ImageViewActivity.this));

						} catch(OutOfMemoryError e) {
							General.quickToast(ImageViewActivity.this, R.string.imageview_oom);
							revertToWeb();

						} catch(Throwable e) {
							General.quickToast(ImageViewActivity.this, R.string.imageview_invalid_gif);
							revertToWeb();
						}
					}
				});

			} else {

				gifThread = new GifDecoderThread(cacheFileInputStream, new GifDecoderThread.OnGifLoadedListener() {

					public void onGifLoaded() {
						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {

								if(mIsDestroyed) return;
								mRequest = null;

								imageView = new ImageView(ImageViewActivity.this);
								imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
								setMainView(imageView);
								gifThread.setView(imageView);

								imageView.setOnTouchListener(new BasicGestureHandler(ImageViewActivity.this));
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

			}

		} else {

			final PrefsUtility.ImageViewMode imageViewMode = PrefsUtility.pref_behaviour_imageview_mode(
					this,
					PreferenceManager.getDefaultSharedPreferences(this));

			if(imageViewMode == PrefsUtility.ImageViewMode.INTERNAL_BROWSER) {
				revertToWeb();
				return;

			} else if(imageViewMode == PrefsUtility.ImageViewMode.EXTERNAL_BROWSER) {
				openInExternalBrowser();
				return;
			}

			final ImageTileSource imageTileSource;
			try {

				final long bytes = cacheFile.getSize();
				final byte[] buf = new byte[(int)bytes];

				try {
					new DataInputStream(cacheFileInputStream).readFully(buf);
				} catch(IOException e) {
					throw new RuntimeException(e);
				}

				try {
					imageTileSource = new ImageTileSourceWholeBitmap(buf);

				} catch(Throwable t) {
					Log.e(TAG, "Exception when creating ImageTileSource", t);
					General.quickToast(this, R.string.imageview_decode_failed);
					revertToWeb();
					return;
				}

			} catch(OutOfMemoryError e) {
				General.quickToast(this, R.string.imageview_oom);
				revertToWeb();
				return;
			}

			AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
				@Override
				public void run() {

					if(mIsDestroyed) return;
					mRequest = null;
					mImageViewDisplayerManager = new ImageViewDisplayListManager(
							ImageViewActivity.this,
							imageTileSource,
							ImageViewActivity.this);
					surfaceView = new RRGLSurfaceView(ImageViewActivity.this, mImageViewDisplayerManager);
					setMainView(surfaceView);

					if(mIsPaused) {
						surfaceView.onPause();
					} else {
						surfaceView.onResume();
					}
				}
			});
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.src.getIdAlone()).generateJsonUri().toString(), false);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	private void revertToWeb() {

		Log.i(TAG, "Reverting to internal browser");

		final Runnable r = new Runnable() {
			@Override
			public void run() {
				if(!mHaveReverted) {
					mHaveReverted = true;
					LinkHandler.onLinkClicked(ImageViewActivity.this, mUrl, true);
					finish();
				}
			}
		};

		if(General.isThisUIThread()) {
			r.run();
		} else {
			AndroidApi.UI_THREAD_HANDLER.post(r);
		}
	}

	private void openInExternalBrowser() {

		final Runnable r = new Runnable() {
			@Override
			public void run() {
				LinkHandler.openWebBrowser(ImageViewActivity.this, Uri.parse(mUrl), false);
				finish();
			}
		};

		if(General.isThisUIThread()) {
			r.run();
		} else {
			AndroidApi.UI_THREAD_HANDLER.post(r);
		}
	}

	@Override
	public void onPause() {

		if(mIsPaused) throw new RuntimeException();

		mIsPaused = true;

		super.onPause();
		if(surfaceView != null) {
			surfaceView.onPause();
		}
	}

	@Override
	public void onResume() {

		if(!mIsPaused) throw new RuntimeException();

		mIsPaused = false;

		super.onResume();
		if(surfaceView != null) {
			surfaceView.onResume();
		}
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		mIsDestroyed = true;
		if(mRequest != null) mRequest.cancel();
		if(gifThread != null) gifThread.stopPlaying();
	}

	@Override
	public void onSingleTap() {
		finish();
	}

	@Override
	public void onHorizontalSwipe(final float pixels) {

		if(mSwipeCancelled) return;

		if(mSwipeOverlay != null && mAlbumInfo != null) {
			mSwipeOverlay.onSwipeUpdate(pixels, mGallerySwipeLengthPx);

			if(pixels >= mGallerySwipeLengthPx) {
				// Back

				mSwipeCancelled = true;
				if(mSwipeOverlay != null) {
					mSwipeOverlay.onSwipeEnd();
				}

				if(mAlbumImageIndex > 0) {

					LinkHandler.onLinkClicked(
							this,
							mAlbumInfo.images.get(mAlbumImageIndex - 1).urlOriginal,
							false,
							mPost,
							mAlbumInfo,
							mAlbumImageIndex - 1);

					finish();

				} else {
					General.quickToast(this, R.string.album_already_first_image);
				}

			} else if(pixels <= -mGallerySwipeLengthPx) {
				// Forwards

				mSwipeCancelled = true;
				if(mSwipeOverlay != null) {
					mSwipeOverlay.onSwipeEnd();
				}

				if(mAlbumImageIndex < mAlbumInfo.images.size() - 1) {

					LinkHandler.onLinkClicked(
							this,
							mAlbumInfo.images.get(mAlbumImageIndex + 1).urlOriginal,
							false,
							mPost,
							mAlbumInfo,
							mAlbumImageIndex + 1);

					finish();

				} else {
					General.quickToast(this, R.string.album_already_last_image);
				}
			}
		}
	}

	@Override
	public void onHorizontalSwipeEnd() {

		mSwipeCancelled = false;

		if(mSwipeOverlay != null) {
			mSwipeOverlay.onSwipeEnd();
		}
	}

	@Override
	public void onImageViewDLMOutOfMemory() {
		if(!mHaveReverted) {
			General.quickToast(this, R.string.imageview_oom);
			revertToWeb();
		}
	}

	@Override
	public void onImageViewDLMException(Throwable t) {
		if(!mHaveReverted) {
			General.quickToast(this, R.string.imageview_decode_failed);
			revertToWeb();
		}
	}

	@Override
	public void onConfigurationChanged(Configuration newConfig) {
		super.onConfigurationChanged(newConfig);
		if(mImageViewDisplayerManager != null){
			mImageViewDisplayerManager.resetTouchState();
		}
	}
}
