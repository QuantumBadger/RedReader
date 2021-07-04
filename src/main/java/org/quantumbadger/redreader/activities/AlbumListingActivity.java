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
import android.os.Bundle;
import android.util.Log;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.adapters.AlbumAdapter;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.image.AlbumInfo;
import org.quantumbadger.redreader.image.GetAlbumInfoListener;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.regex.Matcher;

public class AlbumListingActivity extends BaseActivity {

	private String mUrl;
	private boolean mHaveReverted = false;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.image_gallery);

		final Intent intent = getIntent();

		mUrl = intent.getDataString();

		if(mUrl == null) {
			finish();
			return;
		}

		if(General.isSensitiveDebugLoggingEnabled()) {
			Log.i("AlbumListingActivity", "Loading URL " + mUrl);
		}

		final ProgressBar progressBar = new ProgressBar(
				this,
				null,
				android.R.attr.progressBarStyleHorizontal);
		progressBar.setIndeterminate(true);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(progressBar);

		LinkHandler.getAlbumInfo(
				this,
				mUrl,
				new Priority(Constants.Priority.IMAGE_VIEW),
				new GetAlbumInfoListener() {

					@Override
					public void onGalleryRemoved() {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {
							layout.removeAllViews();

							layout.addView(new ErrorView(
									AlbumListingActivity.this,
									new RRError(
											getApplicationContext().getString(
													R.string.image_gallery_removed_title),
											getApplicationContext().getString(
													R.string.image_gallery_removed_message),
											true,
											null,
											null,
											mUrl,
											null)));
						});
					}

					@Override
					public void onGalleryDataNotPresent() {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {
							layout.removeAllViews();

							layout.addView(new ErrorView(
									AlbumListingActivity.this,
									new RRError(
											getApplicationContext().getString(
													R.string.image_gallery_no_data_present_title),
											getApplicationContext().getString(
													R.string.image_gallery_no_data_present_message),
											true,
											null,
											null,
											mUrl,
											null)));
						});
					}

					@Override
					public void onFailure(
							final @CacheRequest.RequestFailureType int type,
							final Throwable t,
							final Integer status,
							final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {
						Log.e(
								"AlbumListingActivity",
								"getAlbumInfo call failed: " + type);

						if(status != null) {
							Log.e(
									"AlbumListingActivity",
									"status was: " + status);
						}
						if(t != null) {
							Log.e("AlbumListingActivity", "exception was: ", t);
						}

						if(status == null) {
							revertToWeb();
							return;
						}

						// It might be a single image, not an album

						final Matcher matchImgur = LinkHandler.imgurAlbumPattern.matcher(mUrl);

						if(matchImgur.find()) {
							final String albumId = matchImgur.group(2);

							LinkHandler.getImgurImageInfo(
									AlbumListingActivity.this,
									albumId,
									new Priority(Constants.Priority.IMAGE_VIEW),
									false,
									new GetImageInfoListener() {
										@Override
										public void onFailure(
												final @CacheRequest.RequestFailureType int type,
												final Throwable t,
												final Integer status,
												final String readableMessage,
												@NonNull final Optional<FailedRequestBody> body) {
											Log.e(
													"AlbumListingActivity",
													"Image info request also failed: "
															+ type);
											revertToWeb();
										}

										@Override
										public void onSuccess(final ImageInfo info) {
											Log.i(
													"AlbumListingActivity",
													"Link was actually an image.");
											LinkHandler.onLinkClicked(
													AlbumListingActivity.this,
													info.urlOriginal);
											finish();
										}

										@Override
										public void onNotAnImage() {
											Log.i(
													"AlbumListingActivity",
													"Not an image either");
											revertToWeb();
										}
									});

						} else {
							Log.e(
									"AlbumListingActivity",
									"Not an imgur album, not checking for single image");
							revertToWeb();
						}
					}

					@Override
					public void onSuccess(@NonNull final AlbumInfo info) {
						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.i(
									"AlbumListingActivity",
									"Got album, " + info.images.size() + " image(s)");
						}

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							if(info.title != null && !info.title.trim().isEmpty()) {
								setTitle(getString(R.string.image_gallery)
										+ ": "
										+ info.title);
							}

							layout.removeAllViews();

							if(info.images.size() == 1) {
								LinkHandler.onLinkClicked(
										AlbumListingActivity.this,
										info.images.get(0).urlOriginal);
								finish();

							} else {
								final ScrollbarRecyclerViewManager recyclerViewManager
										= new ScrollbarRecyclerViewManager(
										AlbumListingActivity.this,
										null,
										false);

								layout.addView(recyclerViewManager.getOuterView());

								recyclerViewManager.getRecyclerView()
										.setAdapter(new AlbumAdapter(
												AlbumListingActivity.this,
												info));
							}
						});
					}
				});

		setBaseActivityListing(layout);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}

	private void revertToWeb() {

		final Runnable r = () -> {
			if(!mHaveReverted) {
				mHaveReverted = true;
				LinkHandler.onLinkClicked(this, mUrl, true);
				finish();
			}
		};

		if(General.isThisUIThread()) {
			r.run();
		} else {
			AndroidCommon.UI_THREAD_HANDLER.post(r);
		}
	}
}
