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

package org.saiditnet.redreader.activities;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.adapters.AlbumAdapter;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.image.GetAlbumInfoListener;
import org.saiditnet.redreader.image.GetImageInfoListener;
import org.saiditnet.redreader.image.ImageInfo;
import org.saiditnet.redreader.image.ImgurAPI;
import org.saiditnet.redreader.views.ScrollbarRecyclerViewManager;

import java.util.regex.Matcher;

public class AlbumListingActivity extends BaseActivity {

	private String mUrl;
	private boolean mHaveReverted = false;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.imgur_album);

		final Intent intent = getIntent();

		mUrl = intent.getDataString();

		if(mUrl == null) {
			finish();
			return;
		}

		final Matcher matchImgur = LinkHandler.imgurAlbumPattern.matcher(mUrl);
		final String albumId;

		if(matchImgur.find()) {
			albumId = matchImgur.group(2);
		} else {
			Log.e("AlbumListingActivity", "URL match failed");
			revertToWeb();
			return;
		}

		Log.i("AlbumListingActivity", "Loading URL " + mUrl + ", album id " + albumId);

		final ProgressBar progressBar = new ProgressBar(this, null, android.R.attr.progressBarStyleHorizontal);
		progressBar.setIndeterminate(true);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(progressBar);

		LinkHandler.getImgurAlbumInfo(this, albumId, Constants.Priority.IMAGE_VIEW, 0, new GetAlbumInfoListener() {

			@Override
			public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				Log.e("AlbumListingActivity", "getAlbumInfo call failed: " + type);

				if(status != null) Log.e("AlbumListingActivity", "status was: " + status.toString());
				if(t != null) Log.e("AlbumListingActivity", "exception was: ", t);

				// It might be a single image, not an album

				if(status == null) {
					revertToWeb();
					return;
				}

				LinkHandler.getImgurImageInfo(AlbumListingActivity.this, albumId, Constants.Priority.IMAGE_VIEW, 0, false, new GetImageInfoListener() {
					@Override
					public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
						Log.e("AlbumListingActivity", "Image info request also failed: " + type);
						revertToWeb();
					}

					@Override
					public void onSuccess(final ImageInfo info) {
						Log.i("AlbumListingActivity", "Link was actually an image.");
						LinkHandler.onLinkClicked(AlbumListingActivity.this, info.urlOriginal);
						finish();
					}

					@Override
					public void onNotAnImage() {
						Log.i("AlbumListingActivity", "Not an image either");
						revertToWeb();
					}
				});
			}

			@Override
			public void onSuccess(final ImgurAPI.AlbumInfo info) {
				Log.i("AlbumListingActivity", "Got album, " + info.images.size() + " image(s)");

				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						if(info.title != null && !info.title.trim().isEmpty()) {
							setTitle(getString(R.string.imgur_album) + ": " + info.title);
						}

						layout.removeAllViews();

						if(info.images.size() == 1) {
							LinkHandler.onLinkClicked(AlbumListingActivity.this, info.images.get(0).urlOriginal);
							finish();

						} else {
							final ScrollbarRecyclerViewManager recyclerViewManager
									= new ScrollbarRecyclerViewManager(AlbumListingActivity.this, null, false);

							layout.addView(recyclerViewManager.getOuterView());

							recyclerViewManager.getRecyclerView().setAdapter(new AlbumAdapter(
									AlbumListingActivity.this,
									info));
						}
					}
				});
			}
		});

		setBaseActivityContentView(layout);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	private void revertToWeb() {

		final Runnable r = new Runnable() {
			@Override
			public void run() {
				if(!mHaveReverted) {
					mHaveReverted = true;
					LinkHandler.onLinkClicked(AlbumListingActivity.this, mUrl, true);
					finish();
				}
			}
		};

		if(General.isThisUIThread()) {
			r.run();
		} else {
			AndroidCommon.UI_THREAD_HANDLER.post(r);
		}
	}
}
