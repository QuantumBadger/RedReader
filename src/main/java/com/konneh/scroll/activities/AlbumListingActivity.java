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

package com.konneh.scroll.activities;

import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import org.apache.http.StatusLine;
import com.konneh.scroll.R;
import com.konneh.scroll.adapters.AlbumAdapter;
import com.konneh.scroll.cache.RequestFailureType;
import com.konneh.scroll.common.*;
import com.konneh.scroll.image.GetAlbumInfoListener;
import com.konneh.scroll.image.GetImageInfoListener;
import com.konneh.scroll.image.ImgurAPI;

import java.util.regex.Matcher;

public class AlbumListingActivity extends BaseActivity {

	private String mUrl;
	private boolean mHaveReverted = false;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		PrefsUtility.applyTheme(this);

		OptionsMenuUtility.fixActionBar(AlbumListingActivity.this, getString(R.string.imgur_album));

		if(getActionBar() != null) {
			getActionBar().setHomeButtonEnabled(true);
			getActionBar().setDisplayHomeAsUpEnabled(true);
		}

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences)
				&& PrefsUtility.appearance_theme(this, sharedPreferences) == PrefsUtility.AppearanceTheme.NIGHT;

		if(solidblack) getWindow().setBackgroundDrawable(new ColorDrawable(Color.BLACK));

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

		ImgurAPI.getAlbumInfo(this, albumId, Constants.Priority.IMAGE_VIEW, 0, new GetAlbumInfoListener() {

			@Override
			public void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
				Log.e("AlbumListingActivity", "getAlbumInfo call failed: " + type);

				if(status != null) Log.e("AlbumListingActivity", "status was: " + status.toString());
				if(t != null) Log.e("AlbumListingActivity", "exception was: ", t);

				// It might be a single image, not an album

				if(status == null) {
					revertToWeb();
					return;
				}

				ImgurAPI.getImageInfo(AlbumListingActivity.this, albumId, Constants.Priority.IMAGE_VIEW, 0, new GetImageInfoListener() {
					@Override
					public void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
						Log.e("AlbumListingActivity", "Image info request also failed: " + type);
						revertToWeb();
					}

					@Override
					public void onSuccess(final ImgurAPI.ImageInfo info) {
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

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						if(info.title != null && !info.title.trim().isEmpty()) {
							OptionsMenuUtility.fixActionBar(AlbumListingActivity.this, getString(R.string.imgur_album) + ": " + info.title);
						}

						layout.removeAllViews();

						final ListView listView = new ListView(AlbumListingActivity.this);
						listView.setAdapter(new AlbumAdapter(info));
						layout.addView(listView);

						listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
							@Override
							public void onItemClick(
									final AdapterView<?> parent,
									final View view,
									final int position,
									final long id) {

								LinkHandler.onLinkClicked(
										AlbumListingActivity.this,
										info.images.get(position).urlOriginal,
										false,
										null,
										info,
										position);
							}
						});
					}
				});
			}
		});

		setContentView(layout);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				finish();
				return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	private void revertToWeb() {

		final Runnable r = new Runnable() {
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
			AndroidApi.UI_THREAD_HANDLER.post(r);
		}
	}
}
