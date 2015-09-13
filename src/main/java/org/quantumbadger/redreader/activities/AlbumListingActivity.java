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
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.image.GetAlbumInfoListener;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

import java.util.regex.Matcher;

public class AlbumListingActivity extends BaseActivity {

	private String mUrl;
	private boolean mHaveReverted = false;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences);

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
			albumId = matchImgur.group(1);
		} else {
			revertToWeb();
			return;
		}

		Log.i("AlbumListingActivity", "Loading URL " + mUrl + ", album id " + albumId);

		final ProgressBar progressBar = new ProgressBar(this, null, android.R.attr.progressBarStyleHorizontal);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(progressBar);

		ImgurAPI.getAlbumInfo(this, albumId, Constants.Priority.IMAGE_VIEW, 0, new GetAlbumInfoListener() {

			@Override
			public void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
				General.showResultDialog(AlbumListingActivity.this, new RRError(type.toString(), readableMessage, t, status, mUrl));
				//revertToWeb();
			}

			@Override
			public void onSuccess(final ImgurAPI.AlbumInfo info) {
				Log.i("AlbumListingActivity", "Got album, " + info.images.size() + " image(s)");

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						layout.removeAllViews();

						for(final ImgurAPI.ImageInfo image : info.images) {

							final Button button = new Button(AlbumListingActivity.this);
							button.setText(
									image.title != null && !image.title.isEmpty()
											? "IMAGE NO + TITLE " + image.title
											: "IMAGE NO");
							button.setOnClickListener(new View.OnClickListener() {
								@Override
								public void onClick(final View v) {
									LinkHandler.onLinkClicked(
											AlbumListingActivity.this,
											image.urlOriginal);
								}
							});

							layout.addView(button);
						}

					}
				});
			}
		});

		setContentView(layout);
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.url, false, post.src);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.idAlone).generateJsonUri().toString(), false);
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
