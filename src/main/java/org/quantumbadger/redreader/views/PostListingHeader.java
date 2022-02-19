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

package org.quantumbadger.redreader.views;

import android.content.res.TypedArray;
import android.graphics.Color;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.Fonts;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostListingURL;


public final class PostListingHeader extends LinearLayout {

	public PostListingHeader(
			final AppCompatActivity activity,
			final String titleText,
			final String subtitleText,
			final PostListingURL url,
			@Nullable final RedditSubreddit subreddit) {

		super(activity);

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final LinearLayout greyHeader = new LinearLayout(activity);
		greyHeader.setOrientation(LinearLayout.VERTICAL);

		{
			final TypedArray appearance = activity.obtainStyledAttributes(new int[] {
					R.attr.rrPostListHeaderBackgroundCol});

			greyHeader.setBackgroundColor(appearance.getColor(0, General.COLOR_INVALID));

			appearance.recycle();
		}

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		greyHeader.setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final TextView title = new TextView(activity);
		title.setText(titleText);
		title.setTextSize(22.0f);
		title.setTypeface(Fonts.getRobotoLightOrAlternative());
		title.setTextColor(Color.WHITE);
		greyHeader.addView(title);

		final TextView subtitle = new TextView(activity);
		subtitle.setTextSize(14.0f);
		subtitle.setText(subtitleText);
		subtitle.setTextColor(Color.rgb(200, 200, 200));
		greyHeader.addView(subtitle);

		addView(greyHeader);

		if(subreddit != null
				&& !PrefsUtility.pref_appearance_hide_headertoolbar_postlist()) {

			final SubredditToolbar buttons =
					inflate(activity, R.layout.subreddit_header_toolbar, this)
							.findViewById(R.id.subreddit_toolbar_layout);

			buttons.bindSubreddit(subreddit, Optional.of(url.browserUrl()));
		}
	}
}
