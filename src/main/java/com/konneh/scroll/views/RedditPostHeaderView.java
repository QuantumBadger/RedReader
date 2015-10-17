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

package com.konneh.scroll.views;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Typeface;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.konneh.scroll.R;
import com.konneh.scroll.common.BetterSSB;
import com.konneh.scroll.common.RRTime;
import com.konneh.scroll.reddit.prepared.RedditPreparedPost;

public class RedditPostHeaderView extends LinearLayout {

	private final RedditPreparedPost post;

	private final TextView subtitle;

	public RedditPostHeaderView(final Context context, final RedditPreparedPost post) {

		super(context);
		this.post = post;

		final float dpScale = context.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final Typeface tf = Typeface.createFromAsset(context.getAssets(), "fonts/Roboto-Light.ttf");

		final TextView title = new TextView(context);
		title.setTextSize(19.0f);
		title.setTypeface(tf);
		title.setText(post.title);
		title.setTextColor(Color.WHITE);
		addView(title);

		subtitle = new TextView(context);
		subtitle.setTextSize(13.0f);
		rebuildSubtitle(context);

		subtitle.setTextColor(Color.rgb(200, 200, 200));
		addView(subtitle);

		setBackgroundColor(Color.rgb(50, 50, 50)); // TODO color
	}

	private void rebuildSubtitle(Context context) {

		// TODO customise display
		// TODO preference for the X days, X hours thing

		final int boldCol = Color.WHITE;
		final int rrPostSubtitleUpvoteCol;
		final int rrPostSubtitleDownvoteCol;

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[]{
					R.attr.rrPostSubtitleBoldCol,
					R.attr.rrPostSubtitleUpvoteCol,
					R.attr.rrPostSubtitleDownvoteCol
			});

			rrPostSubtitleUpvoteCol = appearance.getColor(1, 255);
			rrPostSubtitleDownvoteCol = appearance.getColor(2, 255);

			appearance.recycle();
		}

		final BetterSSB postListDescSb = new BetterSSB();

		final int pointsCol;
		if(post.isUpvoted()) {
			pointsCol = rrPostSubtitleUpvoteCol;
		} else if(post.isDownvoted()) {
			pointsCol = rrPostSubtitleDownvoteCol;
		} else {
			pointsCol = boldCol;
		}

		if(post.src.over_18) {
			postListDescSb.append(" NSFW ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.RED, 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(String.valueOf(post.src.score), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, pointsCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_points) + " ", 0);
		postListDescSb.append(RRTime.formatDurationFrom(context, post.src.created_utc * 1000), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_by) + " ", 0);
		postListDescSb.append(post.src.author, BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_to) + " ", 0);
		postListDescSb.append(post.src.subreddit, BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);

		postListDescSb.append(" (" + post.src.domain + ")", 0);

		subtitle.setText(postListDescSb.get());
	}
}
