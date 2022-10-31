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

package org.saiditnet.redreader.views;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Typeface;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.BetterSSB;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.RRTime;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;

public class RedditPostHeaderView extends LinearLayout {

	private final RedditPreparedPost post;

	private final TextView subtitle;

	public RedditPostHeaderView(final AppCompatActivity activity, final RedditPreparedPost post) {

		super(activity);
		this.post = post;

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final Typeface tf = Typeface.createFromAsset(activity.getAssets(), "fonts/Roboto-Light.ttf");

		final TextView title = new TextView(activity);
		title.setTextSize(19.0f);
		title.setTypeface(tf);
		title.setText(post.src.getTitle());
		title.setTextColor(Color.WHITE);
		addView(title);

		subtitle = new TextView(activity);
		subtitle.setTextSize(13.0f);
		rebuildSubtitle(activity);

		subtitle.setTextColor(Color.rgb(200, 200, 200));
		addView(subtitle);

		{
			final TypedArray appearance = activity.obtainStyledAttributes(new int[]{
					R.attr.rrPostListHeaderBackgroundCol});

			setBackgroundColor(appearance.getColor(0, General.COLOR_INVALID));

			appearance.recycle();
		}

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(final View v) {
				if(!post.isSelf()) {
					LinkHandler.onLinkClicked(activity, post.src.getUrl(), false, post.src.getSrc());
				}
			}
		});

		setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(final View v) {
				RedditPreparedPost.showActionMenu(activity, post);
				return true;
			}
		});
	}

	private void rebuildSubtitle(Context context) {

		// TODO customise display
		// TODO preference for the X days, X hours thing

		final int boldCol = Color.WHITE;
		final int rrPostSubtitleUpvoteCol;
		final int rrPostSubtitleDownvoteCol;
		final int rrGoldTextCol;
		final int rrGoldBackCol;

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[]{
					R.attr.rrPostSubtitleBoldCol,
					R.attr.rrPostSubtitleUpvoteCol,
					R.attr.rrPostSubtitleDownvoteCol,
					R.attr.rrGoldTextCol,
					R.attr.rrGoldBackCol

			});

			rrPostSubtitleUpvoteCol = appearance.getColor(1, 255);
			rrPostSubtitleDownvoteCol = appearance.getColor(2, 255);
			rrGoldTextCol = appearance.getColor(3, 255);
			rrGoldBackCol = appearance.getColor(4, 255);

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

		if(post.src.isNsfw()) {
			postListDescSb.append(" NSFW ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.RED, 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(String.valueOf(post.computeScore()), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, pointsCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_points) + " ", 0);

		if(post.src.getGoldAmount() > 0) {
			postListDescSb.append(" ", 0);
			postListDescSb.append(" " + context.getString(R.string.gold) + " x" + post.src.getGoldAmount() + " ",
					BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR, rrGoldTextCol, rrGoldBackCol, 1f);
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(RRTime.formatDurationFrom(context, post.src.getCreatedTimeSecsUTC() * 1000), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_by) + " ", 0);
		postListDescSb.append(post.src.getAuthor(), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_to) + " ", 0);
		postListDescSb.append(post.src.getSubreddit(), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);

		postListDescSb.append(" (" + post.src.getDomain() + ")", 0);

		subtitle.setText(postListDescSb.get());
	}
}
