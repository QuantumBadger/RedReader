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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Typeface;
import android.preference.PreferenceManager;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;

public class RedditPostHeaderView extends LinearLayout {

	private final RedditPreparedPost post;

	private final TextView subtitle;

	@Nullable private final Runnable mChangeListenerAddTask;
	@Nullable private final Runnable mChangeListenerRemoveTask;

	public RedditPostHeaderView(final AppCompatActivity activity, final RedditPreparedPost post) {

		super(activity);
		this.post = post;

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final LinearLayout greyHeader = new LinearLayout(activity);
		greyHeader.setOrientation(LinearLayout.VERTICAL);

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		greyHeader.setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final Typeface tf = Typeface.createFromAsset(activity.getAssets(), "fonts/Roboto-Light.ttf");

		final float titleFontScale;
		if(PrefsUtility.appearance_fontscale_post_use_different_scales(activity, PreferenceManager.getDefaultSharedPreferences(activity))) {
			titleFontScale = PrefsUtility.appearance_fontscale_post_header_titles(activity, PreferenceManager.getDefaultSharedPreferences(activity));
		} else {
			titleFontScale = PrefsUtility.appearance_fontscale_posts(activity, PreferenceManager.getDefaultSharedPreferences(activity));
		}

		final TextView title = new TextView(activity);
		title.setTextSize(19.0f * titleFontScale);
		title.setTypeface(tf);
		title.setText(post.src.getTitle());
		title.setTextColor(Color.WHITE);
		greyHeader.addView(title);

		final float subtitleFontScale;
		if(PrefsUtility.appearance_fontscale_post_use_different_scales(activity, PreferenceManager.getDefaultSharedPreferences(activity))) {
			subtitleFontScale = PrefsUtility.appearance_fontscale_post_header_subtitles(activity, PreferenceManager.getDefaultSharedPreferences(activity));
		} else {
			subtitleFontScale = PrefsUtility.appearance_fontscale_post_subtitles(activity, PreferenceManager.getDefaultSharedPreferences(activity));
		}

		subtitle = new TextView(activity);
		subtitle.setTextSize(13.0f * subtitleFontScale);
		rebuildSubtitle(activity);

		subtitle.setTextColor(Color.rgb(200, 200, 200));
		greyHeader.addView(subtitle);

		{
			final TypedArray appearance = activity.obtainStyledAttributes(new int[]{
					R.attr.rrPostListHeaderBackgroundCol});

			greyHeader.setBackgroundColor(appearance.getColor(0, General.COLOR_INVALID));

			appearance.recycle();
		}

		greyHeader.setOnClickListener(v -> {
			if(!post.isSelf()) {
				LinkHandler.onLinkClicked(activity, post.src.getUrl(), false, post.src.getSrc());
			}
		});

		greyHeader.setOnLongClickListener(v -> {
			RedditPreparedPost.showActionMenu(activity, post);
			return true;
		});

		addView(greyHeader);

		final RedditAccount currentUser = RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(!currentUser.isAnonymous()) {

			// A user is logged in

			final LinearLayout buttons = (LinearLayout)inflate(activity, R.layout.post_header_toolbar, this);

			final ImageButton buttonAddUpvote = buttons.findViewById(R.id.post_toolbar_botton_add_upvote);
			final ImageButton buttonRemoveUpvote = buttons.findViewById(R.id.post_toolbar_botton_remove_upvote);
			final ImageButton buttonAddDownvote = buttons.findViewById(R.id.post_toolbar_botton_add_downvote);
			final ImageButton buttonRemoveDownvote = buttons.findViewById(R.id.post_toolbar_botton_remove_downvote);
			final ImageButton buttonReply = buttons.findViewById(R.id.post_toolbar_botton_reply);
			final ImageButton buttonShare = buttons.findViewById(R.id.post_toolbar_botton_share);
			final ImageButton buttonMore = buttons.findViewById(R.id.post_toolbar_botton_more);

			buttonAddUpvote.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.UPVOTE));
			buttonRemoveUpvote.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.UNVOTE));
			buttonAddDownvote.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.DOWNVOTE));
			buttonRemoveDownvote.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.UNVOTE));
			buttonReply.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.REPLY));
			buttonShare.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.SHARE));
			buttonMore.setOnClickListener(v -> post.performAction(activity, RedditPreparedPost.Action.ACTION_MENU));

			final RedditChangeDataManager changeDataManager = RedditChangeDataManager.getInstance(currentUser);

			final RedditChangeDataManager.Listener changeListener = thingIdAndType -> {

				rebuildSubtitle(activity);

				final boolean isUpvoted = changeDataManager.isUpvoted(post.src);
				final boolean isDownvoted = changeDataManager.isDownvoted(post.src);

				if(isUpvoted) {
					buttonAddUpvote.setVisibility(GONE);
					buttonRemoveUpvote.setVisibility(VISIBLE);
					buttonAddDownvote.setVisibility(VISIBLE);
					buttonRemoveDownvote.setVisibility(GONE);

				} else if(isDownvoted) {
					buttonAddUpvote.setVisibility(VISIBLE);
					buttonRemoveUpvote.setVisibility(GONE);
					buttonAddDownvote.setVisibility(GONE);
					buttonRemoveDownvote.setVisibility(VISIBLE);

				} else {
					buttonAddUpvote.setVisibility(VISIBLE);
					buttonRemoveUpvote.setVisibility(GONE);
					buttonAddDownvote.setVisibility(VISIBLE);
					buttonRemoveDownvote.setVisibility(GONE);
				}
			};

			mChangeListenerAddTask = () -> {
				changeDataManager.addListener(post.src, changeListener);
				changeListener.onRedditDataChange(post.src.getIdAndType());
			};

			mChangeListenerRemoveTask = () -> {
				changeDataManager.removeListener(post.src, changeListener);
			};

		} else {
			mChangeListenerAddTask = null;
			mChangeListenerRemoveTask = null;
		}
	}

	@Override
	protected void onAttachedToWindow() {
		super.onAttachedToWindow();

		if(mChangeListenerAddTask != null) {
			mChangeListenerAddTask.run();
		}
	}

	@Override
	protected void onDetachedFromWindow() {
		super.onDetachedFromWindow();

		if(mChangeListenerRemoveTask != null) {
			mChangeListenerRemoveTask.run();
		}
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
			postListDescSb.append(" " + context.getString(R.string.gold) + BetterSSB.NBSP + "x" + post.src.getGoldAmount() + " ",
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
