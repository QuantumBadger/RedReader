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
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.annotation.UiThread;
import android.support.v7.app.AppCompatActivity;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.list.SwipableListItemView;

public final class RedditPostView extends SwipableListItemView implements RedditPreparedPost.ThumbnailLoadedCallback {

	private final ListView mListView;
	private final float dpScale;

	private RedditPreparedPost post = null;
	private final TextView title, subtitle;

	private final ImageView thumbnailView, overlayIcon;

	private final LinearLayout visiblePostLayout, commentsButton;
	private final TextView commentsText;

	private int usageId = 0;

	private final Handler thumbnailHandler;

	private final PostListingFragment fragmentParent;
	private final AppCompatActivity mActivity;

	private boolean swipeReady = false, leftOverlayShown = false, rightOverlayShown = false;

	private final PrefsUtility.PostFlingAction leftFlingPref, rightFlingPref;
	private ActionDescriptionPair leftFlingAction, rightFlingAction;

	private final TextView leftOverlayText, rightOverlayText;

	private final Drawable rrIconFfLeft, rrIconFfRight, rrIconTick;
	private final int
			rrPostTitleReadCol,
			rrPostTitleCol,
			rrListItemBackgroundCol,
			rrPostBackgroundColSticky,
			rrPostCommentsButtonBackCol,
			rrPostCommentsButtonBackColSticky,
			rrListItemHighlightCol,
			rrPostCommentsButtonHighlightCol;

	private final int offsetBeginAllowed, offsetActionPerformed;

	private final class ActionDescriptionPair {
		public final RedditPreparedPost.Action action;
		public final int descriptionRes;

		private ActionDescriptionPair(RedditPreparedPost.Action action, int descriptionRes) {
			this.action = action;
			this.descriptionRes = descriptionRes;
		}
	}

	@Override
	protected void onSwipeBegin(int xOffsetPixels) {
		if(offsetBeginAllowed > Math.abs(xOffsetPixels)) {

			leftFlingAction = chooseFlingAction(leftFlingPref);
			rightFlingAction = chooseFlingAction(rightFlingPref);

			if(leftFlingAction != null) rightOverlayText.setText(leftFlingAction.descriptionRes);
			if(rightFlingAction != null) leftOverlayText.setText(rightFlingAction.descriptionRes);

			rightOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconFfLeft, null, null);
			leftOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconFfRight, null, null);

			swipeReady = true;
		}
	}

	@Override
	protected boolean leftFlingEnabled() {
		return leftFlingAction != null;
	}

	@Override
	protected boolean rightFlingEnabled() {
		return rightFlingAction != null;
	}

	private ActionDescriptionPair chooseFlingAction(PrefsUtility.PostFlingAction pref) {

		switch(pref) {

			case UPVOTE:
				if(post.isUpvoted()) {
					return new ActionDescriptionPair(RedditPreparedPost.Action.UNVOTE, R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(RedditPreparedPost.Action.UPVOTE, R.string.action_upvote);
				}

			case DOWNVOTE:
				if(post.isDownvoted()) {
					return new ActionDescriptionPair(RedditPreparedPost.Action.UNVOTE, R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(RedditPreparedPost.Action.DOWNVOTE, R.string.action_downvote);
				}

			case SAVE:
				if(post.isSaved()) {
					return new ActionDescriptionPair(RedditPreparedPost.Action.UNSAVE, R.string.action_unsave);
				} else {
					return new ActionDescriptionPair(RedditPreparedPost.Action.SAVE, R.string.action_save);
				}

			case HIDE:
				if(post.isHidden()) {
					return new ActionDescriptionPair(RedditPreparedPost.Action.UNHIDE, R.string.action_unhide);
				} else {
					return new ActionDescriptionPair(RedditPreparedPost.Action.HIDE, R.string.action_hide);
				}

			case COMMENTS:
				return new ActionDescriptionPair(RedditPreparedPost.Action.COMMENTS, R.string.action_comments_short);

			case LINK:
				return new ActionDescriptionPair(RedditPreparedPost.Action.LINK, R.string.action_link_short);

			case BROWSER:
				return new ActionDescriptionPair(RedditPreparedPost.Action.EXTERNAL, R.string.action_external_short);

			case ACTION_MENU:
				return new ActionDescriptionPair(RedditPreparedPost.Action.ACTION_MENU, R.string.action_actionmenu_short);
		}

		return null;
	}

	@Override
	protected void onSwipePositionChange(int xOffsetPixels) {

		final int absOffset = Math.abs(xOffsetPixels);

		if(swipeReady && absOffset > offsetActionPerformed) {

			if(xOffsetPixels > 0) {

				RedditPreparedPost.onActionMenuItemSelected(post, mActivity, rightFlingAction.action);
				leftOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			} else {
				RedditPreparedPost.onActionMenuItemSelected(post, mActivity, leftFlingAction.action);
				rightOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			}

			swipeReady = false;

		} else if(absOffset > 5) {

			if(xOffsetPixels > 0) {

				// Right swipe

				if(!leftOverlayShown) {
					leftOverlayShown = true;
					leftOverlayText.setVisibility(View.VISIBLE);
				}

				if(rightOverlayShown) {
					rightOverlayShown = false;
					rightOverlayText.setVisibility(View.GONE);
				}

			} else {

				// Left swipe

				if(!rightOverlayShown) {
					rightOverlayShown = true;
					rightOverlayText.setVisibility(View.VISIBLE);
				}

				if(leftOverlayShown) {
					leftOverlayShown = false;
					leftOverlayText.setVisibility(View.GONE);
				}
			}

		} else {

			if(leftOverlayShown) {
				leftOverlayShown = false;
				leftOverlayText.setVisibility(View.GONE);
			}

			if(rightOverlayShown) {
				rightOverlayShown = false;
				rightOverlayText.setVisibility(View.GONE);
			}
		}
	}

	public RedditPostView(
			final Context context,
			final ListView listParent,
			final PostListingFragment fragmentParent,
			final AppCompatActivity activity) {

		super(context, listParent);
		this.fragmentParent = fragmentParent;
		mListView = listParent;
		mActivity = activity;

		offsetBeginAllowed = General.dpToPixels(context, 50);
		offsetActionPerformed = General.dpToPixels(context, 150);

		thumbnailHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {
				if(usageId != msg.what) return;
				thumbnailView.setImageBitmap((Bitmap)msg.obj);
				invalidate(); // TODO is this necessary?
				mListView.invalidateViews();
			}
		};

		dpScale = context.getResources().getDisplayMetrics().density; // TODO xml?

		final float fontScale = PrefsUtility.appearance_fontscale_posts(context, PreferenceManager.getDefaultSharedPreferences(context));

		final FrameLayout mainLayout = (FrameLayout) inflate(context, R.layout.reddit_post, null);
		visiblePostLayout = (LinearLayout) mainLayout.findViewById(R.id.reddit_post_layout);

		thumbnailView = (ImageView) mainLayout.findViewById(R.id.reddit_post_thumbnail_view);
		overlayIcon = (ImageView) mainLayout.findViewById(R.id.reddit_post_overlay_icon);

		title = (TextView) mainLayout.findViewById(R.id.reddit_post_title);
		subtitle = (TextView) mainLayout.findViewById(R.id.reddit_post_subtitle);
		commentsButton = (LinearLayout) mainLayout.findViewById(R.id.reddit_post_comments_button);
		commentsText = (TextView)commentsButton.findViewById(R.id.reddit_post_comments_text);

		title.setTextSize(TypedValue.COMPLEX_UNIT_PX, title.getTextSize() * fontScale);
		subtitle.setTextSize(TypedValue.COMPLEX_UNIT_PX, subtitle.getTextSize() * fontScale);

		leftOverlayText = (TextView) mainLayout.findViewById(R.id.reddit_post_fling_text_left);
		rightOverlayText = (TextView) mainLayout.findViewById(R.id.reddit_post_fling_text_right);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
		leftFlingPref = PrefsUtility.pref_behaviour_fling_post_left(context, sharedPreferences);
		rightFlingPref = PrefsUtility.pref_behaviour_fling_post_right(context, sharedPreferences);

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[]{
					R.attr.rrIconFfLeft,
					R.attr.rrIconFfRight,
					R.attr.rrIconTick,
					R.attr.rrPostTitleCol,
					R.attr.rrPostTitleReadCol,
					R.attr.rrListItemBackgroundCol,
					R.attr.rrPostBackgroundColSticky,
					R.attr.rrPostCommentsButtonBackCol,
					R.attr.rrPostCommentsButtonBackColSticky,
					R.attr.rrListItemHighlightCol,
					R.attr.rrPostCommentsButtonHighlightCol
			});

			rrIconFfLeft = attr.getDrawable(0);
			rrIconFfRight = attr.getDrawable(1);
			rrIconTick = attr.getDrawable(2);
			rrPostTitleCol = attr.getColor(3, 0);
			rrPostTitleReadCol = attr.getColor(4, 0);
			rrListItemBackgroundCol = attr.getColor(5, 0);
			rrPostBackgroundColSticky = attr.getColor(6, 0);
			rrPostCommentsButtonBackCol = attr.getColor(7, 0);
			rrPostCommentsButtonBackColSticky = attr.getColor(8, 0);
			rrListItemHighlightCol = attr.getColor(9, 0);
			rrPostCommentsButtonHighlightCol = attr.getColor(10, 0);

			attr.recycle();
		}

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		setSwipableView(visiblePostLayout);
		setVisibleView(mainLayout);
	}

	@UiThread
	public void reset(final RedditPreparedPost data) {

		if(data != post) {

			usageId++;

			super.reset();

			swipeReady = false;
			leftOverlayShown = false;
			rightOverlayShown = false;
			leftOverlayText.setVisibility(GONE);
			rightOverlayText.setVisibility(GONE);

			final Bitmap thumbnail = data.getThumbnail(this, usageId);
			thumbnailView.setImageBitmap(thumbnail);

			title.setText(data.src.getTitle());
			commentsText.setText(String.valueOf(data.src.getSrc().num_comments));

			if(data.hasThumbnail) {
				thumbnailView.setVisibility(VISIBLE);
				thumbnailView.setMinimumWidth((int)(64.0f * dpScale)); // TODO remove constant, customise
				thumbnailView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
			} else {
				thumbnailView.setMinimumWidth(0);
				thumbnailView.setVisibility(GONE);
			}
		}

		if(post != null) post.unbind(this);
		data.bind(this);

		this.post = data;

		updateAppearance();
	}

	public void updateAppearance() {

		if(post.isSticky()) {
			visiblePostLayout.setBackgroundColor(rrPostBackgroundColSticky);
			commentsButton.setBackgroundColor(rrPostCommentsButtonBackColSticky);
		} else {
			visiblePostLayout.setBackgroundColor(rrListItemBackgroundCol);
			commentsButton.setBackgroundColor(rrPostCommentsButtonBackCol);
		}

		if(post.isRead()) {
			title.setTextColor(rrPostTitleReadCol);
		} else {
			title.setTextColor(rrPostTitleCol);
		}

		subtitle.setText(post.postListDescription);

		boolean overlayVisible = true;

		if(post.isSaved()) {
			overlayIcon.setImageResource(R.drawable.ic_action_star_filled_dark);

		} else if(post.isHidden()) {
			overlayIcon.setImageResource(R.drawable.ic_action_cross_dark);

		} else if(post.isUpvoted()) {
			overlayIcon.setImageResource(R.drawable.action_upvote_dark);

		} else if(post.isDownvoted()) {
			overlayIcon.setImageResource(R.drawable.action_downvote_dark);

		} else {
			overlayVisible = false;
		}

		if(overlayVisible) {
			overlayIcon.setVisibility(VISIBLE);
		} else {
			overlayIcon.setVisibility(GONE);
		}
	}

	private boolean isPositionInCommentsButton(final int x, final int y) {

		final int[] buttonLoc = new int[2];
		commentsButton.getLocationOnScreen(buttonLoc);

		return x >= buttonLoc[0]
				&& x <= buttonLoc[0] + commentsButton.getWidth()
				&& y >= buttonLoc[1]
				&& y <= buttonLoc[1] + commentsButton.getHeight();
	}

	public void rrOnClick(final int x, final int y) {

		if(isPositionInCommentsButton(x, y)) {
			fragmentParent.onPostCommentsSelected(post);

		} else {
			fragmentParent.onPostSelected(post);
		}
	}

	public void rrOnLongClick() {
		RedditPreparedPost.showActionMenu(mActivity, post);
	}

	@Override
	public void rrOnHighlightStart(final int x, final int y) {

		if(isPositionInCommentsButton(x, y)) {
			commentsButton.setBackgroundColor(rrPostCommentsButtonHighlightCol);

		} else {
			visiblePostLayout.setBackgroundColor(rrListItemHighlightCol);
		}
	}

	@Override
	public void rrOnHighlightEnd() {
		updateAppearance();
	}

	public void betterThumbnailAvailable(final Bitmap thumbnail, final int callbackUsageId) {
		final Message msg = Message.obtain();
		msg.obj = thumbnail;
		msg.what = callbackUsageId;
		thumbnailHandler.sendMessage(msg);
	}

	public interface PostSelectionListener {
		void onPostSelected(RedditPreparedPost post);

		void onPostCommentsSelected(RedditPreparedPost post);
	}
}
