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
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.TypedValue;
import android.view.ViewGroup;
import android.widget.ImageView;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.ListView;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.list.SwipableListItemView;

public final class RedditPostView extends SwipableListItemView implements RedditPreparedPost.ThumbnailLoadedCallback {

	private final float dpScale;

	private RedditPreparedPost post = null;
	private final TextView title, subtitle;

	private final ImageView thumbnailView, savedIcon, hiddenIcon;

	private final LinearLayout commentsButton;
	private final TextView commentsText;

	private int usageId = 0;

	private final Handler thumbnailHandler;

	private final PostListingFragment fragmentParent;

	private boolean swipeReady = false, leftOverlayShown = false, rightOverlayShown = false;

	private final PrefsUtility.PostFlingAction leftFlingPref, rightFlingPref;
	private ActionDescriptionPair leftFlingAction, rightFlingAction;

	private final TextView leftOverlayText, rightOverlayText;

	private final Drawable rrIconFfLeft, rrIconFfRight, rrIconTick;
	private final int rrPostTitleReadCol, rrPostTitleCol;

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

				RedditPreparedPost.onActionMenuItemSelected(post, fragmentParent, rightFlingAction.action);
				leftOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			} else {
				RedditPreparedPost.onActionMenuItemSelected(post, fragmentParent, leftFlingAction.action);
				rightOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			}

			swipeReady = false;

		} else if(absOffset > 5) {

			if(xOffsetPixels > 0) {

				// Right swipe

				if(!leftOverlayShown) {
					leftOverlayShown = true;
					leftOverlayText.setVisibility(VISIBLE);
				}

				if(rightOverlayShown) {
					rightOverlayShown = false;
					rightOverlayText.setVisibility(GONE);
				}

			} else {

				// Left swipe

				if(!rightOverlayShown) {
					rightOverlayShown = true;
					rightOverlayText.setVisibility(VISIBLE);
				}

				if(leftOverlayShown) {
					leftOverlayShown = false;
					leftOverlayText.setVisibility(GONE);
				}
			}

		} else {

			if(leftOverlayShown) {
				leftOverlayShown = false;
				leftOverlayText.setVisibility(GONE);
			}

			if(rightOverlayShown) {
				rightOverlayShown = false;
				rightOverlayText.setVisibility(GONE);
			}
		}
	}

	public RedditPostView(final Context context, final ListView listParent, final PostListingFragment fragmentParent) {

		super(context, listParent);
		this.fragmentParent = fragmentParent;

		offsetBeginAllowed = General.dpToPixels(context, 50);
		offsetActionPerformed = General.dpToPixels(context, 150);

		thumbnailHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {
				if(usageId != msg.what) return;
				thumbnailView.setImageBitmap((Bitmap)msg.obj);
				invalidate(); // TODO is this necessary?
			}
		};

		dpScale = context.getResources().getDisplayMetrics().density; // TODO xml?

		final float fontScale = PrefsUtility.appearance_fontscale_posts(context, PreferenceManager.getDefaultSharedPreferences(context));

		final FrameLayout mainLayout = (FrameLayout) inflate(context, R.layout.reddit_post, null);
		final LinearLayout visiblePostLayout = (LinearLayout) mainLayout.findViewById(R.id.reddit_post_layout);

		thumbnailView = (ImageView) mainLayout.findViewById(R.id.reddit_post_thumbnail_view);
		savedIcon = (ImageView) mainLayout.findViewById(R.id.reddit_post_saved_icon);
		hiddenIcon = (ImageView) mainLayout.findViewById(R.id.reddit_post_hidden_icon);

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

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrIconFfLeft,
				R.attr.rrIconFfRight,
				R.attr.rrIconTick,
				R.attr.rrPostTitleCol,
				R.attr.rrPostTitleReadCol
		});

		rrIconFfLeft = attr.getDrawable(0);
		rrIconFfRight = attr.getDrawable(1);
		rrIconTick = attr.getDrawable(2);
		rrPostTitleCol = attr.getColor(3, 0);
		rrPostTitleReadCol = attr.getColor(4, 0);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		setSwipableView(visiblePostLayout);
		setVisibleView(mainLayout);
	}

	// Only run in the UI thread
	public void reset(final RedditPreparedPost data) {

		usageId++;

		super.reset();
		if(post != null) post.unbind(this);
		data.bind(this);

		swipeReady = false;
		leftOverlayShown = false;
		rightOverlayShown = false;
		leftOverlayText.setVisibility(GONE);
		rightOverlayText.setVisibility(GONE);

		this.post = data;

		final Bitmap thumbnail = data.getThumbnail(this, usageId);
		thumbnailView.setImageBitmap(thumbnail);

		title.setText(data.title);
		commentsText.setText(String.valueOf(post.commentCount));

		if(data.hasThumbnail) {
			thumbnailView.setVisibility(VISIBLE);
			thumbnailView.setMinimumWidth((int)(64.0f * dpScale)); // TODO remove constant, customise
			thumbnailView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		} else {
			thumbnailView.setMinimumWidth(0);
			thumbnailView.setVisibility(GONE);
		}

		updateAppearance();
	}

	public void updateAppearance() {

		if(post.isRead()) {
			title.setTextColor(rrPostTitleReadCol);
		} else {
			title.setTextColor(rrPostTitleCol);
		}

		subtitle.setText(post.postListDescription);

		if(post.isSaved()) {
			savedIcon.setVisibility(ImageView.VISIBLE);
			hiddenIcon.setVisibility(ImageView.GONE);
		} else {
			savedIcon.setVisibility(ImageView.GONE);
			hiddenIcon.setVisibility(post.isHidden() ? ImageView.VISIBLE : ImageView.GONE);
		}
	}

	public void rrOnClick(final int x, final int y) {

		final int[] buttonLoc = new int[2];
		commentsButton.getLocationOnScreen(buttonLoc);

		if(x >= buttonLoc[0]
				&& x <= buttonLoc[0] + commentsButton.getWidth()
				&& y >= buttonLoc[1]
				&& y <= buttonLoc[1] + commentsButton.getHeight()) {

			fragmentParent.onPostCommentsSelected(post);

		} else {
			fragmentParent.onPostSelected(post);
		}
	}

	public void rrOnLongClick() {
		RedditPreparedPost.showActionMenu(getContext(), fragmentParent, post);
	}

	public void betterThumbnailAvailable(final Bitmap thumbnail, final int callbackUsageId) {
		final Message msg = new Message();
		msg.obj = thumbnail;
		msg.what = callbackUsageId;
		thumbnailHandler.sendMessage(msg);
	}

	public static interface PostSelectionListener {
		public void onPostSelected(RedditPreparedPost post);
		public void onPostCommentsSelected(RedditPreparedPost post);
	}
}
