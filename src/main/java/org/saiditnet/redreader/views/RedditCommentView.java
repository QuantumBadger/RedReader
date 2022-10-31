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
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRThemeAttributes;
import org.saiditnet.redreader.fragments.CommentListingFragment;
import org.saiditnet.redreader.reddit.RedditCommentListItem;
import org.saiditnet.redreader.reddit.api.RedditAPICommentAction;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;
import org.saiditnet.redreader.reddit.prepared.RedditParsedComment;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableComment;


public class RedditCommentView extends FlingableItemView implements RedditChangeDataManager.Listener{

	private static final Handler HANDLER = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(Message msg) {

			switch(msg.what) {
				case HANDLER_REQUEST_COMMENT_CHANGED: {
					final RedditCommentView rcv = (RedditCommentView) msg.obj;
					rcv.update();
					break;
				}

				default:
					throw new RuntimeException("Unknown message type " + msg.what);
			}
		}
	};

	private static final int HANDLER_REQUEST_COMMENT_CHANGED = 1;

	private RedditCommentListItem mComment;

	private final AppCompatActivity mActivity;
	private final RedditChangeDataManager mChangeDataManager;
	private final RRThemeAttributes mTheme;

	private final TextView mHeader;
	private final FrameLayout mBodyHolder;

	private final IndentView mIndentView;
	private final LinearLayout mIndentedContent;

	private final float mFontScale;

	private final boolean mShowLinkButtons;

	private final CommentListener mListener;

	@Nullable
	private final CommentListingFragment mFragment;

	@Nullable private ActionDescriptionPair mLeftFlingAction;
	@Nullable private ActionDescriptionPair mRightFlingAction;

	@Override
	protected void onSetItemFlingPosition(final float position) {
		mIndentedContent.setTranslationX(position);
	}

	private final class ActionDescriptionPair {
		public final RedditAPICommentAction.RedditCommentAction action;
		public final int descriptionRes;

		private ActionDescriptionPair(RedditAPICommentAction.RedditCommentAction action, int descriptionRes) {
			this.action = action;
			this.descriptionRes = descriptionRes;
		}
	}

	@Nullable
	private ActionDescriptionPair chooseFlingAction(final PrefsUtility.CommentFlingAction pref) {

		if(!mComment.isComment()) {
			return null;
		}

		final RedditParsedComment comment = mComment.asComment().getParsedComment();

		switch(pref) {

			case UPVOTE:
				if(mChangeDataManager.isUpvoted(comment)) {
					// return new ActionDescriptionPair(
					// 		RedditAPICommentAction.RedditCommentAction.UNVOTE,
					// 		R.string.action_vote_remove);
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.UNUPVOTE,
							R.string.action_upvote_remove);
				} else {
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.UPVOTE,
							R.string.action_upvote);
				}

			case DOWNVOTE:
				if(mChangeDataManager.isDownvoted(comment)) {
					// return new ActionDescriptionPair(
					// 		RedditAPICommentAction.RedditCommentAction.UNVOTE,
					// 		R.string.action_vote_remove);
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.UNDOWNVOTE,
							R.string.action_downvote_remove);
				} else {
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.DOWNVOTE,
							R.string.action_downvote);
				}

			case SAVE:
				if(mChangeDataManager.isSaved(comment)) {
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.UNSAVE,
							R.string.action_unsave);
				} else {
					return new ActionDescriptionPair(
							RedditAPICommentAction.RedditCommentAction.SAVE,
							R.string.action_save);
				}

			case REPLY:
				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.REPLY,
						R.string.action_reply);

			case USER_PROFILE:
				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.USER_PROFILE,
						R.string.action_user_profile);

			case COLLAPSE:

				if(mFragment == null) {
					return null;
				}

				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.COLLAPSE,
						R.string.action_collapse);

			case ACTION_MENU:

				if(mFragment == null) {
					return null;
				}

				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.ACTION_MENU,
						R.string.action_actionmenu_short);

			case PROPERTIES:
				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.PROPERTIES,
						R.string.action_properties);

			case BACK:
				return new ActionDescriptionPair(
						RedditAPICommentAction.RedditCommentAction.BACK,
						R.string.action_back);

			case DISABLED:
				return null;
		}

		return null;
	}

	@NonNull
	@Override
	protected String getFlingLeftText() {

		final Context context = getContext();

		final PrefsUtility.CommentFlingAction pref = PrefsUtility.pref_behaviour_fling_comment_left(
				context,
				PreferenceManager.getDefaultSharedPreferences(context));

		mLeftFlingAction = chooseFlingAction(pref);

		if(mLeftFlingAction == null) {
			return "Disabled";
		}

		return context.getString(mLeftFlingAction.descriptionRes);
	}

	@NonNull
	@Override
	protected String getFlingRightText() {

		final Context context = getContext();

		final PrefsUtility.CommentFlingAction pref = PrefsUtility.pref_behaviour_fling_comment_right(
				context,
				PreferenceManager.getDefaultSharedPreferences(context));

		mRightFlingAction = chooseFlingAction(pref);

		if(mRightFlingAction == null) {
			return "Disabled";
		}

		return context.getString(mRightFlingAction.descriptionRes);
	}

	@Override
	protected boolean allowFlingingLeft() {
		return mLeftFlingAction != null;
	}

	@Override
	protected boolean allowFlingingRight() {
		return mRightFlingAction != null;
	}

	@Override
	protected void onFlungLeft() {

		if(mLeftFlingAction == null || !mComment.isComment()) {
			return;
		}

		RedditAPICommentAction.onActionMenuItemSelected(
				mComment.asComment(),
				this,
				mActivity,
				mFragment,
				mLeftFlingAction.action,
				mChangeDataManager);
	}

	@Override
	protected void onFlungRight() {

		if(mRightFlingAction == null || !mComment.isComment()) {
			return;
		}

		RedditAPICommentAction.onActionMenuItemSelected(
				mComment.asComment(),
				this,
				mActivity,
				mFragment,
				mRightFlingAction.action,
				mChangeDataManager);
	}

	public interface CommentListener {
		void onCommentClicked(RedditCommentView view);
		void onCommentLongClicked(RedditCommentView view);
	}

	public RedditCommentView(
			final AppCompatActivity context,
			final RRThemeAttributes themeAttributes,
			final CommentListener listener,
			final CommentListingFragment fragment) {

		super(context);

		mActivity = context;
		mTheme = themeAttributes;
		mListener = listener;
		mFragment = fragment;

		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(context).getDefaultAccount());

		final View rootView = LayoutInflater.from(context).inflate(R.layout.reddit_comment, this, true);

		mIndentView = (IndentView)rootView.findViewById(R.id.view_reddit_comment_indentview);
		mHeader = (TextView)rootView.findViewById(R.id.view_reddit_comment_header);
		mBodyHolder = (FrameLayout)rootView.findViewById(R.id.view_reddit_comment_bodyholder);
		mIndentedContent = (LinearLayout)rootView.findViewById(R.id.view_reddit_comment_indented_content);

		mFontScale = PrefsUtility.appearance_fontscale_comments(context, PreferenceManager.getDefaultSharedPreferences(context));
		mHeader.setTextSize(TypedValue.COMPLEX_UNIT_PX, mHeader.getTextSize() * mFontScale);

		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, PreferenceManager.getDefaultSharedPreferences(context));

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(final View view) {
				mListener.onCommentClicked(RedditCommentView.this);
			}
		});

		setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(final View v) {
				mListener.onCommentLongClicked(RedditCommentView.this);
				return true;
			}
		});
	}

	@Override
	public void onRedditDataChange(final String thingIdAndType) {
		HANDLER.sendMessage(Message.obtain(HANDLER, HANDLER_REQUEST_COMMENT_CHANGED, this));
	}

	private void update() {
		reset(mActivity, mComment, true);
	}

	public void reset(final AppCompatActivity activity, final RedditCommentListItem comment) {
		reset(activity, comment, false);
	}

	public void reset(final AppCompatActivity activity, final RedditCommentListItem comment, final boolean updateOnly) {

		if(!updateOnly) {
			if(!comment.isComment()) {
				throw new RuntimeException("Not a comment");
			}

			if(mComment != comment) {
				if(mComment != null) {
					mChangeDataManager.removeListener(mComment.asComment(), this);
				}

				mChangeDataManager.addListener(comment.asComment(), this);
			}

			mComment = comment;

			resetSwipeState();
		}

		mIndentView.setIndentation(comment.getIndent());

		final boolean hideLinkButtons = comment.asComment().getParsedComment().getRawComment().author.equalsIgnoreCase("autowikibot");

		mBodyHolder.removeAllViews();
		final View commentBody = comment.asComment().getBody(
				activity,
				mTheme.rrCommentBodyCol,
				13.0f * mFontScale,
				mShowLinkButtons && !hideLinkButtons);

		mBodyHolder.addView(commentBody);
		commentBody.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		((MarginLayoutParams)commentBody.getLayoutParams()).topMargin = General.dpToPixels(activity, 1);

		final RedditRenderableComment renderableComment = mComment.asComment();

		final CharSequence headerText = renderableComment.getHeader(
				mTheme,
				mChangeDataManager,
				activity);

		if(mComment.isCollapsed(mChangeDataManager)) {
			setFlingingEnabled(false);
			mHeader.setText("[ + ]  " + headerText); // Note that this removes formatting (which is fine)
			mBodyHolder.setVisibility(GONE);

		} else {
			setFlingingEnabled(true);
			mHeader.setText(headerText);
			mBodyHolder.setVisibility(VISIBLE);
		}
	}

	public RedditCommentListItem getComment() {
		return mComment;
	}
}
