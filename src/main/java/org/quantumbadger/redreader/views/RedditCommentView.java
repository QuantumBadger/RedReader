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
import android.graphics.Color;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;

public class RedditCommentView extends LinearLayout
		implements RedditChangeDataManager.Listener {

	private static final Handler HANDLER = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(Message msg) {

			switch(msg.what) {
				case HANDLER_REQUEST_COMMENT_CHANGED: {
					final RedditCommentView rcv = (RedditCommentView) msg.obj;
					rcv.mListener.onCommentChanged(rcv);
					break;
				}

				default:
					throw new RuntimeException("Unknown message type " + msg.what);
			}
		}
	};

	private static final int HANDLER_REQUEST_COMMENT_CHANGED = 1;

	private RedditCommentListItem mComment;

	private final RedditChangeDataManager mChangeDataManager;
	private final RRThemeAttributes mTheme;

	private final TextView mHeader;
	private final FrameLayout mBodyHolder;

	private final IndentView mIndentView;

	private final float mFontScale;

	private final boolean mShowLinkButtons;

	private final CommentListener mListener;

	public interface CommentListener {
		void onCommentClicked(RedditCommentView view);

		void onCommentLongClicked(RedditCommentView view);

		void onCommentChanged(RedditCommentView view);
	}

	public RedditCommentView(
			final Context context,
			final RRThemeAttributes themeAttributes,
			final CommentListener listener) {

		super(context);

		mTheme = themeAttributes;
		mListener = listener;

		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(context).getDefaultAccount());

		mFontScale = PrefsUtility.appearance_fontscale_comments(context, PreferenceManager.getDefaultSharedPreferences(context));

		mHeader = new TextView(context);
		mHeader.setTextSize(11.0f * mFontScale);
		mHeader.setTextColor(mTheme.rrCommentHeaderCol);

		mBodyHolder = new FrameLayout(context);
		mBodyHolder.setPadding(0, General.dpToPixels(context, 2), 0, 0);

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

		setLongClickable(true);

		mIndentView = new IndentView(context);

		final LinearLayout main = new LinearLayout(context);
		main.setOrientation(VERTICAL);
		main.addView(mHeader);
		main.addView(mBodyHolder);

		mBodyHolder.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final int paddingPixelsVertical = General.dpToPixels(context, 8.0f);
		final int paddingPixelsHorizontal = General.dpToPixels(context, 12.0f);
		main.setPadding(paddingPixelsHorizontal, paddingPixelsVertical, paddingPixelsHorizontal, paddingPixelsVertical);

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(HORIZONTAL);
		outer.addView(mIndentView);
		outer.addView(main);

		mIndentView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		main.getLayoutParams().width = LinearLayout.LayoutParams.MATCH_PARENT;

		final View divider = new View(context);
		divider.setBackgroundColor(Color.argb(128, 128, 128, 128)); // TODO better

		setOrientation(VERTICAL);
		addView(divider);
		addView(outer);

		divider.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		divider.getLayoutParams().height = 1;
	}

	@Override
	public void onRedditDataChange(final String thingIdAndType) {
		HANDLER.sendMessage(Message.obtain(HANDLER, HANDLER_REQUEST_COMMENT_CHANGED, this));
	}

	public void notifyClick() {
		mListener.onCommentClicked(this);
	}

	public void notifyLongClick() {
		mListener.onCommentLongClicked(this);
	}

	public void reset(final AppCompatActivity activity, final RedditCommentListItem comment) {

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
			mHeader.setText("[ + ]  " + headerText); // Note that this removes formatting (which is fine)
			mBodyHolder.setVisibility(GONE);

		} else {
			mHeader.setText(headerText);
			mBodyHolder.setVisibility(VISIBLE);
		}
	}

	public RedditCommentListItem getComment() {
		return mComment;
	}
}
