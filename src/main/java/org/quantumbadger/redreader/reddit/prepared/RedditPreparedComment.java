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

package org.quantumbadger.redreader.reddit.prepared;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.os.Handler;
import android.os.Looper;
import android.text.SpannableStringBuilder;
import android.view.ViewGroup;
import com.laurencedawson.activetextview.ActiveTextView;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.Hideable;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.RedditPreparedInboxItem;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.views.RedditCommentView;

import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedList;

public final class RedditPreparedComment implements Hideable, RedditPreparedInboxItem {

	public SpannableStringBuilder header;

	private final RedditCommentTextParser.ViewGenerator body;

	public final int indentation;
	private final LinkedList<RedditPreparedComment> directReplies = new LinkedList<RedditPreparedComment>();

	private boolean collapsed = false, collapsedByParent = false;

	public final String idAlone, idAndType, flair;

	private int voteDirection;
	public long lastChange;
	public final RedditComment src;

	private RedditCommentView boundView;

	private final int rrCommentHeaderBoldCol, rrCommentHeaderAuthorCol, rrPostSubtitleUpvoteCol, rrPostSubtitleDownvoteCol;
	private final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems;

	private final RedditPreparedPost parentPost;

	public RedditPreparedComment(final Context context, final RedditComment comment, final RedditPreparedComment parentComment,
								 final long timestamp, final boolean needsUpdating, final RedditPreparedPost parentPost,
								 final RedditAccount user, final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems) {

		this.src = comment;
		this.parentPost = parentPost;
		this.headerItems = headerItems;

		// TODO custom time

		// TODO don't fetch these every time
		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
				R.attr.rrCommentHeaderBoldCol,
				R.attr.rrCommentHeaderAuthorCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol
		});

		rrCommentHeaderBoldCol = appearance.getColor(0, 255);
		rrCommentHeaderAuthorCol = appearance.getColor(1, 255);
		rrPostSubtitleUpvoteCol = appearance.getColor(2, 255);
		rrPostSubtitleDownvoteCol = appearance.getColor(3, 255);

		body = RedditCommentTextParser.parse(StringEscapeUtils.unescapeHtml4(comment.body));
		if(comment.author_flair_text != null) {
			flair = StringEscapeUtils.unescapeHtml4(comment.author_flair_text);
		} else {
			flair = null;
		}

		if(parentComment == null) {
			indentation = 0;
		} else {
			indentation = parentComment.indentation + 1;
			parentComment.addChild(this);
		}

		idAlone = comment.id;
		idAndType = comment.name;

		if(comment.likes == null) {
			voteDirection = 0;
		} else {
			voteDirection = Boolean.TRUE.equals(comment.likes) ? 1 : -1;
		}

		lastChange = timestamp;
		if(src.likes != null) {
			RedditChangeDataManager.getInstance(context).update(src.link_id, user, this, true);
		} else if(needsUpdating) {
			RedditChangeDataManager.getInstance(context).update(src.link_id, user, this, false);
		}

		rebuildHeader(context);
	}

	private void rebuildHeader(final Context context) {

		final BetterSSB sb = new BetterSSB();

		final int pointsCol;
		int score = src.ups - src.downs;

		if(Boolean.TRUE.equals(src.likes)) score--;
		if(Boolean.FALSE.equals(src.likes)) score++;

		if(isUpvoted()) {
			pointsCol = rrPostSubtitleUpvoteCol;
			score++;
		} else if(isDownvoted()) {
			pointsCol = rrPostSubtitleDownvoteCol;
			score--;
		} else {
			pointsCol = rrCommentHeaderBoldCol;
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.AUTHOR)) {
			if(parentPost != null && src.author.equalsIgnoreCase(parentPost.src.author)) {
				sb.append(" " + src.author + " ", BetterSSB.BACKGROUND_COLOR | BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						Color.WHITE, Color.rgb(0, 126, 168), 1f); // TODO color
			} else {
				sb.append(src.author, BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderAuthorCol, 0, 1f);
			}
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.FLAIR)
				&& flair != null && flair.length() > 0) {

			if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.AUTHOR)) {
				sb.append("  ", 0);
			}

			sb.append(" " + flair + " ", BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR, Color.rgb(30, 30, 30), Color.rgb(230, 230, 230), 1f); // TODO theme properly
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.AUTHOR)
				|| headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.FLAIR)) {
			sb.append("   ", 0);
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.SCORE)) {

			if(!Boolean.TRUE.equals(src.score_hidden)) {
				sb.append(String.valueOf(score), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, pointsCol, 0, 1f);
			} else {
				sb.append("??", BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, pointsCol, 0, 1f);
			}

			sb.append(" " + context.getString(R.string.subtitle_points) +  " ", 0);
		}

		if(!Boolean.TRUE.equals(src.score_hidden) && headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.UPS_DOWNS)) {
			sb.append("(", 0);
			sb.append(String.valueOf(src.ups), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrPostSubtitleUpvoteCol, 0, 1f);
			sb.append(" | ", 0);
			sb.append(String.valueOf(src.downs), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrPostSubtitleDownvoteCol, 0, 1f);
			sb.append(")  ", 0);
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.AGE)) {
			sb.append(RRTime.formatDurationMsAgo(context, RRTime.utcCurrentTimeMillis() - src.created_utc * 1000L), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderBoldCol, 0, 1f);
		}

		header = sb.get();
	}

	public void bind(RedditCommentView view) {
		boundView = view;
	}

	public void unbind(RedditCommentView view) {
		if(boundView == view) boundView = null;
	}

	private void addChild(final RedditPreparedComment child) {
		directReplies.add(child);
	}

	public void toggleVisibility() {

		collapsed = !collapsed;

		for(final RedditPreparedComment v : directReplies) {
			v.onParentVisibilityChanged(collapsed);
		}
	}

	private void onParentVisibilityChanged(final boolean parentCollapsed) {

		collapsedByParent = parentCollapsed;

		if(!collapsed) {
			for(final RedditPreparedComment v : directReplies) {
				v.onParentVisibilityChanged(parentCollapsed);
			}
		}
	}

	public boolean isVisible() {
		return !collapsedByParent;
	}

	public boolean isCollapsed() {
		return collapsed;
	}

	public void refreshView(final Context context) {
		new Handler(Looper.getMainLooper()).post(new Runnable() {
			public void run() {
				rebuildHeader(context);
				if(boundView != null) {
					boundView.updateAppearance();
					boundView.requestLayout();
					boundView.invalidate();
				}
			}
		});
	}

	public void action(final Activity activity, final RedditAPI.RedditAction action) {

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(user.isAnonymous()) {
			General.quickToast(activity, "You must be logged in to do that.");
			return;
		}

		final int lastVoteDirection = voteDirection;

		switch(action) {
			case DOWNVOTE: voteDirection = -1; break;
			case UNVOTE: voteDirection = 0; break;
			case UPVOTE: voteDirection = 1; break;
		}

		refreshView(activity);

		RedditAPI.action(CacheManager.getInstance(activity),
				new APIResponseHandler.ActionResponseHandler(activity) {
					@Override
					protected void onCallbackException(final Throwable t) {
						throw new RuntimeException(t);
					}

					@Override
					protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
						revertOnFailure();
						if(t != null) t.printStackTrace();

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status);
						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onSuccess() {
						lastChange = RRTime.utcCurrentTimeMillis();
						RedditChangeDataManager.getInstance(context).update(src.link_id, user, RedditPreparedComment.this, true);
						refreshView(activity);
					}

					private void revertOnFailure() {

						switch(action) {
							case DOWNVOTE:
							case UNVOTE:
							case UPVOTE:
								voteDirection = lastVoteDirection; break;
						}
					}

				}, user, idAndType, action, activity);
	}

	public int getVoteDirection() {
		return voteDirection;
	}

	public boolean isUpvoted() {
		return voteDirection == 1;
	}

	public boolean isDownvoted() {
		return voteDirection == -1;
	}

	public void updateFromChangeDb(final long timestamp, final int voteDirection) {
		this.lastChange = timestamp;
		this.voteDirection = voteDirection;
	}

	public int replyCount() {
		return directReplies.size();
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof RedditPreparedComment
				&& (o == this || ((RedditPreparedComment) o).idAlone.equals(idAlone));
	}

	public HashSet<String> computeAllLinks() {
		return LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(src.body_html));
	}

	public SpannableStringBuilder getHeader() {
		return header;
	}

	public ViewGroup getBody(Context context, float textSize, Integer textCol, ActiveTextView.OnLinkClickedListener listener) {
		return body.generate(context, textSize, textCol, listener, this);
	}

	public RedditCommentView getBoundView() {
		return boundView;
	}
}
