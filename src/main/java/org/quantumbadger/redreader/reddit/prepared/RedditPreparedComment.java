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

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.text.SpannableStringBuilder;
import android.view.ViewGroup;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.RedditPreparedInboxItem;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParagraphGroup;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParser;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.views.RedditCommentView;

import java.net.URI;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedList;

public final class RedditPreparedComment implements RedditPreparedInboxItem {

	public SpannableStringBuilder header;

	private final MarkdownParagraphGroup body;

	private final LinkedList<RedditPreparedComment> directReplies = new LinkedList<RedditPreparedComment>();

	private boolean collapsed = false;

	public final String idAlone, idAndType, flair;

	private int voteDirection;
	private boolean saved;
	public long lastChange;
	public final RedditComment src;

	private RedditCommentView boundView;

	private final int
			rrCommentHeaderBoldCol,
			rrCommentHeaderAuthorCol,
			rrPostSubtitleUpvoteCol,
			rrPostSubtitleDownvoteCol,
			rrFlairBackCol,
			rrFlairTextCol,
			rrGoldBackCol,
			rrGoldTextCol;
	private final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems;

	private final RedditPreparedPost parentPost;

	public RedditPreparedComment(final Context context,
								 final RedditComment comment,
								 final long timestamp,
								 final boolean needsUpdating,
								 final RedditPreparedPost parentPost,
								 final RedditAccount user,
								 final EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems) {

		this.src = comment;
		this.parentPost = parentPost;
		this.headerItems = headerItems;

		// TODO custom time

		// TODO don't fetch these every time
		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
				R.attr.rrCommentHeaderBoldCol,
				R.attr.rrCommentHeaderAuthorCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol,
				R.attr.rrFlairBackCol,
				R.attr.rrFlairTextCol,
				R.attr.rrGoldBackCol,
				R.attr.rrGoldTextCol
		});

		rrCommentHeaderBoldCol = appearance.getColor(0, 255);
		rrCommentHeaderAuthorCol = appearance.getColor(1, 255);
		rrPostSubtitleUpvoteCol = appearance.getColor(2, 255);
		rrPostSubtitleDownvoteCol = appearance.getColor(3, 255);
		rrFlairBackCol = appearance.getColor(4, 0);
		rrFlairTextCol = appearance.getColor(5, 255);
		rrGoldBackCol = appearance.getColor(6, 0);
		rrGoldTextCol = appearance.getColor(7, 255);

		body = MarkdownParser.parse(StringEscapeUtils.unescapeHtml4(comment.body).toCharArray());
		if(comment.author_flair_text != null) {
			flair = StringEscapeUtils.unescapeHtml4(comment.author_flair_text);
		} else {
			flair = null;
		}

		idAlone = comment.id;
		idAndType = comment.name;

		if(comment.likes == null) {
			voteDirection = 0;
		} else {
			voteDirection = Boolean.TRUE.equals(comment.likes) ? 1 : -1;
		}

		saved = Boolean.TRUE.equals(comment.saved);

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
			if(parentPost != null
					&& src.author.equalsIgnoreCase(parentPost.src.author)
					&& !src.author.equals("[deleted]")) {
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

			sb.append(" " + flair + " ", BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR, rrFlairTextCol, rrFlairBackCol, 1f);
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

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.GOLD)) {

			if(src.gilded > 0) {

				sb.append(" ", 0);

				sb.append(" "
								+ context.getString(R.string.gold)
								+ " x"
								+ src.gilded
								+ " ",
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
						rrGoldTextCol,
						rrGoldBackCol,
						1f);

				sb.append("  ", 0);
			}
		}

		if(headerItems.contains(PrefsUtility.AppearanceCommentHeaderItems.AGE)) {
			sb.append(RRTime.formatDurationFrom(context, src.created_utc * 1000L), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderBoldCol, 0, 1f);

			if(src.edited != null && src.edited instanceof Long) {
				sb.append("*", BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderBoldCol, 0, 1f);
			}
		}

		header = sb.get();
	}

	public void bind(RedditCommentView view) {
		boundView = view;
	}

	public void unbind(RedditCommentView view) {
		if(boundView == view) boundView = null;
	}

	public void addChild(final RedditPreparedComment child) {
		directReplies.add(child);
	}

	public void toggleVisibility() {
		collapsed = !collapsed;
	}

	public boolean isCollapsed() {
		return collapsed;
	}

	public void refreshView(final Context context) {
		General.UI_THREAD_HANDLER.post(new Runnable() {
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
			case DOWNVOTE:
				if(!src.archived) {
					voteDirection = -1;
				}
				break;
			case UNVOTE:
				if(!src.archived) {
					voteDirection = 0;
				}
				break;
			case UPVOTE:
				if(!src.archived) {
					voteDirection = 1;
				}
				break;
			case SAVE: saved = true; break;
			case UNSAVE: saved = false; break;
		}

		refreshView(activity);

		boolean vote = (action == RedditAPI.RedditAction.DOWNVOTE
				| action == RedditAPI.RedditAction.UPVOTE
				| action == RedditAPI.RedditAction.UNVOTE);

		if(src.archived && vote){
			Toast.makeText(activity, R.string.error_archived_vote, Toast.LENGTH_SHORT)
					.show();
			return;
		}

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

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status, null);
						General.UI_THREAD_HANDLER.post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						General.UI_THREAD_HANDLER.post(new Runnable() {
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
							case SAVE:
								saved = false; break;
							case UNSAVE:
								saved = true; break;
						}

						refreshView(context);
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

	public void updateFromChangeDb(final long timestamp, final int voteDirection, final boolean saved) {
		this.lastChange = timestamp;
		this.voteDirection = voteDirection;
		this.saved = saved;
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

	public ViewGroup getBody(Activity activity, float textSize, Integer textCol, boolean showLinkButtons) {
		return body.buildView(activity, textCol, textSize, showLinkButtons);
	}

	public RedditCommentView getBoundView() {
		return boundView;
	}

	public void handleInboxClick(Activity activity) {
		final URI commentContext = Constants.Reddit.getUri(src.context);
		LinkHandler.onLinkClicked(activity, commentContext.toString());
	}

	public boolean isSaved() {
		return saved;
	}
}
