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

package org.saiditnet.redreader.reddit.prepared;

import android.content.Context;
import android.graphics.Color;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.common.BetterSSB;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRThemeAttributes;
import org.saiditnet.redreader.common.RRTime;
import org.saiditnet.redreader.reddit.api.RedditAPICommentAction;
import org.saiditnet.redreader.reddit.things.RedditComment;
import org.saiditnet.redreader.reddit.things.RedditThingWithIdAndType;

import java.net.URI;

public class RedditRenderableComment implements RedditRenderableInboxItem, RedditThingWithIdAndType {

	private final RedditParsedComment mComment;
	private final String mParentPostAuthor;
	private final Integer mMinimumCommentScore;
	private final boolean mShowScore;

	public RedditRenderableComment(
			final RedditParsedComment comment,
			final String parentPostAuthor,
			final Integer minimumCommentScore,
			final boolean showScore) {

		mComment = comment;
		mParentPostAuthor = parentPostAuthor;
		mMinimumCommentScore = minimumCommentScore;
		mShowScore = showScore;
	}

	private int computeScore(final RedditChangeDataManager changeDataManager) {

		final RedditComment rawComment = mComment.getRawComment();

		int score = rawComment.ups - rawComment.downs;

		// if(Boolean.TRUE.equals(rawComment.likes)) score--;
		// if(Boolean.FALSE.equals(rawComment.likes)) score++;
		if(Boolean.TRUE.equals(rawComment.likes)) score = score - 2;
		if(Boolean.TRUE.equals(rawComment.dislikes)) score--;

		// if(changeDataManager.isUpvoted(mComment)) {
		// 	score++;
		// } else if(changeDataManager.isDownvoted(mComment)) {
		// 	score--;
		// }
		if(changeDataManager.isUpvoted(mComment)) {
			score = score + 2;
		}
		if(changeDataManager.isDownvoted(mComment)) {
			score++;
		}

		return score;
	}

	@Override
	public CharSequence getHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManager changeDataManager,
			final Context context) {

		final BetterSSB sb = new BetterSSB();

		final RedditComment rawComment = mComment.getRawComment();

		final int pointsCol;
		final int score = computeScore(changeDataManager);

		if(changeDataManager.isUpvoted(mComment)) {
			pointsCol = theme.rrPostSubtitleUpvoteCol;

		} else if(changeDataManager.isDownvoted(mComment)) {
			pointsCol = theme.rrPostSubtitleDownvoteCol;

		} else {
			pointsCol = theme.rrCommentHeaderBoldCol;
		}

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.AUTHOR)) {

			boolean setBackgroundColour = false;
			int backgroundColour = 0; // TODO color from theme

			if(mParentPostAuthor != null
					&& rawComment.author.equalsIgnoreCase(mParentPostAuthor)
					&& !rawComment.author.equals("[deleted]")) {

				setBackgroundColour = true;
				backgroundColour = Color.rgb(0, 126, 168);

			} else if("moderator".equals(rawComment.distinguished)) {
				setBackgroundColour = true;
				backgroundColour = Color.rgb(0, 170, 0);

			} else if("admin".equals(rawComment.distinguished)) {
				setBackgroundColour = true;
				backgroundColour = Color.rgb(170, 0, 0);
			}

			if(setBackgroundColour) {

				sb.append(
						" " + rawComment.author + " ",
						BetterSSB.BACKGROUND_COLOR | BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						Color.WHITE,
						backgroundColour,
						1f);

			} else {
				sb.append(
						rawComment.author,
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						theme.rrCommentHeaderAuthorCol,
						0,
						1f);
			}
		}

		final String flair = mComment.getFlair();

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.FLAIR)
				&& flair != null && flair.length() > 0) {

			if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.AUTHOR)) {
				sb.append("  ", 0);
			}

			sb.append(
					" " + flair + "\u200E ",
					BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					theme.rrFlairTextCol,
					theme.rrFlairBackCol,
					1f);
		}

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.AUTHOR)
				|| theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.FLAIR)) {
			sb.append("   ", 0);
		}

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.SCORE) && mShowScore) {

			if(!Boolean.TRUE.equals(rawComment.score_hidden)) {
				sb.append(String.valueOf(score), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, pointsCol, 0, 1f);
			} else {
				sb.append("??", BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, pointsCol, 0, 1f);
			}

			sb.append(" " + context.getString(R.string.subtitle_points) +  " ", 0);
		}

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.GOLD)) {

			if(rawComment.gilded > 0) {

				sb.append(" ", 0);

				sb.append(" "
								+ context.getString(R.string.gold)
								+ " x"
								+ rawComment.gilded
								+ " ",
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
						theme.rrGoldTextCol,
						theme.rrGoldBackCol,
						1f);

				sb.append("  ", 0);
			}
		}

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.AGE)) {
			sb.append(
					RRTime.formatDurationFrom(context, rawComment.created_utc * 1000L),
					BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
					theme.rrCommentHeaderBoldCol,
					0,
					1f);

			if(rawComment.edited != null && rawComment.edited instanceof Long) {
				sb.append("*", BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, theme.rrCommentHeaderBoldCol, 0, 1f);
			}
		}

		return sb.get();
	}

	@Override
	public View getBody(
			final AppCompatActivity activity,
			final Integer textColor,
			final Float textSize,
			final boolean showLinkButtons) {

		return mComment.getBody().buildView(activity, textColor, textSize, showLinkButtons);
	}

	@Override
	public void handleInboxClick(final AppCompatActivity activity) {
		final URI commentContext = Constants.Reddit.getUri(mComment.getRawComment().context);
		LinkHandler.onLinkClicked(activity, commentContext.toString());
	}

	@Override
	public void handleInboxLongClick(final AppCompatActivity activity) {

		final RedditChangeDataManager changeDataManager
				= RedditChangeDataManager.getInstance(
						RedditAccountManager.getInstance(activity).getDefaultAccount());

		RedditAPICommentAction.showActionMenu(
				activity,
				null,
				this,
				null,
				changeDataManager,
				// TODO instead of assuming that it isn't an archived post, somehow find out if it actually is
				false);
	}

	@Override
	public String getIdAlone() {
		return mComment.getIdAlone();
	}

	@Override
	public String getIdAndType() {
		return mComment.getIdAndType();
	}

	public RedditParsedComment getParsedComment() {
		return mComment;
	}

	private boolean isScoreBelowThreshold(final RedditChangeDataManager changeDataManager) {

		if(mMinimumCommentScore == null) {
			return false;
		}

		if(Boolean.TRUE.equals(mComment.getRawComment().score_hidden)) {
			return false;
		}

		return (computeScore(changeDataManager) < mMinimumCommentScore);
	}

	public boolean isCollapsed(final RedditChangeDataManager changeDataManager) {

		final Boolean collapsed = changeDataManager.isHidden(this);

		if(collapsed != null) {
			return collapsed;
		}

		return isScoreBelowThreshold(changeDataManager);

	}
}
