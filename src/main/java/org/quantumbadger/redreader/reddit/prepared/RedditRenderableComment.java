package org.quantumbadger.redreader.reddit.prepared;

import android.content.Context;
import android.graphics.Color;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.reddit.api.RedditAPICommentAction;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType;

import java.net.URI;
import java.util.List;

public class RedditRenderableComment implements RedditRenderableInboxItem, RedditThingWithIdAndType {

	private final RedditParsedComment mComment;
	private final String mParentPostAuthor;
	private final Integer mMinimumCommentScore;
	private final boolean mShowScore;
	private final List<String> mToggledComments;

	public RedditRenderableComment(
			final RedditParsedComment comment,
			final String parentPostAuthor,
			final Integer minimumCommentScore,
			final boolean showScore,
			final List<String> toggledComments) {

		mComment = comment;
		mParentPostAuthor = parentPostAuthor;
		mMinimumCommentScore = minimumCommentScore;
		mShowScore = showScore;
		mToggledComments = toggledComments;
	}

	private int computeScore(final RedditChangeDataManagerVolatile changeDataManager) {

		final RedditComment rawComment = mComment.getRawComment();

		int score = rawComment.ups - rawComment.downs;

		if(Boolean.TRUE.equals(rawComment.likes)) score--;
		if(Boolean.FALSE.equals(rawComment.likes)) score++;

		if(changeDataManager.isUpvoted(mComment)) {
			score++;
		} else if(changeDataManager.isDownvoted(mComment)) {
			score--;
		}

		return score;
	}

	@Override
	public CharSequence getHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManagerVolatile changeDataManager,
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
			if(mParentPostAuthor != null
					&& rawComment.author.equalsIgnoreCase(mParentPostAuthor)
					&& !rawComment.author.equals("[deleted]")) {
				sb.append(" " + rawComment.author + " ", BetterSSB.BACKGROUND_COLOR | BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						Color.WHITE, Color.rgb(0, 126, 168), 1f); // TODO color
			} else {
				sb.append(rawComment.author, BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, theme.rrCommentHeaderAuthorCol, 0, 1f);
			}
		}

		final String flair = mComment.getFlair();

		if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.FLAIR)
				&& flair != null && flair.length() > 0) {

			if(theme.shouldShow(PrefsUtility.AppearanceCommentHeaderItem.AUTHOR)) {
				sb.append("  ", 0);
			}

			sb.append(
					" " + flair + " ",
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

		final RedditChangeDataManagerVolatile changeDataManager
				= RedditChangeDataManagerVolatile.getInstance(
						RedditAccountManager.getInstance(activity).getDefaultAccount());

		RedditAPICommentAction.showActionMenu(
				activity,
				null,
				this,
				null,
				changeDataManager);
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

	private boolean isScoreBelowThreshold(final RedditChangeDataManagerVolatile changeDataManager) {

		if(mMinimumCommentScore == null) {
			return false;
		}

		if(Boolean.TRUE.equals(mComment.getRawComment().score_hidden)) {
			return false;
		}

		return (computeScore(changeDataManager) < mMinimumCommentScore);
	}

	public boolean isCollapsed(final RedditChangeDataManagerVolatile changeDataManager) {

		final Boolean collapsed = changeDataManager.isHidden(this);

		if(collapsed != null) {
			return collapsed;
		}

		if (mToggledComments.contains(this.getIdAlone())) {
			return true;
		}

		return isScoreBelowThreshold(changeDataManager);

	}
}
