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
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Typeface;
import android.text.SpannableStringBuilder;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.activities.InboxListingActivity;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.ScreenreaderPronunciation;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;
import org.quantumbadger.redreader.reddit.things.RedditMessage;

public final class RedditPreparedMessage implements RedditRenderableInboxItem {

	public final SpannableStringBuilder header;
	public final BodyElement body;
	public final String idAndType;
	public final RedditMessage src;
	public final InboxListingActivity.InboxType inboxType;

	public RedditPreparedMessage(
			@NonNull final AppCompatActivity activity,
			@NonNull final RedditMessage message,
			final long timestamp,
			final InboxListingActivity.InboxType inboxType) {

		final Context applicationContext = activity.getApplicationContext();

		this.src = message;
		this.inboxType = inboxType;

		// TODO respect RRTheme

		final int rrCommentHeaderBoldCol;
		final int rrCommentHeaderAuthorCol;

		{
			final TypedArray appearance = activity.obtainStyledAttributes(new int[] {
					R.attr.rrCommentHeaderBoldCol,
					R.attr.rrCommentHeaderAuthorCol,
			});

			rrCommentHeaderBoldCol = appearance.getColor(0, 255);
			rrCommentHeaderAuthorCol = appearance.getColor(1, 255);

			appearance.recycle();
		}

		body = HtmlReader.parse(message.getUnescapedBodyHtml(), activity);

		idAndType = message.name;

		final BetterSSB sb = new BetterSSB();

		if(inboxType == InboxListingActivity.InboxType.SENT) {
			sb.append(applicationContext.getString(R.string.subtitle_to) + " ", 0);

			if(src.dest == null) {
				sb.append(
						"[" + applicationContext.getString(R.string.general_unknown) + "]",
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						rrCommentHeaderAuthorCol,
						0,
						1f);
			} else {
				sb.append(
						src.dest,
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						rrCommentHeaderAuthorCol,
						0,
						1f);
			}
		} else {

			final String author = General.nullAlternative(
					src.author,
					src.subreddit_name_prefixed,
					"[" + applicationContext.getString(R.string.general_unknown) + "]");

			sb.append(
					author,
					BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
					rrCommentHeaderAuthorCol,
					0,
					1f);
		}

		sb.append("   ", 0);
		sb.append(
				RRTime.formatDurationFrom(
						applicationContext,
						src.created_utc * 1000L,
						R.string.time_ago,
						PrefsUtility.appearance_inbox_age_units()),
				BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
				rrCommentHeaderBoldCol,
				0,
				1f);

		header = sb.get();
	}

	public SpannableStringBuilder getHeader() {
		return header;
	}

	private void openReplyActivity(final AppCompatActivity activity) {

		final Intent intent = new Intent(activity, CommentReplyActivity.class);
		intent.putExtra(CommentReplyActivity.PARENT_ID_AND_TYPE_KEY, idAndType);
		intent.putExtra(
				CommentReplyActivity.PARENT_MARKDOWN_KEY,
				src.getUnescapedBodyMarkdown());
		intent.putExtra(
				CommentReplyActivity.PARENT_TYPE,
				CommentReplyActivity.PARENT_TYPE_MESSAGE);
		activity.startActivity(intent);
	}

	@Override
	public void handleInboxClick(final BaseActivity activity) {

		if(src.author == null) {
			return;
		}

		final String currentCanonicalUserName = RedditAccountManager.getInstance(activity)
				.getDefaultAccount().getCanonicalUsername();

		if(!StringUtils.asciiLowercase(src.author.trim()).equals(currentCanonicalUserName)) {
			openReplyActivity(activity);
		}
	}

	@Override
	public void handleInboxLongClick(final BaseActivity activity) {
		handleInboxClick(activity);
	}

	@Override
	public CharSequence getHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManager changeDataManager,
			final Context context,
			final int commentAgeUnits,
			final long postCreated,
			final long parentCommentCreated) {
		return header;
	}

	@Override
	public String getAccessibilityHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManager changeDataManager,
			final Context context,
			final int commentAgeUnits,
			final long postCreated,
			final long parentCommentCreated,
			final boolean collapsed,
			@NonNull final Optional<Integer> indentLevel) {

		final StringBuilder accessibilityHeader = new StringBuilder();
		final String separator = " \n";

		if(inboxType == InboxListingActivity.InboxType.SENT && src.dest != null) {
			accessibilityHeader
					.append(context.getString(
							R.string.accessibility_subtitle_recipient_withperiod,
							ScreenreaderPronunciation.getPronunciation(
									context,
									src.dest)))
					.append(separator);
		} else if(src.author != null) {
			accessibilityHeader
					.append(context.getString(
							PrefsUtility.pref_accessibility_concise_mode()
									?R.string.accessibility_subtitle_author_withperiod_concise_post
									: R.string.accessibility_subtitle_author_withperiod,
							ScreenreaderPronunciation.getPronunciation(
									context,
									src.author)))
					.append(separator);
		}

		accessibilityHeader
				.append(context.getString(
						R.string.accessibility_subtitle_age_withperiod,
						RRTime.formatDurationFrom(
								context,
								src.created_utc * 1000L,
								R.string.time_ago,
								PrefsUtility.appearance_inbox_age_units())))
				.append(separator);

		return accessibilityHeader.toString();
	}

	@Override
	public View getBody(
			final BaseActivity activity,
			final Integer textColor,
			final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout subjectLayout = new LinearLayout(activity);
		subjectLayout.setOrientation(LinearLayout.VERTICAL);

		final TextView subjectText = new TextView(activity);
		subjectText.setText(StringEscapeUtils.unescapeHtml4(src.subject != null
				? src.subject
				: "(no subject)"));
		subjectText.setTextColor(textColor);
		subjectText.setTextSize(textSize);
		subjectText.setTypeface(null, Typeface.BOLD);

		subjectLayout.addView(subjectText);
		subjectLayout.addView(body.generateView(
				activity,
				textColor,
				textSize,
				showLinkButtons));

		return subjectLayout;
	}
}
