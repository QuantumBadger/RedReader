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
import androidx.annotation.Nullable;
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
import org.quantumbadger.redreader.common.ScreenreaderPronunciation;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.time.TimeFormatHelper;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.kthings.RedditMessage;
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;

public final class RedditPreparedMessage implements RedditRenderableInboxItem {

	public final SpannableStringBuilder header;
	public final BodyElement body;
	public final RedditIdAndType idAndType;
	public final RedditMessage src;
	public final InboxListingActivity.InboxType inboxType;

	public RedditPreparedMessage(
			@NonNull final AppCompatActivity activity,
			@NonNull final RedditMessage message,
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

		body = HtmlReader.parse(message.getBody_html().getDecoded(), activity);

		idAndType = message.getIdAndType();

		final BetterSSB sb = new BetterSSB();

		if(inboxType == InboxListingActivity.InboxType.SENT) {
			sb.append(applicationContext.getString(R.string.subtitle_to) + " ", 0);

			if(src.getDest() == null) {
				sb.append(
						"[" + applicationContext.getString(R.string.general_unknown) + "]",
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						rrCommentHeaderAuthorCol,
						0,
						1f);
			} else {
				sb.append(
						src.getDest().getDecoded(),
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD,
						rrCommentHeaderAuthorCol,
						0,
						1f);
			}
		} else {

			final String author = General.nullAlternative(
					General.mapIfNotNull(
							src.getAuthor(),
							UrlEncodedString::getDecoded),
					General.mapIfNotNull(
							src.getSubreddit_name_prefixed(),
							UrlEncodedString::getDecoded),
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
				TimeFormatHelper.format(
						src.getCreated_utc().getValue().elapsedPeriod(),
						applicationContext,
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
				General.mapIfNotNull(src.getBody_html(), UrlEncodedString::getDecoded));
		intent.putExtra(
				CommentReplyActivity.PARENT_TYPE,
				CommentReplyActivity.PARENT_TYPE_MESSAGE);
		activity.startActivity(intent);
	}

	@Override
	public void handleInboxClick(final BaseActivity activity) {

		if(src.getAuthor() == null) {
			return;
		}

		final String currentCanonicalUserName = RedditAccountManager.getInstance(activity)
				.getDefaultAccount().getCanonicalUsername();

		if(!StringUtils.asciiLowercase(src.getAuthor().getDecoded().trim())
				.equals(currentCanonicalUserName)) {
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
			@Nullable final TimestampUTC postCreated,
			@Nullable final TimestampUTC parentCommentCreated) {
		return header;
	}

	@Override
	public String getAccessibilityHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManager changeDataManager,
			final Context context,
			final int commentAgeUnits,
			@Nullable final TimestampUTC postCreated,
			@Nullable final TimestampUTC parentCommentCreated,
			final boolean collapsed,
			@NonNull final Optional<Integer> indentLevel) {

		final StringBuilder accessibilityHeader = new StringBuilder();
		final String separator = " \n";

		if(inboxType == InboxListingActivity.InboxType.SENT && src.getDest() != null) {
			accessibilityHeader
					.append(context.getString(
							R.string.accessibility_subtitle_recipient_withperiod,
							ScreenreaderPronunciation.getPronunciation(
									context,
									src.getDest().getDecoded())))
					.append(separator);
		} else if(src.getAuthor() != null) {
			accessibilityHeader
					.append(context.getString(
							PrefsUtility.pref_accessibility_concise_mode()
									?R.string.accessibility_subtitle_author_withperiod_concise_post
									: R.string.accessibility_subtitle_author_withperiod,
							ScreenreaderPronunciation.getPronunciation(
									context,
									src.getAuthor().getDecoded())))
					.append(separator);
		}

		accessibilityHeader
				.append(context.getString(
						R.string.accessibility_subtitle_age_withperiod,
								TimeFormatHelper.format(
										src.getCreated_utc().getValue().elapsedPeriod(),
										context,
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
		subjectText.setText(StringEscapeUtils.unescapeHtml4(src.getSubject() != null
				? src.getSubject().getDecoded()
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
