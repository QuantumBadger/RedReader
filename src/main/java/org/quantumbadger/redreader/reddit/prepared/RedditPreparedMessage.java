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
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import android.view.View;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParagraphGroup;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParser;
import org.quantumbadger.redreader.reddit.things.RedditMessage;

import java.util.HashSet;

public final class RedditPreparedMessage implements RedditRenderableInboxItem {

	public SpannableStringBuilder header;
	public final MarkdownParagraphGroup body;
	public final String idAndType;
	public final RedditMessage src;

	public RedditPreparedMessage(final Context context, final RedditMessage message, final long timestamp) {

		this.src = message;

		// TODO custom time
		// TODO respect RRTheme

		final int rrCommentHeaderBoldCol;
		final int rrCommentHeaderAuthorCol;

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[]{
					R.attr.rrCommentHeaderBoldCol,
					R.attr.rrCommentHeaderAuthorCol,
			});

			rrCommentHeaderBoldCol = appearance.getColor(0, 255);
			rrCommentHeaderAuthorCol = appearance.getColor(1, 255);

			appearance.recycle();
		}

		body = MarkdownParser.parse(StringEscapeUtils.unescapeHtml4(message.body).toCharArray());

		idAndType = message.name;

		final BetterSSB sb = new BetterSSB();

		if(src.author == null) {
			sb.append("[" + context.getString(R.string.general_unknown) + "]", BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderAuthorCol, 0, 1f);
		} else {
			sb.append(src.author, BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderAuthorCol, 0, 1f);
		}

		sb.append("   ", 0);
		sb.append(RRTime.formatDurationFrom(context, src.created_utc * 1000L), BetterSSB.FOREGROUND_COLOR | BetterSSB.BOLD, rrCommentHeaderBoldCol, 0, 1f);

		header = sb.get();
	}

	public HashSet<String> computeAllLinks() {
		return LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(src.body_html));
	}

	public SpannableStringBuilder getHeader() {
		return header;
	}

	public void handleInboxClick(AppCompatActivity activity) {
		// Do nothing
	}

	@Override
	public void handleInboxLongClick(final AppCompatActivity activity) {
		// Do nothing
	}

	@Override
	public CharSequence getHeader(final RRThemeAttributes theme, final RedditChangeDataManager changeDataManager, final Context context) {
		return header;
	}

	@Override
	public View getBody(final AppCompatActivity activity, final Integer textColor, final Float textSize, final boolean showLinkButtons) {
		return body.buildView(activity, textColor, textSize, showLinkButtons);
	}
}
