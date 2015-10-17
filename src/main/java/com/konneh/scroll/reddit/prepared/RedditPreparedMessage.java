/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.reddit.prepared;

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.text.SpannableStringBuilder;
import android.view.ViewGroup;
import org.apache.commons.lang3.StringEscapeUtils;
import com.konneh.scroll.R;
import com.konneh.scroll.common.BetterSSB;
import com.konneh.scroll.common.LinkHandler;
import com.konneh.scroll.common.RRTime;
import com.konneh.scroll.reddit.RedditPreparedInboxItem;
import com.konneh.scroll.reddit.prepared.markdown.MarkdownParagraphGroup;
import com.konneh.scroll.reddit.prepared.markdown.MarkdownParser;
import com.konneh.scroll.reddit.things.RedditMessage;

import java.util.HashSet;

public final class RedditPreparedMessage implements RedditPreparedInboxItem {

	public SpannableStringBuilder header;
	public final MarkdownParagraphGroup body;
	public final String idAndType;
	public final RedditMessage src;

	public RedditPreparedMessage(final Context context, final RedditMessage message, final long timestamp) {

		this.src = message;

		// TODO custom time

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

	public ViewGroup getBody(Activity context, float textSize, Integer textCol, boolean showLinkButtons) {
		return body.buildView(context, textCol, textSize, showLinkButtons);
	}

	public void handleInboxClick(Activity activity) {
		// Do nothing
	}
}
