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

package org.saiditnet.redreader.common;

import android.graphics.Typeface;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.*;

import java.util.HashSet;

public class BetterSSB {

	private final SpannableStringBuilder sb;

	public static final int
			BOLD = 1,
			ITALIC = 1 << 1,
			UNDERLINE = 1 << 2,
			STRIKETHROUGH = 1 << 3,
			FOREGROUND_COLOR = 1 << 4,
			BACKGROUND_COLOR = 1 << 5,
			SIZE = 1 << 6;

	public BetterSSB() {
		this.sb = new SpannableStringBuilder();
	}

	public void append(String str, int flags) {
		append(str, flags, 0, 0, 1f);
	}

	public void append(String str, int flags, String url) {
		append(str, flags, 0, 0, 1f, url);
	}

	public void append(String str, int flags, int foregroundCol, int backgroundCol, float scale) {
		append(str, flags,  foregroundCol, backgroundCol, scale, null);
	}

	public void append(String str, int flags, int foregroundCol, int backgroundCol, float scale, String url) {

		final int strStart = sb.length();
		sb.append(str);
		final int strEnd = sb.length();

		if((flags & BOLD) != 0) {
			sb.setSpan(new StyleSpan(Typeface.BOLD), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & ITALIC) != 0) {
			sb.setSpan(new StyleSpan(Typeface.ITALIC), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & UNDERLINE) != 0) {
			sb.setSpan(new UnderlineSpan(), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & STRIKETHROUGH) != 0) {
			sb.setSpan(new StrikethroughSpan(), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & FOREGROUND_COLOR) != 0) {
			sb.setSpan(new ForegroundColorSpan(foregroundCol), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & BACKGROUND_COLOR) != 0) {
			sb.setSpan(new BackgroundColorSpan(backgroundCol), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & SIZE) != 0) {
			sb.setSpan(new RelativeSizeSpan(scale), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(url != null) {
			sb.setSpan(new URLSpan(url), strStart, strEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}
	}

	public void linkify() {

		final String asText = sb.toString();
		final HashSet<String> links = LinkHandler.computeAllLinks(asText);

		for(String link : links) {

			int index = -1;

			while(index < asText.length() && (index = asText.indexOf(link, index + 1)) >= 0) {
				if(sb.getSpans(index, index + link.length(), URLSpan.class).length < 1) {
					sb.setSpan(new URLSpan(link), index, index + link.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
				}
			}
		}
	}

	public SpannableStringBuilder get() {
		return sb;
	}
}
