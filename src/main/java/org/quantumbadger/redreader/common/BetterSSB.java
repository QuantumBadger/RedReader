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

package org.quantumbadger.redreader.common;

import android.graphics.Typeface;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.SuperscriptSpan;
import android.text.style.URLSpan;
import android.text.style.UnderlineSpan;

import java.util.HashSet;

public class BetterSSB {

	private final SpannableStringBuilder sb;

	public static final int BOLD = 1;
	public static final int ITALIC = 1 << 1;
	public static final int UNDERLINE = 1 << 2;
	public static final int STRIKETHROUGH = 1 << 3;
	public static final int FOREGROUND_COLOR = 1 << 4;
	public static final int BACKGROUND_COLOR = 1 << 5;
	public static final int SIZE = 1 << 6;
	public static final int SUPERSCRIPT = 1 << 7;

	public static final char NBSP = '\u00A0';

	public BetterSSB() {
		this.sb = new SpannableStringBuilder();
	}

	public void append(final String str, final int flags) {
		append(str, flags, 0, 0, 1f);
	}

	public void append(final String str, final int flags, final String url) {
		append(str, flags, 0, 0, 1f, url);
	}

	public void append(
			final String str,
			final int flags,
			final int foregroundCol,
			final int backgroundCol,
			final float scale) {
		append(str, flags, foregroundCol, backgroundCol, scale, null);
	}

	public void append(
			final String str,
			final int flags,
			final int foregroundCol,
			final int backgroundCol,
			final float scale,
			final String url) {

		final int strStart = sb.length();
		sb.append(str);
		final int strEnd = sb.length();

		if((flags & BOLD) != 0) {
			sb.setSpan(
					new StyleSpan(Typeface.BOLD),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & ITALIC) != 0) {
			sb.setSpan(
					new StyleSpan(Typeface.ITALIC),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & UNDERLINE) != 0) {
			sb.setSpan(
					new UnderlineSpan(),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & STRIKETHROUGH) != 0) {
			sb.setSpan(
					new StrikethroughSpan(),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & FOREGROUND_COLOR) != 0) {
			sb.setSpan(
					new ForegroundColorSpan(foregroundCol),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & BACKGROUND_COLOR) != 0) {
			sb.setSpan(
					new BackgroundColorSpan(backgroundCol),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & SIZE) != 0) {
			sb.setSpan(
					new RelativeSizeSpan(scale),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if((flags & SUPERSCRIPT) != 0) {
			sb.setSpan(
					new SuperscriptSpan(),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(url != null) {
			sb.setSpan(
					new URLSpan(url),
					strStart,
					strEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}
	}

	public void linkify() {

		final String asText = sb.toString();
		final HashSet<String> links = LinkHandler.computeAllLinks(asText);

		for(final String link : links) {

			int index = -1;

			while(index < asText.length()
					&& (index = asText.indexOf(link, index + 1)) >= 0) {
				if(sb.getSpans(index, index + link.length(), URLSpan.class).length < 1) {
					sb.setSpan(
							new URLSpan(link),
							index,
							index + link.length(),
							Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
				}
			}
		}
	}

	public SpannableStringBuilder get() {
		return sb;
	}
}
