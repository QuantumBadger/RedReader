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

package org.quantumbadger.redreader.reddit.prepared.html;

import android.graphics.Color;
import androidx.annotation.NonNull;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.text.style.ForegroundColorSpan;

import java.util.ArrayList;

public abstract class HtmlRawElementInlineErrorMessage extends HtmlRawElement {

	private HtmlRawElementInlineErrorMessage() {
	}

	public static HtmlRawElementStyledText create(@NonNull final String text) {

		final ArrayList<CharacterStyle> spans = new ArrayList<>();
		spans.add(new BackgroundColorSpan(Color.RED));
		spans.add(new ForegroundColorSpan(Color.WHITE));

		return new HtmlRawElementStyledText(text, spans);
	}

	@NonNull
	public static HtmlRawElementTagPassthrough appendError(
			@NonNull final String text,
			@NonNull final HtmlRawElement element) {

		final ArrayList<HtmlRawElement> children = new ArrayList<>();

		children.add(element);
		children.add(HtmlRawElementInlineErrorMessage.create(text));

		return new HtmlRawElementTagPassthrough(children);
	}
}
