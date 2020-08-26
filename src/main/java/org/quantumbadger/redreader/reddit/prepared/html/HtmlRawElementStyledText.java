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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.CharacterStyle;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;

public class HtmlRawElementStyledText extends HtmlRawElement {

	@NonNull private final String mText;
	@Nullable private final ArrayList<CharacterStyle> mSpans;

	public HtmlRawElementStyledText(
			@NonNull final String text,
			@Nullable final ArrayList<CharacterStyle> spans) {
		mText = text;
		mSpans = spans;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		stringBuilder.append(mText);
	}

	public final void writeTo(@NonNull final SpannableStringBuilder ssb) {

		final int textStart = ssb.length();
		ssb.append(mText);
		final int textEnd = ssb.length();

		if(mSpans != null) {
			for(final CharacterStyle span : mSpans) {
				ssb.setSpan(span, textStart, textEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
			}
		}
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(this);
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException(
				"Attempt to call generate() on styled text: should be inside a block");
	}
}
