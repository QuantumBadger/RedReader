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

import android.graphics.Typeface;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import android.text.style.CharacterStyle;
import android.text.style.ClickableSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.SuperscriptSpan;
import android.text.style.TypefaceSpan;
import android.text.style.UnderlineSpan;
import android.view.View;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;

public class HtmlRawElementPlainText extends HtmlRawElement {

	@NonNull private final String mText;

	public HtmlRawElementPlainText(@NonNull final String text) {
		mText = text;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		stringBuilder.append(mText);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes attributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		ArrayList<CharacterStyle> spans = null;

		if(attributes.bold > 0) {
			//noinspection ConstantConditions
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new StyleSpan(Typeface.BOLD));
		}

		if(attributes.italic > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new StyleSpan(Typeface.ITALIC));
		}

		if(attributes.underline > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new UnderlineSpan());
		}

		if(attributes.strikethrough > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new StrikethroughSpan());
		}

		if(attributes.monospace > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new TypefaceSpan("monospace"));
		}

		if(attributes.superscript > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}

			for(int i = 0; i < attributes.superscript; i++) {
				spans.add(new SuperscriptSpan());
				spans.add(new RelativeSizeSpan(0.85f));
			}
		}

		if(attributes.extraLarge > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new RelativeSizeSpan(1.6f));

		} else if(attributes.large > 0) {
			if(spans == null) {
				spans = new ArrayList<>();
			}
			spans.add(new RelativeSizeSpan(1.3f));
		}

		if(attributes.href != null) {

			if(spans == null) {
				spans = new ArrayList<>();
			}

			final String url = attributes.href;

			spans.add(new ClickableSpan() {
				@Override
				public void onClick(@NonNull final View widget) {
					LinkHandler.onLinkClicked(activity, url);
				}
			});
		}

		destination.add(new HtmlRawElementStyledText(mText, spans));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException("Attempt to call generate() on reducible element");
	}
}
