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

package org.quantumbadger.redreader.reddit.prepared.markdown;

import android.graphics.Color;
import androidx.appcompat.app.AppCompatActivity;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.style.UnderlineSpan;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LinkDetailsView;
import org.quantumbadger.redreader.views.LinkifiedTextView;

public final class MarkdownParagraphGroup {

	private final MarkdownParagraph[] paragraphs;

	public MarkdownParagraphGroup(final MarkdownParagraph[] paragraphs) {
		this.paragraphs = paragraphs;
	}

	public ViewGroup buildView(final AppCompatActivity activity, final Integer textColor, final Float textSize,
							   final boolean showLinkButtons) {

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		final int paragraphSpacing = (int) (dpScale * 6);
		final int codeLineSpacing = (int) (dpScale * 3);
		final int quoteBarWidth = (int) (dpScale * 3);
		final int maxQuoteLevel = 5;

		final LinearLayout layout = new LinearLayout(activity);
		layout.setOrientation(LinearLayout.VERTICAL);

		for(final MarkdownParagraph paragraph : paragraphs) {

			final TextView tv = new LinkifiedTextView(activity);
			tv.setText(paragraph.spanned, TextView.BufferType.SPANNABLE);

			if(textColor != null) tv.setTextColor(textColor);
			if(textSize != null) tv.setTextSize(textSize);

			switch(paragraph.type) {

				case BULLET: {
					final LinearLayout bulletItem = new LinearLayout(activity);
					final int paddingPx = General.dpToPixels(activity, 6);
					bulletItem.setPadding(paddingPx, paddingPx, paddingPx, 0);

					final TextView bullet = new TextView(activity);
					bullet.setText("•   ");
					if(textSize != null) bullet.setTextSize(textSize);

					bulletItem.addView(bullet);
					bulletItem.addView(tv);

					layout.addView(bulletItem);

					((ViewGroup.MarginLayoutParams)bulletItem.getLayoutParams()).leftMargin
							= (int) (dpScale * (paragraph.level == 0 ? 12 : 24));

					break;
				}

				case NUMBERED: {
					final LinearLayout numberedItem = new LinearLayout(activity);
					final int paddingPx = General.dpToPixels(activity, 6);
					numberedItem.setPadding(paddingPx, paddingPx, paddingPx, 0);

					final TextView number = new TextView(activity);
					number.setText(paragraph.number + ".   ");
					if(textSize != null) number.setTextSize(textSize);

					numberedItem.addView(number);
					numberedItem.addView(tv);

					layout.addView(numberedItem);

					((ViewGroup.MarginLayoutParams)numberedItem.getLayoutParams()).leftMargin
							= (int) (dpScale * (paragraph.level == 0 ? 12 : 24));

					break;
				}

				case CODE:
					tv.setTypeface(General.getMonoTypeface(activity));
					tv.setText(paragraph.raw.arr, paragraph.raw.start, paragraph.raw.length);
					layout.addView(tv);

					if(paragraph.parent != null) {
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin
								= paragraph.parent.type == MarkdownParser.MarkdownParagraphType.CODE
								? codeLineSpacing : paragraphSpacing;
					}

					((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).leftMargin = (int) (dpScale * 6);
					break;

				case HEADER:
					final SpannableString underlinedText = new SpannableString(paragraph.spanned);
					underlinedText.setSpan(new UnderlineSpan(), 0, underlinedText.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
					tv.setText(underlinedText);
					layout.addView(tv);
					if(paragraph.parent != null) {
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin = paragraphSpacing;
					}
					break;

				case HLINE: {

					final View hLine = new View(activity);
					layout.addView(hLine);
					final ViewGroup.MarginLayoutParams hLineParams = (ViewGroup.MarginLayoutParams) hLine.getLayoutParams();
					hLineParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
					hLineParams.height = (int) dpScale;
					hLineParams.setMargins((int)(dpScale * 15), paragraphSpacing, (int)(dpScale * 15), 0);
					hLine.setBackgroundColor(Color.rgb(128, 128, 128));
					break;
				}

				case QUOTE: {

					final LinearLayout quoteLayout = new LinearLayout(activity);

					for(int lvl = 0; lvl < Math.min(maxQuoteLevel, paragraph.level); lvl++) {
						final View quoteIndent = new View(activity);
						quoteLayout.addView(quoteIndent);
						quoteIndent.setBackgroundColor(Color.rgb(128, 128, 128));
						quoteIndent.getLayoutParams().width = quoteBarWidth;
						quoteIndent.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
						((ViewGroup.MarginLayoutParams)quoteIndent.getLayoutParams()).rightMargin = quoteBarWidth;
					}

					quoteLayout.addView(tv);
					layout.addView(quoteLayout);

					if(paragraph.parent != null) {
						if(paragraph.parent.type == MarkdownParser.MarkdownParagraphType.QUOTE) {
							((ViewGroup.MarginLayoutParams)tv.getLayoutParams()).topMargin = paragraphSpacing;
						} else {
							((ViewGroup.MarginLayoutParams)quoteLayout.getLayoutParams()).topMargin = paragraphSpacing;
						}
					}

					break;
				}

				case TEXT:

					layout.addView(tv);
					if(paragraph.parent != null) {
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin = paragraphSpacing;
					}

					break;

				case EMPTY:
					throw new RuntimeException("Internal error: empty paragraph when building view");
			}

			if(showLinkButtons) {
				for(final MarkdownParagraph.Link link : paragraph.links) {

					final LinkDetailsView ldv = new LinkDetailsView(activity, link.title, link.subtitle);
					layout.addView(ldv);

					final int linkMarginPx = Math.round(dpScale * 8);
					((LinearLayout.LayoutParams) ldv.getLayoutParams()).setMargins(0, linkMarginPx, 0, linkMarginPx);
					ldv.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

					ldv.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View v) {
							link.onClicked(activity);
						}
					});
					ldv.setOnLongClickListener(new View.OnLongClickListener(){
						@Override
						public boolean onLongClick(View v){
							link.onLongClicked(activity);
							return true;
						}
					});
				}
			}
		}

		return layout;
	}
}
