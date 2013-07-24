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
import android.view.View;
import android.view.ViewGroup;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LinkDetailsView;

public final class MarkdownParagraphGroup {

	private final MarkdownParagraph[] paragraphs;

	public MarkdownParagraphGroup(final MarkdownParagraph[] paragraphs) {
		this.paragraphs = paragraphs;
	}

	public ViewGroup buildView(final Activity activity, final Integer textColor, final Float textSize) {

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		final int paragraphSpacing = (int) (dpScale * 6);
		final int codeLineSpacing = (int) (dpScale * 3);
		final int quoteBarWidth = (int) (dpScale * 3);
		final int maxQuoteLevel = 5;

		final LinearLayout layout = new LinearLayout(activity);
		layout.setOrientation(android.widget.LinearLayout.VERTICAL);

		for(final MarkdownParagraph paragraph : paragraphs) {

			final TextView tv = new TextView(activity);
			tv.setText(paragraph.spanned);

			if(textColor != null) tv.setTextColor(textColor);
			if(textSize != null) tv.setTextSize(textSize);

			switch(paragraph.type) {

				case BULLET:
					break;

				case NUMBERED:
					break;

				case CODE:
					tv.setTypeface(General.getMonoTypeface(activity));
					tv.setText(paragraph.raw.arr, paragraph.raw.start + 4, paragraph.raw.length - 4);
					layout.addView(tv);

					if(paragraph.parent != null) {
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin
								= paragraph.parent.type == MarkdownParser.MarkdownParagraphType.CODE
								? codeLineSpacing : paragraphSpacing;
					}

					((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).leftMargin = (int) (dpScale * 6);
					break;

				case HEADER:
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

			for(final MarkdownParagraph.Link link : paragraph.links) {

				final LinkDetailsView ldv = new LinkDetailsView(activity, link.title, link.subtitle);
				layout.addView(ldv);

				final int linkMarginPx = Math.round(dpScale * 8);
				((LinearLayout.LayoutParams) ldv.getLayoutParams()).setMargins(0, linkMarginPx, 0, linkMarginPx);
				ldv.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

				ldv.setOnClickListener(new View.OnClickListener() {
					public void onClick(View v) {
						link.onClicked(activity);
					}
				});
			}
		}

		return layout;
	}
}
