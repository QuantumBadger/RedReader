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

import java.util.ArrayList;

public class HtmlRawElementTagAnchor extends HtmlRawElementTagAttributeChange {

	@NonNull private final String mHref;

	public HtmlRawElementTagAnchor(
			final ArrayList<HtmlRawElement> children,
			@NonNull final String href) {
		super(children);
		mHref = href;
	}

	@Override
	protected void onLinkButtons(@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		final String text = getPlainText().trim();

		linkButtons.add(new LinkButtonDetails(
				text.isEmpty() ? null : text,
				mHref));
	}

	@Override
	protected void onStart(@NonNull final HtmlTextAttributes activeAttributes) {
		activeAttributes.href = mHref;
	}

	@Override
	protected void onEnd(@NonNull final HtmlTextAttributes activeAttributes) {
		activeAttributes.href = null;
	}
}
