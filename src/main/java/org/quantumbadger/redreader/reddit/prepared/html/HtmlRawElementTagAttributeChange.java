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
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;

public abstract class HtmlRawElementTagAttributeChange extends HtmlRawElementTag {

	private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementTagAttributeChange(final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	protected void onLinkButtons(@NonNull final ArrayList<LinkButtonDetails> linkButtons) {
		// Add nothing by default
	}

	protected abstract void onStart(@NonNull HtmlTextAttributes activeAttributes);

	protected abstract void onEnd(@NonNull HtmlTextAttributes activeAttributes);

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		for(final HtmlRawElement element : mChildren) {
			element.getPlainText(stringBuilder);
		}
	}

	@Override
	public final void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		onStart(activeAttributes);

		try {
			for(final HtmlRawElement child : mChildren) {
				child.reduce(activeAttributes, activity, destination, linkButtons);
			}

		} finally {
			onEnd(activeAttributes);
		}

		onLinkButtons(linkButtons);
	}

	@Override
	public final void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException("Attempt to call generate() on reducible element");
	}
}
