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
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementNumberedListElement;

import java.util.ArrayList;

public class HtmlRawElementNumberedList extends HtmlRawElement {

	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementNumberedList(@NonNull final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		for(final HtmlRawElement element : mChildren) {
			element.getPlainText(stringBuilder);
		}
	}

	public HtmlRawElementNumberedList reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced, linkButtons);
		}

		return new HtmlRawElementNumberedList(reduced);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(reduce(activeAttributes, activity, linkButtons));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		int number = 1;

		for(final HtmlRawElement child : mChildren) {

			final ArrayList<BodyElement> thisElement = new ArrayList<>();
			child.generate(activity, thisElement);

			destination.add(new BodyElementNumberedListElement(number, thisElement));

			number++;
		}
	}
}
