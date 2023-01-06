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

import android.text.SpannableStringBuilder;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementTextSpanned;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReference;

public class HtmlRawElementBlock extends HtmlRawElement {

	@NonNull private final BlockType mBlockType;
	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementBlock(
			@NonNull final BlockType blockType,
			@NonNull final ArrayList<HtmlRawElement> children) {

		mBlockType = blockType;
		mChildren = children;
	}

	public HtmlRawElementBlock(
			@NonNull final BlockType blockType,
			final HtmlRawElement... children) {

		mBlockType = blockType;
		mChildren = new ArrayList<>(children.length);
		mChildren.addAll(Arrays.asList(children));
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		for(final HtmlRawElement element : mChildren) {
			element.getPlainText(stringBuilder);
		}
	}

	public HtmlRawElementBlock reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();
		final ArrayList<LinkButtonDetails> linkButtons = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced, linkButtons);
		}

		for(final LinkButtonDetails details : linkButtons) {
			reduced.add(new HtmlRawElementLinkButton(details));
		}

		return new HtmlRawElementBlock(mBlockType, reduced);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(reduce(activeAttributes, activity));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {
		boolean lastChildNotProcessed = false;

		final AtomicReference<SpannableStringBuilder> ssbReference =
				new AtomicReference<>(new SpannableStringBuilder());

		final BodyElementTextSpanned bodyElementTextSpanned =
				new BodyElementTextSpanned(mBlockType, ssbReference);

		for(final HtmlRawElement child : mChildren) {
			if(child instanceof HtmlRawElementStyledText) {
				((HtmlRawElementStyledText)child).writeTo(ssbReference);

				lastChildNotProcessed = true;
			} else if (child instanceof  HtmlRawElementImg) {
				((HtmlRawElementImg) child).writeTo(ssbReference,
						activity,
						bodyElementTextSpanned.mInvalidateCallback);

				lastChildNotProcessed = true;
			} else {
				if (lastChildNotProcessed) {
					destination.add(bodyElementTextSpanned);
				}

				child.generate(activity, destination);
				lastChildNotProcessed = false;
			}
		}

		// If the last child in the array is a HtmlRawElementStyledText
		// or HtmlRawElementImg object, it won't be added to the destination array in the loop
		// Need this logic to make sure that it's added
		if(lastChildNotProcessed) {
			destination.add(bodyElementTextSpanned);
		}
	}
}
