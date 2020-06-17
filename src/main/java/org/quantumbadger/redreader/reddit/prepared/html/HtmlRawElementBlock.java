package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementTextSpanned;

import java.util.ArrayList;
import java.util.Arrays;

public class HtmlRawElementBlock extends HtmlRawElement {

	@NonNull private final BlockType mBlockType;
	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementBlock(
			@NonNull final BlockType blockType,
			final ArrayList<HtmlRawElement> children) {

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

		@Nullable SpannableStringBuilder currentSsb = null;

		for(final HtmlRawElement child : mChildren) {

			if(child instanceof HtmlRawElementStyledText) {

				if(currentSsb == null) {
					currentSsb = new SpannableStringBuilder();
				}

				((HtmlRawElementStyledText)child).writeTo(currentSsb);

			} else {

				if(currentSsb != null) {
					destination.add(new BodyElementTextSpanned(mBlockType, currentSsb));
					currentSsb = null;
				}

				child.generate(activity, destination);
			}
		}

		if(currentSsb != null) {
			destination.add(new BodyElementTextSpanned(mBlockType, currentSsb));
			currentSsb = null;
		}
	}
}
