package org.quantumbadger.redreader.reddit.prepared.html;

import android.app.AlertDialog;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ScrollView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementButton;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementVerticalSequence;

import java.util.ArrayList;

public class HtmlRawElementSpoiler extends HtmlRawElement {

	@NonNull private final HtmlRawElementBlock mChild;

	public HtmlRawElementSpoiler(
			@NonNull final HtmlRawElementBlock child) {

		mChild = child;
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		destination.add(new HtmlRawElementSpoiler(mChild.reduce(activeAttributes, activity)));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			@NonNull final ArrayList<BodyElement> destination) {

		final ArrayList<BodyElement> elements = new ArrayList<>();
		mChild.generate(activity, textColor, textSize, elements);

		destination.add(new BodyElementButton(
				"Spoiler", // TODO translate?
				null,
				v -> {

					final ScrollView scrollView = new ScrollView(activity);

					final View view = new BodyElementVerticalSequence(elements).generateView(
							activity,
							textColor,
							textSize,
							true);

					scrollView.addView(view);

					final ViewGroup.MarginLayoutParams layoutParams
							= (FrameLayout.LayoutParams)view.getLayoutParams();

					final int marginPx = General.dpToPixels(activity, 14);
					layoutParams.setMargins(marginPx, marginPx, marginPx, marginPx);

					final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
					builder.setView(scrollView);

					builder.setNeutralButton(
							R.string.dialog_close,
							(dialog, which) -> {});

					final AlertDialog alert = builder.create();
					alert.show();
				},
				null));
	}
}
