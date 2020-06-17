package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.app.AlertDialog;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ScrollView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;

public class BodyElementSpoilerButton extends BodyElementBaseButton {

	@NonNull private final BodyElement mSpoilerText;

	public BodyElementSpoilerButton(
			@NonNull final AppCompatActivity activity,
			@NonNull final BodyElement spoilerText) {

		super(activity.getApplicationContext().getString(R.string.spoiler), null, false);
		mSpoilerText = spoilerText;
	}

	@NonNull
	@Override
	protected View.OnClickListener generateOnClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return (button) -> {
			final ScrollView scrollView = new ScrollView(activity);

			final View view = mSpoilerText.generateView(
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
					(dialog, which) -> {
					});

			final AlertDialog alert = builder.create();
			alert.show();
		};
	}

	@Nullable
	@Override
	protected View.OnLongClickListener generateOnLongClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return null;
	}
}
