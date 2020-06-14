package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;

public class BodyElementButton extends BodyElementBaseButton {

	@NonNull private final View.OnClickListener mOnClick;
	@Nullable private final View.OnLongClickListener mOnLongClick;

	public BodyElementButton(
			@NonNull final String text,
			@Nullable final String subtitle,
			@NonNull final View.OnClickListener onClick,
			@Nullable final View.OnLongClickListener onLongClick) {

		super(text, subtitle);
		mOnClick = onClick;
		mOnLongClick = onLongClick;
	}

	@NonNull
	@Override
	protected View.OnClickListener generateOnClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return mOnClick;
	}

	@Nullable
	@Override
	protected View.OnLongClickListener generateOnLongClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return mOnLongClick;
	}
}
