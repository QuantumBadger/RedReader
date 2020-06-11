package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LinkDetailsView;

public class BodyElementButton extends BodyElement {

	@NonNull private final String mText;
	@Nullable private final String mSubtitle;

	@NonNull private final View.OnClickListener mOnClick;
	@Nullable private final View.OnLongClickListener mOnLongClick;

	public BodyElementButton(
			@NonNull final String text,
			@Nullable final String subtitle,
			@NonNull final View.OnClickListener onClick,
			@Nullable final View.OnLongClickListener onLongClick) {

		super(BlockType.BUTTON);
		mText = text;
		mSubtitle = subtitle;
		mOnClick = onClick;
		mOnLongClick = onLongClick;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinkDetailsView ldv = new LinkDetailsView(
				activity,
				mText,
				mSubtitle);

		final int linkMarginPx = General.dpToPixels(activity, 8);

		final ViewGroup.MarginLayoutParams layoutParams = new ViewGroup.MarginLayoutParams(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.WRAP_CONTENT);

		// TODO gets partially overwritten by vertical sequence
		layoutParams.setMargins(0, linkMarginPx, 0, linkMarginPx);
		ldv.setLayoutParams(layoutParams);

		ldv.setOnClickListener(mOnClick);

		if(mOnLongClick != null) {
			ldv.setOnLongClickListener(mOnLongClick);
		}


		return ldv;
	}
}
