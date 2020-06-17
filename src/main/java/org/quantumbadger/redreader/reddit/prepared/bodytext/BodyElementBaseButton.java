package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LinkDetailsView;

public abstract class BodyElementBaseButton extends BodyElement {

	@NonNull private final String mText;
	@Nullable private final String mSubtitle;

	private final boolean mIsLinkButton;

	@NonNull
	protected abstract View.OnClickListener generateOnClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons);

	@Nullable
	protected abstract View.OnLongClickListener generateOnLongClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons);

	public BodyElementBaseButton(
			@NonNull final String text,
			@Nullable final String subtitle, final boolean isLinkButton) {

		super(BlockType.BUTTON);
		mText = text;
		mSubtitle = subtitle;
		mIsLinkButton = isLinkButton;
	}

	@Override
	public final View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		if(mIsLinkButton && !showLinkButtons) {
			// Don't show
			final View result = new View(activity);
			result.setVisibility(View.GONE);
			return result;
		}

		final LinkDetailsView ldv = new LinkDetailsView(
				activity,
				mText,
				mSubtitle);

		final int linkMarginPx = General.dpToPixels(activity, 8);

		final ViewGroup.MarginLayoutParams layoutParams = new ViewGroup.MarginLayoutParams(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.WRAP_CONTENT);

		layoutParams.setMargins(0, linkMarginPx, 0, linkMarginPx);
		ldv.setLayoutParams(layoutParams);

		ldv.setOnClickListener(
				generateOnClickListener(activity, textColor, textSize, showLinkButtons));

		final View.OnLongClickListener longClickListener
				= generateOnLongClickListener(activity, textColor, textSize, showLinkButtons);

		if(longClickListener != null) {
			ldv.setOnLongClickListener(longClickListener);
		}

		return ldv;
	}
}
