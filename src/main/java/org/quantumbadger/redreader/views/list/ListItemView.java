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

package org.quantumbadger.redreader.views.list;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.Optional;

// TODO just make this a linear layout
public class ListItemView extends FrameLayout {

	private final View mDivider;

	private final TextView mMainText;
	private final ImageView mMainIcon;
	private final LinearLayout mMainLink;

	private final View mSecondaryLink;
	private final ImageView mSecondaryIcon;

	public ListItemView(final Context context) {

		super(context);

		final LinearLayout ll = (LinearLayout)inflate(context, R.layout.list_item, null);

		mDivider = ll.findViewById(R.id.list_item_divider);

		mMainText = ll.findViewById(R.id.list_item_text);
		mMainIcon = ll.findViewById(R.id.list_item_icon);
		mMainLink = ll.findViewById(R.id.list_item_main_link);

		mSecondaryLink = ll.findViewById(R.id.list_item_secondary_link_outer);
		mSecondaryIcon = ll.findViewById(R.id.list_item_secondary_icon);

		addView(ll);
	}

	public void reset(
			@Nullable final Drawable icon,
			@NonNull final CharSequence text,
			@Nullable final String contentDescription,
			final boolean hideDivider,
			@Nullable final OnClickListener clickListener,
			@Nullable final OnLongClickListener longClickListener,
			@NonNull final Optional<Drawable> secondaryIcon,
			@NonNull final Optional<OnClickListener> secondaryAction,
			@NonNull final Optional<String> secondaryContentDesc) {

		if(hideDivider) {
			mDivider.setVisibility(View.GONE);
		} else {
			mDivider.setVisibility(View.VISIBLE);
		}

		mMainText.setText(text);
		mMainText.setContentDescription(contentDescription);

		if(icon != null) {
			mMainIcon.setImageDrawable(icon);
			mMainIcon.setVisibility(VISIBLE);
		} else {
			mMainIcon.setImageBitmap(null);
			mMainIcon.setVisibility(GONE);
		}

		if(clickListener != null) {
			mMainLink.setClickable(true);
			mMainLink.setFocusable(true);
			mMainLink.setOnClickListener(clickListener);
		} else {
			mMainLink.setClickable(false);
			mMainLink.setFocusable(false);
			mMainLink.setOnClickListener(null);
		}

		if(longClickListener != null) {
			mMainLink.setLongClickable(true);
			mMainLink.setOnLongClickListener(longClickListener);
		} else {
			mMainLink.setLongClickable(false);
			mMainLink.setOnLongClickListener(null);
		}

		if(secondaryIcon.isPresent()) {
			mSecondaryIcon.setImageDrawable(secondaryIcon.get());
			mSecondaryIcon.setContentDescription(secondaryContentDesc.orElseNull());
		}

		if(secondaryAction.isPresent()) {
			mSecondaryLink.setVisibility(VISIBLE);
			mSecondaryIcon.setOnClickListener(secondaryAction.get());
		} else {
			mSecondaryLink.setVisibility(GONE);
			mSecondaryIcon.setOnClickListener(null);
		}
	}
}
