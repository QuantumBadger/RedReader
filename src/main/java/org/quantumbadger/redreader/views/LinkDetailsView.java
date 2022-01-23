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

package org.quantumbadger.redreader.views;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RectShape;
import android.view.Gravity;
import android.view.MotionEvent;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;

public class LinkDetailsView extends FrameLayout {

	@SuppressLint("ClickableViewAccessibility")
	public LinkDetailsView(
			final Context context,
			@NonNull final String title,
			@Nullable final String subtitle) {

		super(context);

		setClickable(true);

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.HORIZONTAL);
		addView(layout);
		final int marginPx = General.dpToPixels(context, 10);

		layout.setGravity(Gravity.CENTER_VERTICAL);

		final TypedArray appearance
				= context.obtainStyledAttributes(new int[] {R.attr.rrIconLink});
		final ImageView icon = new ImageView(context);
		icon.setImageDrawable(appearance.getDrawable(0));
		appearance.recycle();
		layout.addView(icon);
		((LinearLayout.LayoutParams)icon.getLayoutParams()).setMargins(
				marginPx,
				marginPx,
				marginPx,
				marginPx);

		final LinearLayout textLayout = new LinearLayout(context);
		textLayout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textLayout);
		((LinearLayout.LayoutParams)textLayout.getLayoutParams()).setMargins(
				0,
				marginPx,
				marginPx,
				marginPx);

		final float linkFontScale = PrefsUtility.appearance_fontscale_linkbuttons();

		{
			final TextView titleView = new TextView(context);
			titleView.setText(title);
			titleView.setTextSize(15f * linkFontScale);
			textLayout.addView(titleView);
		}

		if(subtitle != null && !title.equals(subtitle)) {
			final TextView subtitleView = new TextView(context);
			subtitleView.setText(subtitle);
			subtitleView.setTextSize(11f * linkFontScale);
			textLayout.addView(subtitleView);
		}

		final float borderPx = General.dpToPixels(context, 2);

		final RectShape borderShape = new RectShape();
		final ShapeDrawable border = new ShapeDrawable(borderShape);
		border.getPaint().setColor(Color.argb(128, 128, 128, 128));
		border.getPaint().setStrokeWidth(borderPx);
		border.getPaint().setStyle(Paint.Style.STROKE);

		//noinspection deprecation
		setBackgroundDrawable(border);

		setOnTouchListener((v, event) -> {

			switch(event.getActionMasked()) {

				case MotionEvent.ACTION_DOWN:
					layout.setBackgroundColor(Color.argb(50, 128, 128, 128));
					invalidate();
					break;

				case MotionEvent.ACTION_UP:
				case MotionEvent.ACTION_CANCEL:
					layout.setBackgroundColor(Color.TRANSPARENT);
					invalidate();
					break;
			}

			return false;
		});
	}
}
