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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RectShape;
import android.view.Gravity;
import android.view.MotionEvent;
import android.widget.ImageView;
import android.widget.TextView;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;

public class LinkDetailsView extends FrameLayout {

	public LinkDetailsView(Context context, final String title, final String subtitle) {

		super(context);

		setClickable(true);

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.HORIZONTAL);
		addView(layout);
		final int marginPx = General.dpToPixels(context, 8);

		layout.setGravity(Gravity.CENTER_VERTICAL);

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{R.attr.rrIconGlobe});
		final ImageView globe = new ImageView(context);
		globe.setImageDrawable(appearance.getDrawable(0));
		layout.addView(globe);
		((LinearLayout.LayoutParams)globe.getLayoutParams()).setMargins(marginPx, marginPx, marginPx, marginPx);

		final LinearLayout textLayout = new LinearLayout(context);
		textLayout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textLayout);
		((LinearLayout.LayoutParams)textLayout.getLayoutParams()).setMargins(0, marginPx, marginPx, marginPx);

		{
			final TextView titleView = new TextView(context);
			titleView.setText(title);
			titleView.setTextSize(15f); // TODO scale with comment
			textLayout.addView(titleView);
		}

		if(subtitle != null && !title.equals(subtitle)) {
			final TextView subtitleView = new TextView(context);
			subtitleView.setText(subtitle);
			subtitleView.setTextSize(11f); // TODO scale with comment
			textLayout.addView(subtitleView);
		}

		final RectShape borderShape = new RectShape();
		final ShapeDrawable border = new ShapeDrawable(borderShape);
		border.getPaint().setColor(Color.rgb(128, 128, 128));
		border.getPaint().setStrokeWidth(1f);
		border.getPaint().setStyle(Paint.Style.STROKE);

		//noinspection deprecation
		setBackgroundDrawable(border);
	}

	@Override
	public boolean onInterceptTouchEvent(MotionEvent ev) {

		switch(ev.getActionMasked()) {

			case MotionEvent.ACTION_DOWN:
				((ShapeDrawable)getBackground()).getPaint().setColor(Color.rgb(0x00, 0x99, 0xCC));
				((ShapeDrawable)getBackground()).getPaint().setStrokeWidth(2f);
				invalidate();
				break;

			case MotionEvent.ACTION_UP:
			case MotionEvent.ACTION_CANCEL:
				((ShapeDrawable)getBackground()).getPaint().setColor(Color.rgb(128, 128, 128));
				((ShapeDrawable)getBackground()).getPaint().setStrokeWidth(1f);
				invalidate();
				break;
		}

		return super.onInterceptTouchEvent(ev);
	}
}
