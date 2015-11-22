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

package org.quantumbadger.redreader.views.floatingtoolbar;

import android.content.Context;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import org.quantumbadger.redreader.common.General;

public class FloatingToolbar extends FrameLayout {

	private final LinearLayout mToolbar;

	public FloatingToolbar(final Context context) {

		super(context);

		mToolbar = new LinearLayout(context);
		mToolbar.setOrientation(LinearLayout.HORIZONTAL);

		addView(mToolbar);
		final LayoutParams toolbarLayoutParams = (LayoutParams)mToolbar.getLayoutParams();

		final int marginPx = General.dpToPixels(context, 30);
		toolbarLayoutParams.leftMargin = marginPx;
		toolbarLayoutParams.rightMargin = marginPx;
		toolbarLayoutParams.topMargin = marginPx;
		toolbarLayoutParams.bottomMargin = marginPx;

		toolbarLayoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
		toolbarLayoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;

		toolbarLayoutParams.gravity = Gravity.BOTTOM | Gravity.RIGHT;

		mToolbar.setBackgroundColor(Color.argb(200, 0, 0, 0));
	}

	public void addToolbarItem(final View v) {
		mToolbar.addView(v);
	}
}
