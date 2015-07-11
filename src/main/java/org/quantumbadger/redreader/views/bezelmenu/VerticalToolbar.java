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

package org.quantumbadger.redreader.views.bezelmenu;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ScrollView;

public class VerticalToolbar extends FrameLayout {

	private final LinearLayout buttons;

	public VerticalToolbar(Context context) {

		super(context);

		setBackgroundColor(Color.argb(192, 0, 0, 0)); // TODO change color based on theme?
		// TODO add light, vertical line on swipe side

		buttons = new LinearLayout(context);
		buttons.setOrientation(android.widget.LinearLayout.VERTICAL);

		final ScrollView sv = new ScrollView(context);
		sv.addView(buttons);
		addView(sv);
	}

	public void addItem(View v) {
		buttons.addView(v);
	}
}
