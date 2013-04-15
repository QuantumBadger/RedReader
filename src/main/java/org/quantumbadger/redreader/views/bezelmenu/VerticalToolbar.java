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
import android.widget.ImageButton;
import android.widget.ScrollView;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;

public class VerticalToolbar extends FrameLayout {

	public VerticalToolbar(Context context) {

		super(context);

		setBackgroundColor(Color.argb(192, 0, 0, 0)); // TODO change color based on theme?
		// TODO add light, vertical line on swipe side

		final LinearLayout ll = new LinearLayout(context);
		ll.setOrientation(android.widget.LinearLayout.VERTICAL);

		for(int i = 0; i < 20; i++) {

			final ImageButton ib = new ImageButton(context);
			ib.setImageResource(R.drawable.ic_action_thumb_up_dark);
			ib.setBackgroundColor(Color.TRANSPARENT); // Need to be able to change color if upvoted/downvoted/saved/hidden/etc
			ll.addView(ib);

			final int buttonPadding = General.dpToPixels(context, 12);

			ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding);
		}

		final ScrollView sv = new ScrollView(context);
		sv.addView(ll);
		addView(sv);
	}
}
