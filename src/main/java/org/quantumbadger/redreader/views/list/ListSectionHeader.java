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
import android.widget.FrameLayout;
import android.widget.TextView;

import org.quantumbadger.redreader.R;

// TODO doesn't need to be in a frame layout
public class ListSectionHeader extends FrameLayout {

	private final TextView textView;

	public ListSectionHeader(final Context context) {

		super(context);
		textView = (TextView) inflate(context, R.layout.list_sectionheader, null);
		addView(textView);
	}

	public void reset(final String text) {
		textView.setText(text);
	}

	public void setColor(int color) {
		textView.setTextColor(color);
	}
}
