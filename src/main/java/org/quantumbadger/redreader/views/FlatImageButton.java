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
import android.graphics.Color;
import android.widget.ImageButton;

public class FlatImageButton extends ImageButton {

	private int backgroundColor;

	public FlatImageButton(Context context) {
		super(context);
		backgroundColor = Color.TRANSPARENT;
		setBackgroundColor(backgroundColor);
	}

	@Override
	protected void drawableStateChanged() {

		if(isPressed()) {
			setBackgroundResource(android.R.color.holo_blue_dark);
		} else {
			setBackgroundColor(backgroundColor);
		}

		super.drawableStateChanged();
	}

	@Override
	public void setBackgroundColor(int color) {
		this.backgroundColor = color;
		super.setBackgroundColor(color);
	}
}
