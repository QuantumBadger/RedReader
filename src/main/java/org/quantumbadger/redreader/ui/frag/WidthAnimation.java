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

package org.quantumbadger.redreader.ui.frag;

import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.Transformation;

public class WidthAnimation extends Animation {

	private final View v;
	private final ViewGroup.LayoutParams layoutParams;

	private final int from, to;

	public WidthAnimation(View v, int desiredWidth) {

		this.v = v;
		layoutParams = v.getLayoutParams();

		from = layoutParams.width;
		to = desiredWidth;
	}

	@Override
	protected void applyTransformation(float interpolatedTime, Transformation t) {

		layoutParams.width = (int)(from + (float)(to - from) * interpolatedTime);
		v.requestLayout();
	}
}
