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

import android.view.View;
import android.view.ViewGroup;

public class RRAnimationShrinkHeight extends RRAnimation {

	private static final long DURATION_NANOS = 500L * 1000 * 1000;

	private final View mTarget;
	private final ViewGroup.LayoutParams mLayoutParams;
	private final int mStartHeight;

	public RRAnimationShrinkHeight(final View target) {
		mTarget = target;
		mLayoutParams = mTarget.getLayoutParams();
		mStartHeight = mTarget.getMeasuredHeight();
	}

	@Override
	protected boolean handleFrame(final long nanosSinceAnimationStart) {

		mLayoutParams.height = (int)(mStartHeight * interpolateSine(
				1.0 - (double)nanosSinceAnimationStart / (double)DURATION_NANOS));

		mTarget.setLayoutParams(mLayoutParams);

		final boolean finished = nanosSinceAnimationStart > DURATION_NANOS;

		if(finished) {
			mTarget.setVisibility(View.GONE);
		}

		return !finished;
	}

	private static double interpolateSine(final double fraction) {
		return 0.5 + Math.sin((fraction - 0.5) * Math.PI) / 2;
	}
}
