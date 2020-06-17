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
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import androidx.annotation.IntDef;
import android.view.MotionEvent;
import android.view.View;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class BezelSwipeOverlay extends View {

	public static final int LEFT = 0;
	public static final int RIGHT = 1;

	@IntDef({LEFT, RIGHT})
	@Retention(RetentionPolicy.SOURCE)
	public @interface SwipeEdge {}

	private final BezelSwipeListener listener;

	private final int mSwipeZonePixels;

	public BezelSwipeOverlay(Context context, BezelSwipeListener listener) {
		super(context);
		this.listener = listener;

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		final int swipeZoneDp = PrefsUtility.pref_behaviour_bezel_toolbar_swipezone_dp(context, prefs);

		mSwipeZonePixels = General.dpToPixels(getContext(), swipeZoneDp);
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {

		final int action = event.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			if(event.getX() < mSwipeZonePixels) {
				return listener.onSwipe(LEFT);

			} else if(event.getX() > getWidth() - mSwipeZonePixels) {
				return listener.onSwipe(RIGHT);

			} else {
				return listener.onTap();
			}
		}

		return false;
	}

	public interface BezelSwipeListener {
		boolean onSwipe(@SwipeEdge int edge);

		boolean onTap();
	}
}
