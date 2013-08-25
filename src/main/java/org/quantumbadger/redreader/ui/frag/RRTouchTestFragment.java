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

import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.util.Log;
import android.view.View;
import org.quantumbadger.redreader.ui.views.touch.RRClickHandler;
import org.quantumbadger.redreader.ui.views.touch.RRHSwipeHandler;
import org.quantumbadger.redreader.ui.views.touch.RRSingleTouchViewWrapper;
import org.quantumbadger.redreader.ui.views.touch.RRVSwipeHandler;

public class RRTouchTestFragment extends RRFragment {

	public RRTouchTestFragment(RRContext context, Uri uri, Bundle args, Parcelable state) {
		super(context, uri, args, state);
	}

	@Override
	protected View buildContentView() {

		return new RRSingleTouchViewWrapper(context) {
			public RRClickHandler getClickHandler(float x, float y) {
				return new RRClickHandler() {
					public boolean onHoverBegin(float x, float y) {
						Log.i("RRTouchTestFragment", String.format("onHoverBegin(%.1f, %.1f)", x, y));
						return true;
					}

					public void onHoverEnd(ClickType clickType) {
						Log.i("RRTouchTestFragment", String.format("onHoverEnd(%s)", clickType.name()));
					}
				};
			}

			public RRHSwipeHandler getHSwipeHandler(float x, float y) {
				return new RRHSwipeHandler() {
					public void onHSwipeBegin() {
						Log.i("RRTouchTestFragment", String.format("onHSwipeBegin()"));
					}

					public void onHSwipeDelta(float dx) {
						Log.i("RRTouchTestFragment", String.format("onHSwipeDelta(%.1f)", dx));
					}

					public void onHSwipeEnd() {
						Log.i("RRTouchTestFragment", String.format("onHSwipeEnd()"));
					}
				};
			}

			public RRVSwipeHandler getVSwipeHandler(float x, float y) {
				return new RRVSwipeHandler() {
					public void onVSwipeBegin(long timestamp) {
						Log.i("RRTouchTestFragment", String.format("onVSwipeBegin()"));
					}

					public void onVSwipeDelta(long timestamp, float dx) {
						Log.i("RRTouchTestFragment", String.format("onVSwipeDelta(%.1f)", dx));
					}

					public void onVSwipeEnd(long timestamp) {
						Log.i("RRTouchTestFragment", String.format("onVSwipeEnd()"));
					}
				};
			}
		};
	}
}
