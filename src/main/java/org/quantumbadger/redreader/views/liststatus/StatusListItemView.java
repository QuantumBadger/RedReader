/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.views.liststatus;

import android.content.Context;
import android.view.View;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.views.list.RRTouchable;

public class StatusListItemView extends FrameLayout implements RRTouchable {

	protected final float dpScale;

	private View contents = null;

	public StatusListItemView(final Context context) {
		super(context);
		dpScale = context.getResources().getDisplayMetrics().density; // TODO xml?
	}

	public void setContents(final View contents) {
		if(this.contents != null) removeView(this.contents);
		this.contents = contents;
		addView(contents);
	}

	public void hideNoAnim() {

		setVisibility(GONE);
		removeAllViews();
		contents = null;

		requestLayout();
	}

	public void rrOnClick(final int x, final int y) {}
	public void rrOnLongClick() {}
	public void rrOnFingerDown() {}
	public void rrOnSwipeDelta(final float dx) {}
	public void rrOnFingerUp() {}

	public boolean rrAllowLongClick() {
		return false;
	}
}
