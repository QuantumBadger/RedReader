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

package org.quantumbadger.redreader.views.liststatus;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
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

		final ViewGroup.LayoutParams layoutParams = contents.getLayoutParams();
		layoutParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
		layoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	public void hideNoAnim() {

		setVisibility(GONE);
		removeAllViews();
		contents = null;

		requestLayout();
	}

	@Override public void rrOnClick(final int x, final int y) {}
	@Override public void rrOnLongClick() {}
	@Override public void rrOnFingerDown() {}
	@Override public void rrOnSwipeDelta(final float dx) {}
	@Override public void rrOnFingerUp() {}
	@Override public void rrOnHighlightStart(final int x, final int y) {}
	@Override public void rrOnHighlightEnd() {}

	@Override
	public boolean rrAllowLongClick() {
		return false;
	}
}
