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

package org.saiditnet.redreader.views.bezelmenu;

import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

public class SideToolbarOverlay extends FrameLayout {

	private View contents;
	private SideToolbarPosition shownPosition = null;

	public enum SideToolbarPosition {
		LEFT, RIGHT
	}

	public SideToolbarOverlay(Context context) {
		super(context);
	}

	public void setContents(View contents) {
		this.contents = contents;
		if(shownPosition != null) {
			show(shownPosition);
		}
	}

	public void show(SideToolbarPosition pos) {

		removeAllViews();
		addView(contents);

		((LayoutParams)contents.getLayoutParams()).gravity = (pos == SideToolbarPosition.LEFT ? Gravity.LEFT : Gravity.RIGHT);
		contents.getLayoutParams().width = ViewGroup.LayoutParams.WRAP_CONTENT;
		contents.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		shownPosition = pos;
	}

	public void hide() {
		shownPosition = null;
		removeAllViews();
	}

	public boolean isShown() {
		return shownPosition != null;
	}
}
