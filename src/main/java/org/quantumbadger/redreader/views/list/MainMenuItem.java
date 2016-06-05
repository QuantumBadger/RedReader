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

import android.graphics.drawable.Drawable;
import android.view.View;

public final class MainMenuItem implements View.OnClickListener {

	public final String title;
	public final Drawable icon;
	public final boolean isHeader;
	private final View.OnClickListener clickListener;
	private final View.OnLongClickListener longClickListener;

	public MainMenuItem(final String title) {
		this.title = title;
		icon = null;
		isHeader = true;
		clickListener = null;
		longClickListener = null;
	}

	public MainMenuItem(final String title, final Drawable icon, final View.OnClickListener clickListener, final View.OnLongClickListener longClickListener) {
		this.title = title;
		this.icon = icon;
		isHeader = false;
		this.clickListener = clickListener;
		this.longClickListener = longClickListener;
	}

	@Override
	public void onClick(final View view) {
		if(clickListener != null) clickListener.onClick(view);
	}

	public void onLongClick(final View view) {
		if(longClickListener != null) longClickListener.onLongClick(view);
	}
}
