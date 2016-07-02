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

package org.quantumbadger.redreader.viewholders;

import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import org.quantumbadger.redreader.R;

/**
 * A view holder for a two line, text and icon list item.
 */
public class VH2TextIcon extends VH {

	public final TextView text;
	public final TextView text2;
	public final ImageView icon;

	public long bindingId = 0;

	public VH2TextIcon(View itemView) {
		super(itemView);

		text = (TextView) itemView.findViewById(R.id.recycler_item_text);
		text2 = (TextView) itemView.findViewById(R.id.recycler_item_2_text);
		icon = (ImageView) itemView.findViewById(R.id.recycler_item_icon);
	}
}
