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

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.ContextMenu;
import android.view.View;
import android.widget.ImageView;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;

// TODO just make this a linear layout
public class ListItemView extends FrameLayout {

	private final TextView textView;
	private final ImageView imageView;
	private final View divider;

	private ContextMenuBuilder contextMenuBuilder;

	public ListItemView(final Context context) {

		super(context);

		final LinearLayout ll = (LinearLayout)inflate(context, R.layout.list_item, null);

		divider = ll.findViewById(R.id.list_item_divider);
		textView = (TextView)ll.findViewById(R.id.list_item_text);
		imageView = (ImageView)ll.findViewById(R.id.list_item_icon);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		addView(ll);
	}

	public void reset(final Drawable icon, final CharSequence text, final boolean hideDivider) {

		if(hideDivider) {
			divider.setVisibility(View.GONE);
		} else {
			divider.setVisibility(View.VISIBLE);
		}

		textView.setText(text);

		if(icon != null) {
			imageView.setImageDrawable(icon);
			imageView.setVisibility(VISIBLE);
		} else {
			imageView.setImageBitmap(null);
			imageView.setVisibility(GONE);
		}

		contextMenuBuilder = null;
	}

	public void setContextMenuBuilder(ContextMenuBuilder contextMenuBuilder) {
		this.contextMenuBuilder = contextMenuBuilder;
	}

	@Override
	protected void onCreateContextMenu(ContextMenu menu) {
		if(contextMenuBuilder != null) contextMenuBuilder.build(menu);
	}

	public static interface ContextMenuBuilder {
		public void build(ContextMenu menu);
	}
}
