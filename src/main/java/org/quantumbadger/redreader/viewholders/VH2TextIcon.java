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
