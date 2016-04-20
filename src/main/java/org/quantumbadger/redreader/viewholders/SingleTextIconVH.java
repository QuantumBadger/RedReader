package org.quantumbadger.redreader.viewholders;

import android.view.View;
import android.widget.ImageView;

import org.quantumbadger.redreader.R;

public class SingleTextIconVH extends SingleTextVH {

	public final ImageView icon;

	public SingleTextIconVH(View itemView) {
		super(itemView);

		icon = (ImageView) itemView.findViewById(R.id.recycler_item_icon);
	}
}
