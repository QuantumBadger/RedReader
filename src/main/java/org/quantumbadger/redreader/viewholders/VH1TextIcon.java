package org.quantumbadger.redreader.viewholders;

import android.view.View;
import android.widget.ImageView;

import org.quantumbadger.redreader.R;

public class VH1TextIcon extends VH1Text {

	public final ImageView icon;

	public VH1TextIcon(View itemView) {
		super(itemView);

		icon = (ImageView) itemView.findViewById(R.id.recycler_item_icon);
	}
}
