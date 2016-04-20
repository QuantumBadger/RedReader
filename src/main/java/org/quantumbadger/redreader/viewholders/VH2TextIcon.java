package org.quantumbadger.redreader.viewholders;

import android.view.View;
import android.widget.TextView;

import org.quantumbadger.redreader.R;

public class VH2TextIcon extends VH1TextIcon {

	public final TextView text2;

	public VH2TextIcon(View itemView) {
		super(itemView);

		text2 = (TextView) itemView.findViewById(R.id.recycler_item_2_text);
	}
}
