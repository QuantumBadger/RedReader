package org.quantumbadger.redreader.ui.frag;

import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.text.TextPaint;
import android.view.View;
import org.quantumbadger.redreader.ui.RRContext;
import org.quantumbadger.redreader.ui.list.RRListView;
import org.quantumbadger.redreader.ui.views.RRTextView;

public class RRListTestFragment extends RRFragment {

	public RRListTestFragment(RRContext context, Uri uri, Bundle args, Parcelable state) {
		super(context, uri, args, state);
	}

	@Override
	protected View buildContentView() {

		final RRListView lv = new RRListView(context);

		final TextPaint textPaint = new TextPaint();
		textPaint.setColor(Color.WHITE);
		textPaint.setTextSize(30);

		for(int i = 0; i < 5; i++) {

			final RRTextView textView = new RRTextView();
			textView.setText("Hello World " + i);

			final RRListItemViewWrapper itemViewWrapper = new RRListItemViewWrapper(textView);
			lv.getContents().appendChild(itemViewWrapper);
		}

		return lv;
	}
}
