package org.quantumbadger.redreader.ui.frag;

import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.text.TextPaint;
import android.view.View;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.ui.RRContext;
import org.quantumbadger.redreader.ui.list.RRListItemViewWrapper;
import org.quantumbadger.redreader.ui.list.RRListView;
import org.quantumbadger.redreader.ui.views.RRTextView;

import java.util.UUID;

public class RRListTestFragment extends RRFragment {

	private RRListView lv;

	public RRListTestFragment(RRContext context, Uri uri, Bundle args, Parcelable state) {
		super(context, uri, args, state);
	}

	@Override
	protected View buildContentView() {

		lv = new RRListView(context);

		final TextPaint textPaint = new TextPaint();
		textPaint.setColor(Color.WHITE);
		textPaint.setTextSize(50);
		textPaint.setAntiAlias(true);

		for(int i = 0; i < 100; i++) {

			final RRTextView textView = new RRTextView();
			textView.setText(i + ". " + UUID.randomUUID().toString());
			textView.setTextPaint(textPaint);
			textView.setPadding(General.dpToPixels(context.activity, 8));

			final RRListItemViewWrapper itemViewWrapper = new RRListItemViewWrapper(textView);
			lv.getContents().appendChild(itemViewWrapper);
		}

		return lv;
	}

	@Override
	public void onResume() {
		lv.resume();
	}

	@Override
	public void onPause() {
		lv.pause();
	}
}
