package org.quantumbadger.redreader.ui.frag;

import android.graphics.Canvas;
import org.quantumbadger.redreader.ui.list.RRListViewItem;
import org.quantumbadger.redreader.ui.views.RRView;
import org.quantumbadger.redreader.ui.views.RRViewParent;

public class RRListItemViewWrapper extends RRListViewItem implements RRViewParent {

	private final RRView view;
	private boolean isVisible = true;

	public RRListItemViewWrapper(final RRView view) {
		this.view = view;
		view.setParent(this);
	}

	@Override
	protected int onMeasureHeight(final int width) {
		return view.setWidth(width);
	}

	@Override
	protected void onRender(final Canvas c) {
		view.draw(c);
	}

	@Override
	public boolean isVisible() {
		return isVisible;
	}

	public void setVisible(final boolean isVisible) {
		this.isVisible = isVisible;
	}
}
