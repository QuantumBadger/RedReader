package org.quantumbadger.redreader.ui.list;

import android.graphics.Canvas;
import org.quantumbadger.redreader.ui.views.MeasurementException;
import org.quantumbadger.redreader.ui.views.RRView;

public class RRListItemViewWrapper extends RRListViewItem {

	private final RRView view;
	private boolean isVisible = true;

	public RRListItemViewWrapper(final RRView view) {
		this.view = view;
		view.setParent(this);
	}

	@Override
	protected void onRender(final Canvas c) {
		view.draw(c);
	}

	@Override
	protected int onMeasureByWidth(int width) {
		view.setPositionInParent(0, 0);
		return view.setWidth(width);
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}

	@Override
	public boolean isVisible() {
		return isVisible;
	}

	public void setVisible(final boolean isVisible) {
		this.isVisible = isVisible;
	}
}
