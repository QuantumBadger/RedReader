package org.quantumbadger.redreader.ui.views;

import android.graphics.Canvas;

import java.util.ArrayList;

public class RRVerticalLinearLayout extends RRView {

	private final ArrayList<RRView> children = new ArrayList<RRView>(16);

	public void addView(final RRView view) {
		children.add(view);
		view.setParent(this);
	}

	@Override
	public void onRender(final Canvas canvas) {
		for(final RRView child : children) child.onRender(canvas);
	}

	@Override
	public RRView getChildAt(final int x, final int y) {

		for(final RRView child : children) {
			if(child.getYPositionInParent() <= y || child.getOuterHeight() > y) return child;
		}

		return null;
	}

	@Override
	protected int onMeasureByWidth(final int width) {

		int totalHeight = 0;

		for(final RRView child : children) {
			child.setPositionInParent(0, totalHeight);
			totalHeight += child.setWidth(width);
		}

		return totalHeight;
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}
}
