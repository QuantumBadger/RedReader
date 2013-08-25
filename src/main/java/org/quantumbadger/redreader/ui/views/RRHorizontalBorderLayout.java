package org.quantumbadger.redreader.ui.views;

import android.graphics.Canvas;

public final class RRHorizontalBorderLayout extends RRView {

	private RRView left, middle, right;

	public RRHorizontalBorderLayout(RRView left, RRView middle, RRView right) {

		this.left = left;
		this.middle = middle;
		this.right = right;

		left.setParent(this);
		middle.setParent(this);
		right.setParent(this);
	}

	@Override
	protected void onRender(final Canvas canvas) {
		left.draw(canvas);
		middle.draw(canvas);
		right.draw(canvas);
	}

	@Override
	protected int onMeasureByWidth(int width) {

		final int leftFixedWidth = left.getFixedWidth();
		final int rightFixedWidth = right.getFixedWidth();

		if(leftFixedWidth < 0)
			throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.EXPECTING_FIXED_WIDTH);

		if(rightFixedWidth < 0)
			throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.EXPECTING_FIXED_WIDTH);

		final int middleWidth = width - leftFixedWidth - rightFixedWidth;
		final int middleHeight = middle.setWidth(middleWidth);

		final int height = Math.max(middleHeight, Math.max(left.getMinHeight(), right.getMinHeight()));

		left.setHeight(height);
		right.setHeight(height);

		left.setPositionInParent(0, 0);
		middle.setPositionInParent(leftFixedWidth, (getInnerHeight() - middle.getOuterHeight()) / 2);
		right.setPositionInParent(leftFixedWidth + middleWidth, 0);

		return height;
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}

	@Override
	public RRView getChildAt(int x, int y) {
		if(y < left.getOuterWidth()) return left;
		else if(y < left.getOuterWidth() + middle.getOuterWidth()) return middle;
		else return right;
	}
}
