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

		final int leftFixedWidth = left.getFixedWidth();
		final int rightFixedWidth = right.getFixedWidth();
		final int middleWidth = getInnerWidth() - leftFixedWidth - rightFixedWidth;

		canvas.save();

		left.draw(canvas, leftFixedWidth);

		canvas.translate(leftFixedWidth, 0);
		canvas.save();
		canvas.translate(0, (getInnerHeight() - middle.getOuterHeight()) / 2);
		middle.draw(canvas, middleWidth);
		canvas.restore();

		canvas.translate(middleWidth, 0);
		right.draw(canvas, rightFixedWidth);

		canvas.restore();
	}

	@Override
	protected void handleTouchEvent(int eventType, int x, int y) {

		if(x < left.getOuterWidth()) {
			left.onTouchEvent(eventType, x, y);

		} else if(x >= getInnerWidth() - right.getOuterWidth()) {
			right.onTouchEvent(eventType, x - left.getOuterWidth() - middle.getOuterWidth(), y);

		} else {
			middle.onTouchEvent(eventType, x - left.getOuterWidth(), y);
		}

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

		return height;
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}
}
