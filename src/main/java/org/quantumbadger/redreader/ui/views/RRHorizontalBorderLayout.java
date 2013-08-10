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

		canvas.save();

		left.draw(canvas);

		canvas.translate(left.getWidth(), 0);
		canvas.save();
		canvas.translate(0, (getHeight() - middle.getHeight()) / 2);
		middle.draw(canvas);
		canvas.restore();

		canvas.translate(middle.getWidth(), 0);
		right.draw(canvas);

		canvas.restore();
	}

	@Override
	protected void handleTouchEvent(int eventType, int x, int y) {

		if(x < left.getWidth()) {
			left.onTouchEvent(eventType, x, y);

		} else if(x >= getWidth() - right.getWidth()) {
			right.onTouchEvent(eventType, x - left.getWidth() - middle.getWidth(), y);

		} else {
			middle.onTouchEvent(eventType, x - left.getWidth(), y);
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
