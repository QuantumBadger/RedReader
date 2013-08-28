package org.quantumbadger.redreader.ui.views;

import android.graphics.Canvas;
import android.util.Log;
import org.quantumbadger.redreader.ui.views.touch.RRClickHandler;
import org.quantumbadger.redreader.ui.views.touch.RRHSwipeHandler;

public class RRSwipableView extends RRView implements RRHSwipeHandler {

	private final RRView child;
	private float xPos = 0, xVel = 0;

	public RRSwipableView(RRView child) {
		this.child = child;
		child.setParent(this);
	}

	@Override
	protected void onRender(Canvas canvas) {

		if(Math.abs(xVel) > 0.2 || Math.abs(xPos) > 1) {
			xVel *= 0.95;
			xVel += findAcceleration() / 60f;
			setXPos(xPos + xVel / 60f); // TODO detect elapsed time
			rrInvalidate();
		} else {
			xVel = 0;
			xPos = 0;
		}

		child.draw(canvas);
	}

	private float findAcceleration() {
		return -5 * xPos; // DPI?
	}

	@Override
	protected int onMeasureByWidth(int width) {
		child.setPositionInParent((int)xPos, 0);
		return child.setWidth(width);
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}

	@Override
	public RRHSwipeHandler getHSwipeHandlerTraversingDown() {
		Log.i("RRSwipableView", "getHSwipeHandlerTraversingDown");
		return this;
	}

	@Override
	public RRClickHandler getClickHandlerTraversingDown() {
		xVel = 0;
		return null;
	}

	private void setXPos(float xPos) {
		this.xPos = xPos;
		child.setPositionInParent((int)xPos, 0);
		rrInvalidate();
	}

	public void onHSwipeBegin(long timestamp) {
	}

	public void onHSwipeDelta(long timestamp, float dx) {
		setXPos(xPos + dx);
	}

	public void onHSwipeEnd(long timestamp, float xVelocity) {
		xVel = xVelocity;
		rrInvalidate();
	}

	@Override
	public RRView getChildAt(int x, int y) {
		return child;
	}
}
