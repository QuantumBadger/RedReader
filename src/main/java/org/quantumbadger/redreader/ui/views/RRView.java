package org.quantumbadger.redreader.ui.views;


import android.graphics.Canvas;
import android.graphics.Paint;

public abstract class RRView implements RRViewParent, TouchEventHandler {

	private RRViewParent parent;
	private TouchEventHandler touchEventHandler;

	protected int paddingTop, paddingBottom, paddingLeft, paddingRight;
	protected Paint paddingPaint = null, backgroundPaint = null;

	private int width = -1, height = -1;

	public static final int HOVER_START = 1, HOVER_HIGHLIGHT = 2, HOVER_LONGCLICK = 3, HOVER_CANCEL = 4, TAP = 5;
	public static final int UNSPECIFIED = -1;

	public final void draw(final Canvas canvas) {

		if(paddingPaint != null) {
			canvas.drawRect(0, 0, width, paddingTop, paddingPaint);
			canvas.drawRect(0, height - paddingBottom, width, height, paddingPaint);
			canvas.drawRect(0, paddingTop, paddingLeft, height - paddingBottom, paddingPaint);
			canvas.drawRect(width - paddingRight, paddingTop, width, height - paddingBottom, paddingPaint);
		}

		if(backgroundPaint != null) {
			canvas.drawRect(paddingLeft, paddingTop, width - paddingRight, height - paddingBottom, backgroundPaint);
		}

		canvas.save();
		canvas.translate(paddingLeft, paddingTop);

		onRender(canvas);

		canvas.restore();
	}

	protected abstract void onRender(Canvas canvas);

	public final void invalidate() {
		width = -1;
		parent.invalidate();
	}

	public final void setTouchEventHandler(TouchEventHandler touchEventHandler) {
		this.touchEventHandler = touchEventHandler;
	}

	public final void onTouchEvent(int eventType, int x, int y) {

		if(touchEventHandler != null) {
			touchEventHandler.onTouchEvent(eventType, x - paddingLeft, y - paddingTop);
		} else {
			handleTouchEvent(eventType, x - paddingLeft, y - paddingTop);
		}
	}

	protected abstract void handleTouchEvent(int eventType, int x, int y);

	public final int setWidth(final int width) {

		if(this.width == width) return height;

		final int widthMinusPadding = width - paddingLeft - paddingRight;

		final int fixedWidth = getFixedWidth();

		if(fixedWidth != UNSPECIFIED && fixedWidth != widthMinusPadding) {
			throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.WIDTH_IS_FIXED);
		}

		final int heightMinusPadding = onMeasureByWidth(widthMinusPadding);

		height = heightMinusPadding + paddingTop + paddingBottom;
		this.width = width;

		return height;
	}

	public final int setHeight(final int height) {

		if(this.height == height) return width;

		final int fixedHeight = getFixedHeight();

		final int heightMinusPadding = height - paddingTop - paddingBottom;

		if(fixedHeight != UNSPECIFIED && fixedHeight != heightMinusPadding) {
			throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_IS_FIXED);
		}

		final int widthMinusPadding = onMeasureByHeight(heightMinusPadding);

		width = widthMinusPadding + paddingLeft + paddingRight;
		this.height = height;

		return width;
	}

	public int getWidth() {
		return width - paddingLeft - paddingRight;
	}

	public int getHeight() {
		return height - paddingTop - paddingBottom;
	}

	protected abstract int onMeasureByWidth(int width);
	protected abstract int onMeasureByHeight(int height);

	protected int getFixedWidth() {
		return UNSPECIFIED;
	}

	protected int getFixedHeight() {
		return UNSPECIFIED;
	}

	protected int getMinWidth() {
		return getFixedWidth();
	}

	protected int getMinHeight() {
		return getFixedHeight();
	}

	public void setParent(RRViewParent parent) {
		this.parent = parent;
	}
}
