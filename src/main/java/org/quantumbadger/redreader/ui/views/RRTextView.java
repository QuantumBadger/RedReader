package org.quantumbadger.redreader.ui.views;

import android.graphics.Canvas;
import android.text.Layout;
import android.text.StaticLayout;
import android.text.TextPaint;

public class RRTextView extends RRView {

	private StaticLayout staticLayout;

	private TextPaint textPaint;
	private CharSequence text;

	public RRTextView(final CharSequence text, final TextPaint textPaint) {
		this.text = text;
		this.textPaint = textPaint;
	}

	public void setText(CharSequence text) {
		this.text = text;
		staticLayout = null;
	}

	public void setTextPaint(TextPaint textPaint) {
		this.textPaint = textPaint;
		staticLayout = null;
	}

	@Override
	protected void onRender(Canvas canvas) {
		staticLayout.draw(canvas);
	}

	@Override
	protected void handleTouchEvent(int eventType, int x, int y) {
	}

	@Override
	protected int onMeasureByWidth(int width) {
		staticLayout = new StaticLayout(text, textPaint, width, Layout.Alignment.ALIGN_NORMAL, 1, 0, false);
		return staticLayout.getHeight();
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}
}
