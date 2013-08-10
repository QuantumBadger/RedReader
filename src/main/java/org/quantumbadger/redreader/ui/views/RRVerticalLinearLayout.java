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

		canvas.save();

		for(final RRView child : children) {
			child.onRender(canvas);
			canvas.translate(0, child.getHeight());
		}

		canvas.restore();
	}

	@Override
	protected void handleTouchEvent(int eventType, int x, int y) {

		int yPos = 0;

		for(final RRView child : children) {
			final int nextYPos = yPos + child.getHeight();
			if(y >= yPos && y < nextYPos) {
				child.onTouchEvent(eventType, x, y - yPos);
				return;
			}
		}
	}

	@Override
	protected int onMeasureByWidth(final int width) {

		int totalHeight = 0;

		for(final RRView child : children) {
			totalHeight += child.setWidth(width);
		}

		return totalHeight;
	}

	@Override
	protected int onMeasureByHeight(int height) {
		throw new MeasurementException(this, MeasurementException.InvalidMeasurementType.HEIGHT_DETERMINED_BY_WIDTH);
	}
}
