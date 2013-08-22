package org.quantumbadger.redreader.ui.views;

import android.graphics.Canvas;
import org.quantumbadger.redreader.ui.views.touch.RRHSwipeHandler;
import org.quantumbadger.redreader.ui.views.touch.RROffsetClickHandler;
import org.quantumbadger.redreader.ui.views.touch.RRVSwipeHandler;

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
			canvas.translate(0, child.getOuterHeight());
		}

		canvas.restore();
	}

	@Override
	public RROffsetClickHandler getClickHandler(float x, float y) {
		// TODO
		return null;
	}

	public RRHSwipeHandler getHSwipeHandler(float x, float y) {
		// TODO
		return null;
	}

	public RRVSwipeHandler getVSwipeHandler(float x, float y) {
		// TODO
		return null;
	}
/*
	@Override
	protected void handleTouchEvent(int eventType, int x, int y) {

		int yPos = 0;

		for(final RRView child : children) {
			final int nextYPos = yPos + child.getOuterHeight();
			if(y >= yPos && y < nextYPos) {
				child.onTouchEvent(eventType, x, y - yPos);
				return;
			}
		}
	}*/

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
