package org.quantumbadger.redreader.views.bezelmenu;

import android.content.Context;
import android.view.MotionEvent;
import android.view.View;

public class BezelSwipeOverlay extends View {

	private boolean interceptTaps = false;
	private final BezelSwipeListener listener;

	public BezelSwipeOverlay(Context context, BezelSwipeListener listener) {
		super(context);
		this.listener = listener;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {

		final int action = event.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			if(interceptTaps) {
				interceptTaps = listener.onTap();
				return true;

			} else if(event.getX() < 5) {
				interceptTaps = listener.onLeftSwipe();
				return true;

			} else if(event.getX() > getWidth() - 5) {
				interceptTaps = listener.onRightSwipe();
				return true;

			}
		}

		return false;
	}

	public interface BezelSwipeListener {
		public boolean onLeftSwipe();
		public boolean onRightSwipe();
		public boolean onTap();
	}
}
