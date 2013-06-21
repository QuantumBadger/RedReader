package org.quantumbadger.redreader.views.bezelmenu;

import android.content.Context;
import android.view.MotionEvent;
import android.view.View;
import org.quantumbadger.redreader.common.General;

public class BezelSwipeOverlay extends View {

	private final BezelSwipeListener listener;

	public static enum SwipeEdge {LEFT, RIGHT}

	public BezelSwipeOverlay(Context context, BezelSwipeListener listener) {
		super(context);
		this.listener = listener;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {

		final int action = event.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			final int px = General.dpToPixels(getContext(), 10);

			if(event.getX() < px) {
				return listener.onSwipe(SwipeEdge.LEFT);

			} else if(event.getX() > getWidth() - px) {
				return listener.onSwipe(SwipeEdge.RIGHT);

			} else {
				return listener.onTap();
			}
		}

		return false;
	}

	public interface BezelSwipeListener {
		public boolean onSwipe(SwipeEdge edge);
		public boolean onTap();
	}
}
