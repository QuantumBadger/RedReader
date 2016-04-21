package org.quantumbadger.redreader.views.bezelmenu;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.support.annotation.IntDef;
import android.view.MotionEvent;
import android.view.View;

import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class BezelSwipeOverlay extends View {

	public static final int LEFT = 0;
	public static final int RIGHT = 1;

	@IntDef({LEFT, RIGHT})
	@Retention(RetentionPolicy.SOURCE)
	public @interface SwipeEdge {}

	private final BezelSwipeListener listener;

	private final int mSwipeZonePixels;

	public BezelSwipeOverlay(Context context, BezelSwipeListener listener) {
		super(context);
		this.listener = listener;

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		final int swipeZoneDp = PrefsUtility.pref_behaviour_bezel_toolbar_swipezone_dp(context, prefs);

		mSwipeZonePixels = General.dpToPixels(getContext(), swipeZoneDp);
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {

		final int action = event.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			if(event.getX() < mSwipeZonePixels) {
				return listener.onSwipe(LEFT);

			} else if(event.getX() > getWidth() - mSwipeZonePixels) {
				return listener.onSwipe(RIGHT);

			} else {
				return listener.onTap();
			}
		}

		return false;
	}

	public interface BezelSwipeListener {
		boolean onSwipe(@SwipeEdge int edge);

		boolean onTap();
	}
}
