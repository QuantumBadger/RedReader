/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.ui.views.touch;

import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRSchedulerManager;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.ui.frag.RRContext;
import org.quantumbadger.redreader.ui.frag.RRViewWrapper;

public abstract class RRSingleTouchViewWrapper extends RRViewWrapper implements RRSingleTouchHandlerProvider {

	private enum GlobalTouchDetectionState { UNLOCKED, NOT_A_CLICK, IGNORE, IN_H_SWIPE, IN_V_SWIPE }

	private GlobalTouchDetectionState currentState = GlobalTouchDetectionState.UNLOCKED;

	private RRClickHandler clickHandler;
	private RRHSwipeHandler hSwipeHandler;
	private RRVSwipeHandler vSwipeHandler;

	private RRSchedulerManager.RRSingleTaskScheduler longClickScheduler;
	private final Runnable longClickSchedulerRunnable = new Runnable() {
		public void run() {
			onLongClickTimerExpired();
		}
	};

	private final RRContext context;

	public RRSingleTouchViewWrapper(final RRContext context) {
		super(context.activity);
		this.context = context;
		longClickScheduler = context.scheduler.obtain();
	}

	private void onLongClickTimerExpired() {
		if(currentState == GlobalTouchDetectionState.UNLOCKED) {
			currentState = GlobalTouchDetectionState.IGNORE;
			clickHandler.onHoverEnd(RRClickHandler.ClickType.LONG_CLICK);
		}
	}

	@Override
	protected void onTouchEvent(TouchEvent e) {

		//		Allow for unlimited h/v swipes, but if two handlers are equal, merge them and supply the average position
		//		Allow for h swipes to be disabled - revert to v swipe

		switch(e.type) {

			case BEGIN:

				// TODO if scrolling was in progress, NOT_A_CLICK, and set velocity = 0 (use hoverstart in listview)

				if(currentState != GlobalTouchDetectionState.UNLOCKED) {
					throw new UnexpectedInternalStateException();
				}

				if(e.pointerCount != 1) {
					currentState = GlobalTouchDetectionState.NOT_A_CLICK;

				} else {

					clickHandler = getClickHandler((int)e.xPos[0], (int)e.yPos[0]);

					if(clickHandler == null || !clickHandler.onHoverBegin(e.xPos[0], e.yPos[0])) {
						currentState = GlobalTouchDetectionState.NOT_A_CLICK;
						clickHandler = null;
					} else {
						longClickScheduler.setSchedule(longClickSchedulerRunnable, 300, true);
					}
				}

				break;

			case MOVE:

				switch(currentState) {

					case UNLOCKED:
					case NOT_A_CLICK:

						final double euclideanDistanceSquared = e.totalDistanceMovedSquared(0);

						if(euclideanDistanceSquared > General.sq(13f * context.dpScale)) {

							longClickScheduler.cancel();
							if(clickHandler != null) {
								clickHandler.onHoverEnd(RRClickHandler.ClickType.CANCEL);
								clickHandler = null;
							}

							if(Math.abs(e.totalDistanceMovedX(0)) > Math.abs(e.totalDistanceMovedY(0))) {
								hSwipeHandler = getHSwipeHandler((int)e.xPos[0], (int)e.yPos[0]);

								if(hSwipeHandler != null) {
									hSwipeHandler.onHSwipeBegin(e.currentTime);
									currentState = GlobalTouchDetectionState.IN_H_SWIPE;
								} else {
									currentState = GlobalTouchDetectionState.IGNORE;
									// TODO try v swipe
								}

							} else {
								vSwipeHandler = getVSwipeHandler((int)e.xPos[0], (int)e.yPos[0]);

								if(vSwipeHandler != null) {
									vSwipeHandler.onVSwipeBegin(e.currentTime);
									currentState = GlobalTouchDetectionState.IN_V_SWIPE;
								} else {
									currentState = GlobalTouchDetectionState.IGNORE;
								}
							}
						}

						break;

					case IN_H_SWIPE:
						hSwipeHandler.onHSwipeDelta(e.currentTime, e.xDelta(0));
						break;

					case IN_V_SWIPE:
						vSwipeHandler.onVSwipeDelta(e.currentTime, e.yDelta(0));
						break;

					case IGNORE:
						break;

					default:
						throw new UnexpectedInternalStateException();
				}

				break;

			case FINISH:
			case CANCEL:

				longClickScheduler.cancel();

				switch(currentState) {
					case UNLOCKED:
						if(e.pointerCount != 1) throw new UnexpectedInternalStateException();
						clickHandler.onHoverEnd(e.type == TouchEventType.CANCEL
										? RRClickHandler.ClickType.CANCEL
										: RRClickHandler.ClickType.CLICK);
						break;

					case IN_H_SWIPE:
						hSwipeHandler.onHSwipeEnd(e.currentTime, getXVelocity());
						break;

					case IN_V_SWIPE:
						vSwipeHandler.onVSwipeEnd(e.currentTime, getYVelocity());
						break;

					case NOT_A_CLICK:
					case IGNORE:
						break;

					default:
						throw new UnexpectedInternalStateException();
				}

				currentState = GlobalTouchDetectionState.UNLOCKED;
				clickHandler = null;
				hSwipeHandler = null;
				vSwipeHandler = null;

				break;
		}

	}
}
