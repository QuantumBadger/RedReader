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

package org.quantumbadger.redreader.views.liststatus;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.Transformation;
import org.holoeverywhere.widget.FrameLayout;
import org.quantumbadger.redreader.views.list.RRTouchable;

import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class StatusListItemView extends FrameLayout implements RRTouchable {

	protected final float dpScale;

	private View contents = null;

	public StatusListItemView(final Context context) {

		super(context);

		dpScale = context.getResources().getDisplayMetrics().density; // TODO xml?
	}

	public void setContents(final View contents) {
		if(this.contents != null) removeView(this.contents);
		this.contents = contents;
		addView(contents);
	}

	public void hideNoAnim() {

		clearAnimation();
		setVisibility(GONE);
		removeAllViews();
		contents = null;

		requestLayout();
	}

	public void hide(final long initialDelay) {

		if(contents == null) return;

		final long heightAnimMs = 500;

		//AlphaAnimation alphaAnim = new AlphaAnimation(0);
		//alphaAnim.setDuration(500);
		//alphaAnim.setStartOffset(initialDelay);

		final HeightAnimation heightAnim = new HeightAnimation(0);
		heightAnim.setDuration(heightAnimMs);
		heightAnim.setStartOffset(initialDelay);

		//AnimationSet anim = new AnimationSet(false);

		//anim.addAnimation(alphaAnim);
		//anim.addAnimation(heightAnim);

		//startAnimation(anim);
		startAnimation(heightAnim);

		Executors.newSingleThreadScheduledExecutor().schedule(new Runnable() {
			public void run() {
				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						clearAnimation();
						setVisibility(GONE);
						removeAllViews();

						if(contents != null) {
							((LayoutParams) contents.getLayoutParams()).height = 0;
							contents = null;
						}

						requestLayout();
					}
				});
			}
		}, initialDelay + heightAnimMs, TimeUnit.MILLISECONDS);
	}

	public void rrOnClick(final int x, final int y) {}
	public void rrOnLongClick() {}
	public void rrOnFingerDown() {}
	public void rrOnSwipeDelta(final float dx) {}
	public void rrOnFingerUp() {}

	public boolean rrAllowLongClick() {
		return false;
	}

	private class HeightAnimation extends Animation {

		private final int startHeight, endHeight;

		private final LayoutParams layoutParams = (LayoutParams)contents.getLayoutParams();

		public HeightAnimation(final int endHeight) {
			setInterpolator(new AccelerateDecelerateInterpolator());
			startHeight = getHeight();
			this.endHeight = endHeight;
		}

		@Override
		protected void applyTransformation(final float interpolatedTime, final Transformation t) {
			layoutParams.height = (int)(startHeight + ((endHeight - startHeight) * interpolatedTime));
			requestLayout();
		}

		@Override
		public boolean willChangeBounds() {
			return true;
		}
	}

	private class AlphaAnimation extends Animation {

		private final float startAlpha, endAlpha;

		public AlphaAnimation(final int endAlpha) {
			setInterpolator(new AccelerateInterpolator());
			startAlpha = getAlpha();
			this.endAlpha = endAlpha;
		}

		@Override
		protected void applyTransformation(final float interpolatedTime, final Transformation t) {
			setAlpha(startAlpha + ((endAlpha - startAlpha) * interpolatedTime));
		}
	}
}
