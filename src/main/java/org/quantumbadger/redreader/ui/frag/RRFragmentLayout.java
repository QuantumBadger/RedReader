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

package org.quantumbadger.redreader.ui.frag;

import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.LinearInterpolator;
import android.view.animation.Transformation;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.ui.RRContext;

import java.util.LinkedList;

public final class RRFragmentLayout extends ViewGroup {

	final boolean twoPane = true;

	private boolean paused = true;

	private final LinkedList<RRFragment> activeViews = new LinkedList<RRFragment>();

	private double hOffset = 0, hVel = 0;
	private int desiredHOffset = 0;

	private RRUriHandler uriHandler;

	private final RRContext context;

	public RRFragmentLayout(Activity activity) {
		super(activity);
		this.context = new RRContext(activity, this);
	}

	public static RRFragmentLayout restore(Context context, Bundle bundle) {
		return null; // TODO
	}

	public int getFragmentCount() {
		return activeViews.size();
	}

	public Bundle saveState() {

		final Bundle bundle = new Bundle();

		bundle.putInt("frag-count", activeViews.size());

		int fragId = 0;
		for(final RRFragment fragment : activeViews) {
			bundle.putBundle("frag-" + fragId, fragment.saveState());
			fragId++;
		}

		return bundle;
	}

	public void appendFragment(RRFragment fragment) {

		activeViews.addFirst(fragment);
		addView(fragment.getContentView());

		requestLayout();
	}

	public void removeFragmentsAfter(RRFragment fragment) {

		if(!activeViews.contains(fragment)) throw new RuntimeException();

		while(activeViews.getFirst() != fragment) {
			removeTopFragment();
		}
	}

	public void removeTopFragment() {

		final LayoutParams params = (LayoutParams)activeViews.getFirst().getContentView().getLayoutParams();

		if(params.status != LayoutParams.STATUS_ACTIVE) return;

		params.status = LayoutParams.STATUS_SLIDING_SIDE;
		activeViews.removeFirst();

		requestLayout();
	}

	public void setChildFragment(RRFragment parent, RRFragment fragment) {

		if(!activeViews.contains(parent)) throw new RuntimeException();

		final View parentView = parent.getContentView();

		final LinkedList<View> toRemove = new LinkedList<View>();

		final int childCount = getChildCount();
		boolean remove = false;

		for(int i = 0; i < childCount; i++) {
			if(remove) {
				toRemove.add(getChildAt(i));
			} else if(getChildAt(i) == parentView) {
				remove = true;
			}
		}

		for(final View v : toRemove) removeView(v);

		while(activeViews.getFirst() != parent) {
			activeViews.removeFirst();
		}

		appendFragment(fragment);
	}

	public void handleUri(RRFragment parent, Uri child, RRUriHandler.Mode mode, Bundle arguments) {
		final RRUriHandler.Result result = uriHandler.handle(context, child, mode, arguments);

		if(result == null) throw new RuntimeException("No handler for " + child.toString() + " found.");

		if(result.fragmentToOpen != null) {
			setChildFragment(parent, result.fragmentToOpen);
		}
	}

	public void handleUri(Uri child, RRUriHandler.Mode mode, Bundle arguments) {
		final RRUriHandler.Result result = uriHandler.handle(context, child, mode, arguments);

		if(result == null) throw new RuntimeException("No handler for " + child.toString() + " found.");

		if(result.fragmentToOpen != null) {
			appendFragment(result.fragmentToOpen);
		}
	}

	public void handleUri(Uri child) {
		handleUri(child, RRUriHandler.Mode.ANY, null);
	}

	private void cleanUpInactiveFragments() {

		final int childCount = getChildCount();

		final LinkedList<View> removalList = new LinkedList<View>();

		for(int i = 0; i < childCount; i++) {

			final View v = getChildAt(i);
			final LayoutParams params = (LayoutParams)v.getLayoutParams();

			// TODO check if off screen, if not, relayout
			if(params.status == LayoutParams.STATUS_SLIDING_SIDE) {
				removalList.add(v);
			}
		}

		for(final View v : removalList) {
			removeView(v);
		}
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

		final int width = MeasureSpec.getSize(widthMeasureSpec);
		final int height = MeasureSpec.getSize(heightMeasureSpec);

		final float dpScale = getContext().getResources().getDisplayMetrics().density;

		setMeasuredDimension(width, height);

		final int activeViewCount = activeViews.size();

		if(activeViewCount > 0) {
			if(twoPane && activeViewCount > 1) {

				final RRFragment leftPane = activeViews.get(1);
				final RRFragment rightPane = activeViews.getFirst();

				final LayoutParams leftPaneLayoutParams = (LayoutParams)leftPane.getContentView().getLayoutParams();
				final LayoutParams rightPaneLayoutParams = (LayoutParams)rightPane.getContentView().getLayoutParams();

				final int leftWidth, rightWidth;

				final int leftPreferredWidth = leftPane.preferredWidthLeftcolPx(dpScale);
				final int rightPreferredWidth = rightPane.preferredWidthPx(dpScale);

				if(leftPreferredWidth + rightPreferredWidth > width) {
					leftPaneLayoutParams.calculateWidth(leftPane, true, width / 2, width, dpScale);
					rightPaneLayoutParams.calculateWidth(rightPane, true, width / 2, width, dpScale);

				} else {
					leftPaneLayoutParams.calculateWidth(leftPane, true, leftPreferredWidth, width, dpScale);
					rightPaneLayoutParams.calculateWidth(rightPane, true, width - leftPaneLayoutParams.desiredWidth, width, dpScale);
				}

			} else {
				final RRFragment rightPane = activeViews.getFirst();
				((LayoutParams)(rightPane.getContentView().getLayoutParams())).calculateWidth(rightPane, true, width, width, dpScale);
			}
		}

		final int childCount = getChildCount();

		for(int i = 0; i < childCount; i++) {
			final View v = getChildAt(i);

			if(v.getVisibility() == GONE) continue;

			v.measure(MeasureSpec.makeMeasureSpec(v.getLayoutParams().width, MeasureSpec.EXACTLY),
					MeasureSpec.makeMeasureSpec(height, MeasureSpec.EXACTLY));
		}

	}

	@Override
	public void removeView(View view) {
		super.removeView(view);
		Log.i("RRFragmentLayout", "VIEW REMOVED");
	}

	@Override
	protected void onLayout(boolean changed, int l, int t, int r, int b) {

		final int childCount = getChildCount();
		final int width = r - l, height = b - t;

		int totalWidth = 0;
		int totalDesiredWidth = 0;

		final int[] locationBuf = new int[2];

		final int roundedHOffset = (int) Math.round(hOffset);

		for(int i = 0; i < childCount; i++) {

			final View v = getChildAt(i);
			final RRFragmentLayout.LayoutParams params = (LayoutParams) v.getLayoutParams();

			final int vOffset = Math.round(params.verticalPosition * height);

			v.layout(totalWidth + roundedHOffset, vOffset, totalWidth + params.width + roundedHOffset, vOffset + height);

			v.getLocationOnScreen(locationBuf);
			final int maxX = locationBuf[0] + v.getWidth();

			final int newVisibility = maxX > 0 ? View.VISIBLE : View.INVISIBLE;

			if(newVisibility != v.getVisibility()) {
				v.setVisibility(newVisibility);
				Log.i("RRFragmentLayout", "VIS CHANGED " + (newVisibility == View.VISIBLE ? "VISIBLE" : "INVISIBLE")); // TODO debug
			}

			totalWidth += params.width;

			if(params.status == LayoutParams.STATUS_ACTIVE) {
				totalDesiredWidth += params.desiredWidth;
			}
		}

		if(desiredHOffset != width - totalDesiredWidth) {

			//hOffset = width - totalDesiredWidth;
			desiredHOffset = width - totalDesiredWidth;

			animateHOffset();

			Log.i("RRFragmentLayout", "Offset " + desiredHOffset + " wrong, animating to pos");

		}
	}

	public void setUriHandler(RRUriHandler uriHandler) {
		this.uriHandler = uriHandler;
	}

	// TODO account for dpi
	// TODO account for fps
	private class HOffsetAnimation extends Animation {

		public HOffsetAnimation() {
			setDuration(1000);
		}

		@Override
		protected void applyTransformation(float interpolatedTime, Transformation t) {

			if(Math.abs(hOffset - desiredHOffset) < 1 && Math.abs(hVel) < 0.05) {

				hOffset = desiredHOffset;
				hVel = 0;
				clearAnimation();
				cleanUpInactiveFragments();

				Log.i("RRFragmentLayout", "Animation done.");

			} else {

				final double hAcc = (desiredHOffset - hOffset) * 0.03;
				final double hDamping = 0.86f;

				hVel += hAcc;
				hVel *= hDamping;

				//Log.i("RRFragmentLayout", "hVel = " + hVel);

				hOffset += hVel;

			}

			requestLayout();
		}
	}

	private void animateHOffset() {
		clearAnimation();
		HOffsetAnimation anim = new HOffsetAnimation();
		anim.setInterpolator(new LinearInterpolator());
		startAnimation(anim);
	}

	@Override
	protected void onAnimationEnd() {
		super.onAnimationEnd();

		if(desiredHOffset != Math.round(hOffset) || Math.abs(hVel) > 0.05) {
			animateHOffset();

		} else {

			hOffset = desiredHOffset;
			hVel = 0;
			cleanUpInactiveFragments();

			Log.i("RRFragmentLayout", "Animation done (onAnimEnd).");
		}
	}

	public class LayoutParams extends ViewGroup.LayoutParams {

		public float verticalPosition = 0;
		public int desiredWidth = -1;

		private RRFragment fragment = null;

		public static final int STATUS_ACTIVE = 1, STATUS_SLIDING_SIDE = 2;

		public int status = STATUS_ACTIVE;

		public LayoutParams(Context c, AttributeSet attrs) {
			super(c, attrs);
		}

		public LayoutParams(int width, int height) {
			super(width, height);
		}

		public LayoutParams(ViewGroup.LayoutParams source) {
			super(source);
		}

		public void calculateWidth(RRFragment fragment, boolean animate, int newWidth, int deviceWidth, float dpScale) {

			this.fragment = fragment;

			newWidth = Math.min(deviceWidth, bound(fragment.minWidthPx(dpScale), fragment.maxWidthPx(dpScale), newWidth));

			if(desiredWidth <= 0) animate = false;

			if(newWidth != desiredWidth) {

				desiredWidth = newWidth;

				if(!animate) {
					width = newWidth;

				} else {

					final View v = fragment.getContentView();
					final WidthAnimation anim = new WidthAnimation(v, newWidth);
					anim.setInterpolator(new AccelerateDecelerateInterpolator());
					anim.setDuration(500);
					v.startAnimation(anim);
				}
			}
		}

		private int bound(int lowerBound, int upperBound, int value) {
			return Math.max(lowerBound, Math.min(upperBound, value));
		}
	}

	@Override
	public RRFragmentLayout.LayoutParams generateLayoutParams(AttributeSet attrs) {
		return new LayoutParams(getContext(), attrs);
	}

	@Override
	protected RRFragmentLayout.LayoutParams generateLayoutParams(ViewGroup.LayoutParams p) {
		return new LayoutParams(p);
	}

	@Override
	protected RRFragmentLayout.LayoutParams generateDefaultLayoutParams() {
		return new LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.MATCH_PARENT);
	}

	public void onResume() {

		if(!paused) return;

		for(RRFragment fragment : activeViews) {
			fragment.onResume();
		}

		paused = false;
	}

	public void onPause() {

		if(paused) return;

		for(RRFragment fragment : activeViews) {
			fragment.onPause();
		}

		paused = true;
	}
}
