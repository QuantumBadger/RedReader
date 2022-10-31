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

package org.saiditnet.redreader.views.glview;

import android.content.Context;
import android.opengl.GLSurfaceView;
import android.view.MotionEvent;
import org.saiditnet.redreader.views.glview.displaylist.RRGLDisplayListRenderer;
import org.saiditnet.redreader.views.imageview.FingerTracker;

public class RRGLSurfaceView extends GLSurfaceView {

	private final FingerTracker mFingerTracker;
	private final RRGLDisplayListRenderer.DisplayListManager mDisplayListManager;

	public RRGLSurfaceView(Context context, RRGLDisplayListRenderer.DisplayListManager displayListManager) {
		super(context);

		setEGLContextClientVersion(2);
		setEGLConfigChooser(8, 8, 8, 8, 0, 0);
		setRenderer(new RRGLDisplayListRenderer(displayListManager, this));
		setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);

		mFingerTracker = new FingerTracker(displayListManager);
		mDisplayListManager = displayListManager;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		mFingerTracker.onTouchEvent(event);
		requestRender();
		return true;
	}

	@Override
	protected void onAttachedToWindow() {
		super.onAttachedToWindow();
		mDisplayListManager.onUIAttach();
	}

	@Override
	protected void onDetachedFromWindow() {
		super.onDetachedFromWindow();
		mDisplayListManager.onUIDetach();
	}
}
