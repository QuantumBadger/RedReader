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

package org.saiditnet.redreader.views.glview.displaylist;

import android.opengl.GLSurfaceView;
import android.opengl.Matrix;
import android.util.Log;

import org.saiditnet.redreader.views.glview.RRGLSurfaceView;
import org.saiditnet.redreader.views.glview.Refreshable;
import org.saiditnet.redreader.views.glview.program.RRGLContext;
import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;
import org.saiditnet.redreader.views.imageview.FingerTracker;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

public class RRGLDisplayListRenderer implements GLSurfaceView.Renderer, Refreshable {

	public interface DisplayListManager extends FingerTracker.FingerListener {
		void onGLSceneCreate(RRGLDisplayList scene, RRGLContext context, Refreshable refreshable);

		void onGLSceneResolutionChange(RRGLDisplayList scene, RRGLContext context, int width, int height);

		boolean onGLSceneUpdate(RRGLDisplayList scene, RRGLContext context);

		void onUIAttach();

		void onUIDetach();
	}

	private final float[] mPixelMatrix = new float[16];

	private RRGLDisplayList mScene;
	private RRGLContext mGLContext;
	private RRGLMatrixStack mMatrixStack;

	private final DisplayListManager mDisplayListManager;
	private final RRGLSurfaceView mSurfaceView;

	public RRGLDisplayListRenderer(DisplayListManager displayListManager, RRGLSurfaceView surfaceView) {
		mDisplayListManager = displayListManager;
		mSurfaceView = surfaceView;
	}

	@Override
	public void onSurfaceCreated(GL10 ignore, EGLConfig config) {

		mGLContext = new RRGLContext(mSurfaceView.getContext());
		mMatrixStack = new RRGLMatrixStack(mGLContext);
		mScene = new RRGLDisplayList();

		mGLContext.setClearColor(0f, 0f, 0f, 1);

		mDisplayListManager.onGLSceneCreate(mScene, mGLContext, this);
	}

	@Override
	public void onSurfaceChanged(GL10 ignore, int width, int height) {

		mGLContext.setViewport(width, height);

		final float hScale = 2f / (float)width;
		final float vScale = -2f / (float)height;

		Matrix.setIdentityM(mPixelMatrix, 0);
		Matrix.translateM(mPixelMatrix, 0, -1, 1, 0);
		Matrix.scaleM(mPixelMatrix, 0, hScale, vScale, 1f);

		mDisplayListManager.onGLSceneResolutionChange(mScene, mGLContext, width, height);
	}

	private int frames = 0;
	private long startTime = -1;

	@Override
	public void onDrawFrame(GL10 ignore) {

		final long time = System.currentTimeMillis();

		if(startTime == -1) {
			startTime = time;
		}

		frames++;

		if(time - startTime >= 1000) {
			startTime = time;
			Log.i("FPS", "Frames: " + frames);
			frames = 0;
		}

		final boolean animating = mDisplayListManager.onGLSceneUpdate(mScene, mGLContext);

		mGLContext.clear();

		mGLContext.activatePixelMatrix(mPixelMatrix, 0);

		mMatrixStack.assertAtRoot();
		mScene.startRender(mMatrixStack, time);
		mMatrixStack.assertAtRoot();

		if(animating || mScene.isAnimating()) {
			mSurfaceView.requestRender();
		}
	}

	@Override
	public void refresh() {
		mSurfaceView.requestRender();
	}
}
