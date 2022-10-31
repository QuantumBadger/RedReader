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

package org.saiditnet.redreader.views.glview.program;

import android.content.Context;
import android.opengl.GLES20;
import org.saiditnet.redreader.common.General;

import java.nio.FloatBuffer;

public final class RRGLContext {

	private final RRGLProgramTexture mProgramTexture;
	private final RRGLProgramColour mProgramColour;

	private float[] mPixelMatrix;
	private int mPixelMatrixOffset;

	private RRGLProgramVertices mProgramCurrent;

	private final Context mContext;

	public RRGLContext(Context context) {
		mProgramTexture = new RRGLProgramTexture();
		mProgramColour = new RRGLProgramColour();
		mContext = context;
	}

	public int dpToPixels(float dp) {
		return General.dpToPixels(mContext, dp);
	}

	public float getScreenDensity() {
		return mContext.getResources().getDisplayMetrics().density;
	}

	public void activateProgramColour() {
		if(mProgramCurrent != mProgramColour) {
			activateProgram(mProgramColour);
		}
	}

	public void activateProgramTexture() {
		if(mProgramCurrent != mProgramTexture) {
			activateProgram(mProgramTexture);
		}
	}

	private void activateProgram(final RRGLProgramVertices program) {

		if(mProgramCurrent != null) {
			mProgramCurrent.onDeactivated();
		}

		GLES20.glUseProgram(program.getHandle());
		mProgramCurrent = program;

		program.onActivated();

		if(mPixelMatrix != null) {
			program.activatePixelMatrix(mPixelMatrix, mPixelMatrixOffset);
		}
	}

	void activateTextureByHandle(final int textureHandle) {
		mProgramTexture.activateTextureByHandle(textureHandle);
	}

	public void activateVertexBuffer(FloatBuffer vertexBuffer) {
		mProgramCurrent.activateVertexBuffer(vertexBuffer);
	}

	public void activateColour(final float r, final float g, final float b, final float a) {
		mProgramColour.activateColour(r, g, b, a);
	}

	public void activateUVBuffer(FloatBuffer uvBuffer) {
		mProgramTexture.activateUVBuffer(uvBuffer);
	}

	public void drawTriangleStrip(int vertices) {
		mProgramCurrent.drawTriangleStrip(vertices);
	}

	public void activateMatrix(float[] buf, int offset) {
		mProgramCurrent.activateMatrix(buf, offset);
	}

	public void activatePixelMatrix(float[] buf, int offset) {

		mPixelMatrix = buf;
		mPixelMatrixOffset = offset;

		if(mProgramCurrent != null) {
			mProgramCurrent.activatePixelMatrix(buf, offset);
		}
	}

	public void setClearColor(float r, float g, float b, float a) {
		GLES20.glClearColor(r, g, b, a);
	}

	public void clear() {
		GLES20.glClear(GLES20.GL_DEPTH_BUFFER_BIT | GLES20.GL_COLOR_BUFFER_BIT);
	}

	public void setViewport(int width, int height) {
		GLES20.glViewport(0, 0, width, height);
	}
}
