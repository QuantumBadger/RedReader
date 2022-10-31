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

import android.opengl.GLES20;

import java.nio.FloatBuffer;

public abstract class RRGLProgramVertices extends RRGLProgram {

	private int mVertexBufferHandle;
	private int mMatrixUniformHandle;
	private int mPixelMatrixUniformHandle;

	public RRGLProgramVertices(String vertexShaderSource, String fragmentShaderSource) {
		super(vertexShaderSource, fragmentShaderSource);
	}

	public final void activateVertexBuffer(final FloatBuffer vertexBuffer) {
		GLES20.glVertexAttribPointer(mVertexBufferHandle, 3, GLES20.GL_FLOAT, false, 0, vertexBuffer);
	}

	public final void drawTriangleStrip(int vertices) {
		GLES20.glDrawArrays(GLES20.GL_TRIANGLE_STRIP, 0, vertices);
	}

	protected final void setVertexBufferHandle(int handle) {
		mVertexBufferHandle = handle;
	}

	protected final void setMatrixUniformHandle(int handle) {
		mMatrixUniformHandle = handle;
	}

	protected final void setPixelMatrixHandle(int handle) {
		mPixelMatrixUniformHandle = handle;
	}

	public final void activateMatrix(float[] buf, int offset) {
		GLES20.glUniformMatrix4fv(mMatrixUniformHandle, 1, false, buf, offset);
	}

	public final void activatePixelMatrix(float[] buf, int offset) {
		GLES20.glUniformMatrix4fv(mPixelMatrixUniformHandle, 1, false, buf, offset);
	}

	@Override
	public void onActivated() {
		GLES20.glEnableVertexAttribArray(mVertexBufferHandle);
	}

	@Override
	public void onDeactivated() {
		GLES20.glDisableVertexAttribArray(mVertexBufferHandle);
	}
}
