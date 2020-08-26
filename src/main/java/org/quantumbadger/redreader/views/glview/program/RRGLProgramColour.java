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

package org.quantumbadger.redreader.views.glview.program;

import android.opengl.GLES20;

public class RRGLProgramColour extends RRGLProgramVertices {

	private final int mColorHandle;

	public RRGLProgramColour() {

		super(vertexShaderSource, fragmentShaderSource);

		setVertexBufferHandle(getAttributeHandle("a_Position"));
		setMatrixUniformHandle(getUniformHandle("u_Matrix"));
		setPixelMatrixHandle(getUniformHandle("u_PixelMatrix"));

		mColorHandle = getUniformHandle("u_Color");
	}

	public void activateColour(
			final float r,
			final float g,
			final float b,
			final float a) {
		GLES20.glUniform4f(mColorHandle, r, g, b, a);
	}

	@Override
	public void onActivated() {
		super.onActivated();
		GLES20.glEnableVertexAttribArray(mColorHandle);
	}

	@Override
	public void onDeactivated() {
		super.onDeactivated();
		GLES20.glDisableVertexAttribArray(mColorHandle);
	}

	private static final String vertexShaderSource =
			"uniform mat4 u_Matrix; \n"
					+ "uniform mat4 u_PixelMatrix; \n"
					+ "attribute vec4 a_Position; \n"
					+ "attribute vec2 a_TexCoordinate; \n"
					+ "varying vec2 v_TexCoordinate; \n"
					+ "void main() {\n"
					+ "  v_TexCoordinate = a_TexCoordinate; \n"
					+ "  gl_Position = u_PixelMatrix * (u_Matrix * a_Position);\n"
					+ "} \n";

	private static final String fragmentShaderSource =
			"precision mediump float; \n"
					+ "uniform vec4 u_Color; \n"
					+ "void main() { \n"
					+ "  gl_FragColor = u_Color; \n"
					+ "} \n";
}
