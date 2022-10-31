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

public class RRGLProgramTexture extends RRGLProgramVertices {

	private final int mUVDataHandle;
	private final int mTextureUniformHandle;

	public RRGLProgramTexture() {

		super(vertexShaderSource, fragmentShaderSource);

		setVertexBufferHandle(getAttributeHandle("a_Position"));
		setMatrixUniformHandle(getUniformHandle("u_Matrix"));
		setPixelMatrixHandle(getUniformHandle("u_PixelMatrix"));

		mUVDataHandle = getAttributeHandle("a_TexCoordinate");
		mTextureUniformHandle = getUniformHandle("u_Texture");
	}

	public void activateTextureByHandle(final int textureHandle) {
		GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, textureHandle);
		GLES20.glUniform1i(mTextureUniformHandle, 0);
	}

	public void activateUVBuffer(FloatBuffer uvBuffer) {
		GLES20.glVertexAttribPointer(mUVDataHandle, 2, GLES20.GL_FLOAT, false, 0, uvBuffer);
	}

	@Override
	public void onActivated() {
		super.onActivated();
		GLES20.glEnableVertexAttribArray(mUVDataHandle);
		GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
	}

	@Override
	public void onDeactivated() {
		super.onDeactivated();
		GLES20.glDisableVertexAttribArray(mUVDataHandle);
		GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, 0);
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
					+ "uniform sampler2D u_Texture; \n"
					+ "varying vec2 v_TexCoordinate; \n"
					+ "void main() { \n"
					+ "  gl_FragColor = texture2D(u_Texture, v_TexCoordinate); \n"
					+ "} \n";
}
