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

import java.util.Locale;

public abstract class RRGLProgram {

	private final int mHandle;

	private Integer mFragmentShaderHandle = null;
	private Integer mVertexShaderHandle = null;

	public RRGLProgram(final String vertexShaderSource, final String fragmentShaderSource) {

		mHandle = GLES20.glCreateProgram();

		if(mHandle == 0) {
			throw new RuntimeException("Error creating program.");
		}

		compileAndAttachShader(GLES20.GL_VERTEX_SHADER, vertexShaderSource);
		compileAndAttachShader(GLES20.GL_FRAGMENT_SHADER, fragmentShaderSource);

		link();
	}

	private void compileAndAttachShader(final int type, final String source) {

		switch(type) {
			case GLES20.GL_FRAGMENT_SHADER: if(mFragmentShaderHandle != null) throw new RuntimeException(); break;
			case GLES20.GL_VERTEX_SHADER:   if(mVertexShaderHandle != null) throw new RuntimeException(); break;
			default: throw new RuntimeException("Unknown shader type.");
		}

		final int shaderHandle = GLES20.glCreateShader(type);
		if(shaderHandle == 0) {
			throw new RuntimeException("Error creating shader.");
		}

		GLES20.glShaderSource(shaderHandle, source);
		GLES20.glCompileShader(shaderHandle);

		final int[] compileStatus = new int[1];
		GLES20.glGetShaderiv(shaderHandle, GLES20.GL_COMPILE_STATUS, compileStatus, 0);

		if(compileStatus[0] == 0) {
			final String log = GLES20.glGetShaderInfoLog(mHandle);
			GLES20.glDeleteShader(shaderHandle);
			throw new RuntimeException(String.format(Locale.US, "Shader compile error: \"%s\".", log));
		}

		GLES20.glAttachShader(mHandle, shaderHandle);

		switch(type) {
			case GLES20.GL_FRAGMENT_SHADER: mFragmentShaderHandle = shaderHandle; break;
			case GLES20.GL_VERTEX_SHADER:   mVertexShaderHandle = shaderHandle; break;
			default: throw new RuntimeException("Unknown shader type.");
		}
	}

	private void link() {

		if(mFragmentShaderHandle == null || mVertexShaderHandle == null) throw new RuntimeException();

		GLES20.glLinkProgram(mHandle);

		final int[] linkStatus = new int[1];
		GLES20.glGetProgramiv(mHandle, GLES20.GL_LINK_STATUS, linkStatus, 0);

		if(linkStatus[0] == 0) {
			final String log = GLES20.glGetProgramInfoLog(mHandle);
			GLES20.glDeleteProgram(mHandle);
			throw new RuntimeException(String.format(Locale.US, "Linker error: \"%s\".", log));
		}

		GLES20.glDetachShader(mHandle, mFragmentShaderHandle);
		GLES20.glDetachShader(mHandle, mVertexShaderHandle);

		GLES20.glDeleteShader(mFragmentShaderHandle);
		GLES20.glDeleteShader(mVertexShaderHandle);

		mFragmentShaderHandle = null;
		mVertexShaderHandle = null;
	}

	public int getAttributeHandle(final String name) {
		return GLES20.glGetAttribLocation(mHandle, name);
	}

	public int getUniformHandle(final String name) {
		return GLES20.glGetUniformLocation(mHandle, name);
	}

	public int getHandle() {
		return mHandle;
	}

	public abstract void onActivated();
	public abstract void onDeactivated();
}
