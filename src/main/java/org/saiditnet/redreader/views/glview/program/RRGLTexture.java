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

import android.graphics.Bitmap;
import android.opengl.GLES20;
import android.opengl.GLUtils;

public class RRGLTexture {

	private final int mTextureHandle;
	private final RRGLContext mGLContext;
	private int mRefCount = 1;

	public RRGLTexture(RRGLContext glContext, Bitmap bitmap) {
		mTextureHandle = loadTexture(bitmap);
		mGLContext = glContext;
	}

	public void addReference() {
		mRefCount++;
	}

	public void releaseReference() {
		mRefCount--;
		if(mRefCount == 0) {
			deleteTexture(mTextureHandle);
		}
	}

	public void activate() {
		mGLContext.activateTextureByHandle(mTextureHandle);
	}

	private static int loadTexture(final Bitmap bitmap) {

		final int[] textureHandle = new int[1];
		GLES20.glGenTextures(1, textureHandle, 0);

		if(textureHandle[0] == 0) {
			throw new RuntimeException("OpenGL error: glGenTextures failed.");
		}

		GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, textureHandle[0]);
		GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MIN_FILTER, GLES20.GL_LINEAR); // TODO bicubic?
		GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MAG_FILTER, GLES20.GL_LINEAR);
		GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_S, GLES20.GL_CLAMP_TO_EDGE);
		GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_T, GLES20.GL_CLAMP_TO_EDGE);

		GLUtils.texImage2D(GLES20.GL_TEXTURE_2D, 0, bitmap, 0);

		return textureHandle[0];
	}

	private static void deleteTexture(final int handle) {
		final int[] handles = {handle};
		GLES20.glDeleteTextures(1, handles, 0);
	}
}
