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

import android.opengl.Matrix;

public class RRGLMatrixStack {

	private int mTopMatrixPos = 0;
	private final float[] mMatrices = new float[16 * 128];
	private final RRGLContext mGLContext;

	public RRGLMatrixStack(RRGLContext glContext) {
		mGLContext = glContext;
		setIdentity();
	}

	public int pushAndTranslate(float offsetX, float offsetY) {
		mTopMatrixPos += 16;
		Matrix.translateM(mMatrices, mTopMatrixPos, mMatrices, mTopMatrixPos - 16, offsetX, offsetY, 0);
		return mTopMatrixPos - 16;
	}

	public int pushAndScale(float factorX, float factorY) {
		mTopMatrixPos += 16;
		Matrix.scaleM(mMatrices, mTopMatrixPos, mMatrices, mTopMatrixPos - 16, factorX, factorY, 0);
		return mTopMatrixPos - 16;
	}

	public int pop() {
		mTopMatrixPos -= 16;
		return mTopMatrixPos;
	}

	public void setIdentity() {
		Matrix.setIdentityM(mMatrices, mTopMatrixPos);
	}

	public void scale(float factorX, float factorY, float factorZ) {
		Matrix.scaleM(mMatrices, mTopMatrixPos, factorX, factorY, factorZ);
	}

	public void flush() {
		mGLContext.activateMatrix(mMatrices, mTopMatrixPos);
	}

	public void assertAtRoot() {

		if(mTopMatrixPos != 0) {
			throw new RuntimeException("assertAtRoot() failed!");
		}

		for(int i = 0; i < 16; i++) {
			switch(i) {
				case 0:
				case 5:
				case 10:
				case 15:
					if(mMatrices[i] != 1) throw new RuntimeException("Root matrix is not identity!");
					break;
				default:
					if(mMatrices[i] != 0) throw new RuntimeException("Root matrix is not identity!");
					break;
			}
		}
	}
}
