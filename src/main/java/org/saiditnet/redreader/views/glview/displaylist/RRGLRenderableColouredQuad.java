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

import org.saiditnet.redreader.views.glview.program.RRGLContext;
import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;

public class RRGLRenderableColouredQuad extends RRGLRenderable {

	private final RRGLContext mGLContext;

	private float mRed, mGreen, mBlue, mAlpha;
	private float mOverallAlpha = 1;

	private static final FloatBuffer mVertexBuffer;

	private static final float[] vertexData = {
			0, 0, 0,
			0, 1, 0,
			1, 0, 0,
			1, 1, 0
	};

	static {
		mVertexBuffer = ByteBuffer.allocateDirect(vertexData.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
		mVertexBuffer.put(vertexData).position(0);
	}

	public RRGLRenderableColouredQuad(RRGLContext glContext) {
		mGLContext = glContext;
	}

	public void setColour(final float r, final float g, final float b, final float a) {
		mRed = r;
		mGreen = g;
		mBlue = b;
		mAlpha = a;
	}

	@Override
	public void setOverallAlpha(float alpha) {
		mOverallAlpha = alpha;
	}

	@Override
	protected void renderInternal(RRGLMatrixStack matrixStack, final long time) {

		mGLContext.activateProgramColour();

		matrixStack.flush();

		mGLContext.activateVertexBuffer(mVertexBuffer);
		mGLContext.activateColour(mRed, mGreen, mBlue, mAlpha * mOverallAlpha);

		mGLContext.drawTriangleStrip(4);
	}
}
