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
import org.saiditnet.redreader.views.glview.program.RRGLTexture;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;

public class RRGLRenderableTexturedQuad extends RRGLRenderable {

	private RRGLTexture mTexture;
	private final RRGLContext mGLContext;

	private static final FloatBuffer mVertexBuffer;

	private static final float[] vertexData = {
			0, 0, 0,
			0, 1, 0,
			1, 0, 0,
			1, 1, 0
	};

	private static final FloatBuffer mUVBuffer;

	private static final float[] uvData = {
			0f, 0f,
			0f, 1f,
			1f, 0f,
			1f, 1f
	};

	static {
		mVertexBuffer = ByteBuffer.allocateDirect(vertexData.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
		mVertexBuffer.put(vertexData).position(0);

		mUVBuffer = ByteBuffer.allocateDirect(uvData.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
		mUVBuffer.put(uvData).position(0);
	}

	public RRGLRenderableTexturedQuad(RRGLContext glContext, RRGLTexture texture) {
		mGLContext = glContext;
		mTexture = texture;
	}

	public void setTexture(RRGLTexture newTexture) {

		if(isAdded()) {
			mTexture.releaseReference();
		}

		mTexture = newTexture;

		if(isAdded()) {
			mTexture.addReference();
		}
	}

	@Override
	public void onAdded() {
		super.onAdded();
		mTexture.addReference();
	}

	@Override
	public void onRemoved() {
		mTexture.releaseReference();
		super.onRemoved();
	}

	@Override
	protected void renderInternal(RRGLMatrixStack matrixStack, final long time) {

		mGLContext.activateProgramTexture();

		mTexture.activate();
		matrixStack.flush();

		mGLContext.activateVertexBuffer(mVertexBuffer);
		mGLContext.activateUVBuffer(mUVBuffer);

		mGLContext.drawTriangleStrip(4);
	}
}
