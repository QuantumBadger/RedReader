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

import android.opengl.GLES20;
import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;

public class RRGLRenderableBlend extends RRGLRenderableRenderHooks {

	public RRGLRenderableBlend(RRGLRenderable entity) {
		super(entity);
	}

	@Override
	protected void preRender(RRGLMatrixStack stack, long time) {
		GLES20.glEnable(GLES20.GL_BLEND);
		GLES20.glBlendFunc(GLES20.GL_SRC_ALPHA, GLES20.GL_ONE_MINUS_SRC_ALPHA);
	}

	@Override
	protected void postRender(RRGLMatrixStack stack, long time) {
		GLES20.glDisable(GLES20.GL_BLEND);
	}
}
