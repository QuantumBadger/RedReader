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


import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;

public class RRGLRenderableScale extends RRGLRenderableRenderHooks {

	private float mScaleX = 1, mScaleY = 1;

	public RRGLRenderableScale(final RRGLRenderable entity) {
		super(entity);
	}

	public void setScale(float x, float y) {
		mScaleX = x;
		mScaleY = y;
	}

	@Override
	protected void preRender(final RRGLMatrixStack stack, final long time) {
		stack.pushAndScale(mScaleX, mScaleY);
	}

	@Override
	protected void postRender(final RRGLMatrixStack stack, final long time) {
		stack.pop();
	}
}
