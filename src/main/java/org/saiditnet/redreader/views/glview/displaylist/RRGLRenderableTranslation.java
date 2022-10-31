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


import org.saiditnet.redreader.common.MutableFloatPoint2D;
import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;

public class RRGLRenderableTranslation extends RRGLRenderableRenderHooks {

	private float mPositionX, mPositionY;

	public RRGLRenderableTranslation(final RRGLRenderable entity) {
		super(entity);

	}

	public void setPosition(float x, float y) {
		mPositionX = x;
		mPositionY = y;
	}

	@Override
	protected void preRender(final RRGLMatrixStack stack, final long time) {
		stack.pushAndTranslate(mPositionX, mPositionY);
	}

	@Override
	protected void postRender(final RRGLMatrixStack stack, final long time) {
		stack.pop();
	}

	public void setPosition(MutableFloatPoint2D mPositionOffset) {
		mPositionX = mPositionOffset.x;
		mPositionY = mPositionOffset.y;
	}
}
