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

public abstract class RRGLRenderableRenderHooks extends RRGLRenderable {

	private final RRGLRenderable mEntity;

	public RRGLRenderableRenderHooks(final RRGLRenderable entity) {
		this.mEntity = entity;
	}

	@Override
	protected void renderInternal(final RRGLMatrixStack stack, final long time) {
		preRender(stack, time);
		mEntity.startRender(stack, time);
		postRender(stack, time);
	}

	@Override
	public void onAdded() {
		mEntity.onAdded();
		super.onAdded();
	}

	@Override
	public void onRemoved() {
		super.onRemoved();
		mEntity.onRemoved();
	}

	@Override
	public boolean isAnimating() {
		return mEntity.isAnimating();
	}

	protected abstract void preRender(RRGLMatrixStack stack, final long time);
	protected abstract void postRender(RRGLMatrixStack stack, final long time);

	@Override
	public void setOverallAlpha(float alpha) {
		mEntity.setOverallAlpha(alpha);
	}
}
