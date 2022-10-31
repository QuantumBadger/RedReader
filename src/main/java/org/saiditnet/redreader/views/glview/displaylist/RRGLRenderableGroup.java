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

import java.util.ArrayList;

public class RRGLRenderableGroup extends RRGLRenderable {

	private final ArrayList<RRGLRenderable> mChildren = new ArrayList<>(16);

	public final void add(RRGLRenderable child) {
		mChildren.add(child);
		if(isAdded()) child.onAdded();
	}

	public final void remove(RRGLRenderable child) {
		if(isAdded()) child.onRemoved();
		mChildren.remove(child);
	}

	@Override
	public void onAdded() {

		if(!isAdded()) {
			for(RRGLRenderable entity : mChildren) {
				entity.onAdded();
			}
		}

		super.onAdded();
	}

	@Override
	protected void renderInternal(final RRGLMatrixStack matrixStack, final long time) {
		for(int i = 0; i < mChildren.size(); i++) {
			RRGLRenderable entity = mChildren.get(i);
			entity.startRender(matrixStack, time);
		}
	}

	@Override
	public void onRemoved() {

		super.onRemoved();

		if(!isAdded()) {
			for(RRGLRenderable entity : mChildren) entity.onRemoved();
		}
	}

	@Override
	public boolean isAnimating() {
		for(int i = 0; i < mChildren.size(); i++) {
			RRGLRenderable entity = mChildren.get(i);
			if(entity.isAnimating()) return true;
		}
		return false;
	}

	@Override
	public void setOverallAlpha(float alpha) {
		for(int i = 0; i < mChildren.size(); i++) {
			RRGLRenderable entity = mChildren.get(i);
			entity.setOverallAlpha(alpha);
		}
	}
}
