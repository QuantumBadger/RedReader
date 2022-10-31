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

public abstract class RRGLRenderable {

	private boolean mVisible = true;
	private int mAttachmentCount = 0;

	public final void hide() {
		mVisible = false;
	}

	public final void show() {
		mVisible = true;
	}

	public final boolean isVisible() {
		return mVisible;
	}

	public final void startRender(final RRGLMatrixStack stack, final long time) {
		if(mVisible) renderInternal(stack, time);
	}

	public void onAdded() {
		mAttachmentCount++;
	}

	public boolean isAdded() {
		return mAttachmentCount > 0;
	}

	protected abstract void renderInternal(RRGLMatrixStack stack, final long time);

	public void onRemoved() {
		mAttachmentCount--;
	}

	public boolean isAnimating() {
		return false;
	}

	public void setOverallAlpha(float alpha) {
		throw new UnsupportedOperationException();
	}
}
