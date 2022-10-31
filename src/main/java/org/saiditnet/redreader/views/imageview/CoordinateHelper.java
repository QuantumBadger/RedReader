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

package org.saiditnet.redreader.views.imageview;

import org.saiditnet.redreader.common.MutableFloatPoint2D;

public class CoordinateHelper {

	private float mScale = 1.0f;
	private final MutableFloatPoint2D mPositionOffset = new MutableFloatPoint2D();

	public void setScale(final float scale) {
		mScale = scale;
	}

	public float getScale() {
		return mScale;
	}

	public MutableFloatPoint2D getPositionOffset() {
		return mPositionOffset;
	}

	public void getPositionOffset(MutableFloatPoint2D result) {
		result.set(mPositionOffset);
	}

	public void convertScreenToScene(final MutableFloatPoint2D screenPos, final MutableFloatPoint2D output) {
		output.x = (screenPos.x - mPositionOffset.x) / mScale;
		output.y = (screenPos.y - mPositionOffset.y) / mScale;
	}

	public void convertSceneToScreen(final MutableFloatPoint2D scenePos, final MutableFloatPoint2D output) {
		output.x = scenePos.x * mScale + mPositionOffset.x;
		output.y = scenePos.y * mScale + mPositionOffset.y;
	}

	public void scaleAboutScreenPoint(final MutableFloatPoint2D screenPos, final float scaleFactor) {
		setScaleAboutScreenPoint(screenPos, mScale * scaleFactor);
	}

	public void setScaleAboutScreenPoint(final MutableFloatPoint2D screenPos, final float scale) {

		final MutableFloatPoint2D oldScenePos = new MutableFloatPoint2D();
		convertScreenToScene(screenPos, oldScenePos);

		mScale = scale;

		final MutableFloatPoint2D newScreenPos = new MutableFloatPoint2D();
		convertSceneToScreen(oldScenePos, newScreenPos);

		translateScreen(newScreenPos, screenPos);
	}

	public void translateScreen(final MutableFloatPoint2D oldScreenPos, final MutableFloatPoint2D newScreenPos) {
		mPositionOffset.add(newScreenPos);
		mPositionOffset.sub(oldScreenPos);
	}
}
