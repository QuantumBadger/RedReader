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
import org.saiditnet.redreader.views.glview.displaylist.*;
import org.saiditnet.redreader.views.glview.program.RRGLContext;
import org.saiditnet.redreader.views.glview.program.RRGLMatrixStack;

public class ImageViewScrollbars extends RRGLRenderable {

	private static final float EPSILON = 0.0001f;

	private final RRGLRenderableBlend mRenderable;

	// Vertical scroll bar
	private final RRGLRenderableGroup mVScroll;
	private final RRGLRenderableTranslation mVScrollMarkerTranslation;
	private final RRGLRenderableScale mVScrollMarkerScale;
	private final RRGLRenderableTranslation mVScrollBarTranslation;
	private final RRGLRenderableScale mVScrollBarScale;
	private final RRGLRenderableTranslation mVScrollBorderTranslation;
	private final RRGLRenderableScale mVScrollBorderScale;

	// Horizontal scroll bar
	private final RRGLRenderableGroup mHScroll;
	private final RRGLRenderableTranslation mHScrollMarkerTranslation;
	private final RRGLRenderableScale mHScrollMarkerScale;
	private final RRGLRenderableTranslation mHScrollBarTranslation;
	private final RRGLRenderableScale mHScrollBarScale;
	private final RRGLRenderableTranslation mHScrollBorderTranslation;
	private final RRGLRenderableScale mHScrollBorderScale;

	private final CoordinateHelper mCoordinateHelper;

	private int mResX, mResY;
	private final int mImageResX, mImageResY;

	private final int mDimMarginSides;
	private final int mDimMarginEnds;
	private final int mDimBarWidth;
	private final int mDimBorderWidth;

	private long mShowUntil = -1;
	private float mCurrentAlpha = 1;
	private static final float ALPHA_STEP = 0.05f;
	private boolean mIsVisible = true;

	public ImageViewScrollbars(RRGLContext glContext, CoordinateHelper coordinateHelper, int imageResX, int imageResY) {

		mCoordinateHelper = coordinateHelper;
		mImageResX = imageResX;
		mImageResY = imageResY;

		final RRGLRenderableGroup group = new RRGLRenderableGroup();
		mRenderable = new RRGLRenderableBlend(group);

		mDimMarginSides = glContext.dpToPixels(10);
		mDimMarginEnds = glContext.dpToPixels(20);
		mDimBarWidth = glContext.dpToPixels(6);
		mDimBorderWidth = glContext.dpToPixels(1);

		// Vertical scroll bar
		{
			mVScroll = new RRGLRenderableGroup();
			group.add(mVScroll);

			final RRGLRenderableColouredQuad vScrollMarker = new RRGLRenderableColouredQuad(glContext);
			final RRGLRenderableColouredQuad vScrollBar = new RRGLRenderableColouredQuad(glContext);
			final RRGLRenderableColouredQuad vScrollBorder = new RRGLRenderableColouredQuad(glContext);

			vScrollMarker.setColour(1, 1, 1, 0.8f);
			vScrollBar.setColour(0, 0, 0, 0.5f);
			vScrollBorder.setColour(1, 1, 1, 0.5f);

			mVScrollMarkerScale = new RRGLRenderableScale(vScrollMarker);
			mVScrollBarScale = new RRGLRenderableScale(vScrollBar);
			mVScrollBorderScale = new RRGLRenderableScale(vScrollBorder);

			mVScrollMarkerTranslation = new RRGLRenderableTranslation(mVScrollMarkerScale);
			mVScrollBarTranslation = new RRGLRenderableTranslation(mVScrollBarScale);
			mVScrollBorderTranslation = new RRGLRenderableTranslation(mVScrollBorderScale);

			mVScroll.add(mVScrollBorderTranslation);
			mVScroll.add(mVScrollBarTranslation);
			mVScroll.add(mVScrollMarkerTranslation);
		}

		// Horizontal scroll bar
		{
			mHScroll = new RRGLRenderableGroup();
			group.add(mHScroll);

			final RRGLRenderableColouredQuad hScrollMarker = new RRGLRenderableColouredQuad(glContext);
			final RRGLRenderableColouredQuad hScrollBar = new RRGLRenderableColouredQuad(glContext);
			final RRGLRenderableColouredQuad hScrollBorder = new RRGLRenderableColouredQuad(glContext);

			hScrollMarker.setColour(1, 1, 1, 0.8f);
			hScrollBar.setColour(0, 0, 0, 0.5f);
			hScrollBorder.setColour(1, 1, 1, 0.5f);

			mHScrollMarkerScale = new RRGLRenderableScale(hScrollMarker);
			mHScrollBarScale = new RRGLRenderableScale(hScrollBar);
			mHScrollBorderScale = new RRGLRenderableScale(hScrollBorder);

			mHScrollMarkerTranslation = new RRGLRenderableTranslation(mHScrollMarkerScale);
			mHScrollBarTranslation = new RRGLRenderableTranslation(mHScrollBarScale);
			mHScrollBorderTranslation = new RRGLRenderableTranslation(mHScrollBorderScale);

			mHScroll.add(mHScrollBorderTranslation);
			mHScroll.add(mHScrollBarTranslation);
			mHScroll.add(mHScrollMarkerTranslation);
		}
	}

	public void update() {

		// TODO avoid GC

		final MutableFloatPoint2D tmp1 = new MutableFloatPoint2D();
		final MutableFloatPoint2D tmp2 = new MutableFloatPoint2D();

		mCoordinateHelper.convertScreenToScene(tmp1, tmp2);
		final float xStart = tmp2.x / (float)mImageResX;
		final float yStart = tmp2.y / (float)mImageResY;

		tmp1.set(mResX, mResY);

		mCoordinateHelper.convertScreenToScene(tmp1, tmp2);
		final float xEnd = tmp2.x / (float)mImageResX;
		final float yEnd = tmp2.y / (float)mImageResY;

		// Vertical scroll bar

		if(yStart < EPSILON && yEnd > 1-EPSILON) {
			mVScroll.hide();

		} else {
			mVScroll.show();

			final float vScrollTotalHeight = mResY - 2 * mDimMarginEnds;

			final float vScrollHeight = (yEnd - yStart) * vScrollTotalHeight;
			final float vScrollTop = yStart * vScrollTotalHeight + mDimMarginEnds;
			final float vScrollLeft = mResX - mDimBarWidth - mDimMarginSides;

			mVScrollBorderTranslation.setPosition(vScrollLeft - mDimBorderWidth, mDimMarginEnds - mDimBorderWidth);
			mVScrollBorderScale.setScale(mDimBarWidth + 2 * mDimBorderWidth, vScrollTotalHeight + 2 * mDimBorderWidth);

			mVScrollBarTranslation.setPosition(vScrollLeft, mDimMarginEnds);
			mVScrollBarScale.setScale(mDimBarWidth, vScrollTotalHeight);

			mVScrollMarkerTranslation.setPosition(vScrollLeft, vScrollTop);
			mVScrollMarkerScale.setScale(mDimBarWidth, vScrollHeight);
		}

		// Horizontal scroll bar

		if(xStart < EPSILON && xEnd > 1-EPSILON) {
			mHScroll.hide();

		} else {
			mHScroll.show();

			final float hScrollTotalWidth = mResX - 2 * mDimMarginEnds;

			final float hScrollWidth = (xEnd - xStart) * hScrollTotalWidth;
			final float hScrollLeft = xStart * hScrollTotalWidth + mDimMarginEnds;
			final float hScrollTop = mResY - mDimBarWidth - mDimMarginSides;

			mHScrollBorderTranslation.setPosition(mDimMarginEnds - mDimBorderWidth, hScrollTop - mDimBorderWidth);
			mHScrollBorderScale.setScale(hScrollTotalWidth + 2 * mDimBorderWidth, mDimBarWidth + mDimBorderWidth * 2);

			mHScrollBarTranslation.setPosition(mDimMarginEnds, hScrollTop);
			mHScrollBarScale.setScale(hScrollTotalWidth, mDimBarWidth);

			mHScrollMarkerTranslation.setPosition(hScrollLeft, hScrollTop);
			mHScrollMarkerScale.setScale(hScrollWidth, mDimBarWidth);
		}
	}

	public synchronized void setResolution(int x, int y) {
		mResX = x;
		mResY = y;
	}

	@Override
	public void onAdded() {
		super.onAdded();
		mRenderable.onAdded();
	}

	@Override
	public void onRemoved() {
		mRenderable.onRemoved();
		super.onRemoved();
	}

	@Override
	public synchronized boolean isAnimating() {
		return mIsVisible;
	}

	public synchronized void showBars() {
		mShowUntil = System.currentTimeMillis() + 600;
		mIsVisible = true;
		mCurrentAlpha = 1;
	}

	@Override
	protected synchronized void renderInternal(RRGLMatrixStack stack, long time) {

		if(mIsVisible && time > mShowUntil) {
			mCurrentAlpha -= ALPHA_STEP;

			if(mCurrentAlpha < 0) {
				mIsVisible = false;
				mCurrentAlpha = 0;
			}
		}

		mRenderable.setOverallAlpha(mCurrentAlpha);

		mRenderable.startRender(stack, time);
	}
}
