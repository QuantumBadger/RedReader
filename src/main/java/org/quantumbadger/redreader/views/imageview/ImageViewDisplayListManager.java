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

package org.quantumbadger.redreader.views.imageview;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.util.Log;
import org.quantumbadger.redreader.common.MutableFloatPoint2D;
import org.quantumbadger.redreader.common.UIThreadRepeatingTimer;
import org.quantumbadger.redreader.common.collections.Stack;
import org.quantumbadger.redreader.views.glview.Refreshable;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLDisplayList;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLDisplayListRenderer;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLRenderableGroup;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLRenderableScale;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLRenderableTexturedQuad;
import org.quantumbadger.redreader.views.glview.displaylist.RRGLRenderableTranslation;
import org.quantumbadger.redreader.views.glview.program.RRGLContext;
import org.quantumbadger.redreader.views.glview.program.RRGLTexture;

import java.util.Arrays;

public class ImageViewDisplayListManager implements
		RRGLDisplayListRenderer.DisplayListManager,
		UIThreadRepeatingTimer.Listener,
		ImageViewTileLoader.Listener {

	public interface Listener extends BasicGestureHandler.Listener {
		void onImageViewDLMOutOfMemory();

		void onImageViewDLMException(Throwable t);
	}

	private static final long TAP_MAX_DURATION_MS = 225;
	private static final long DOUBLE_TAP_MAX_GAP_DURATION_MS = 275;

	private static final Bitmap NOT_LOADED_BITMAP;

	static {
		NOT_LOADED_BITMAP = Bitmap.createBitmap(32, 32, Bitmap.Config.ARGB_8888);

		final Canvas notLoadedCanvas = new Canvas(NOT_LOADED_BITMAP);
		notLoadedCanvas.drawRGB(70, 70, 70);
	}

	private final Listener mListener;

	private RRGLRenderableTranslation mOverallTranslation;
	private RRGLRenderableScale mOverallScale;

	private final ImageTileSource mImageTileSource;
	private final int mHTileCount;
	private final int mVTileCount;
	private final int mTileSize;

	private RRGLTexture mNotLoadedTexture;

	private int mResolutionX;
	private int mResolutionY;

	private final MultiScaleTileManager[][] mTileLoaders;
	private final RRGLRenderableTexturedQuad[][] mTiles;
	private boolean[][] mTileVisibility;
	private boolean[][] mTileLoaded;
	private int mLastSampleSize = 1;

	private Refreshable mRefreshable;

	private enum TouchState {
		ONE_FINGER_DOWN,
		ONE_FINGER_DRAG,
		TWO_FINGER_PINCH,
		DOUBLE_TAP_WAIT_NO_FINGERS_DOWN,
		DOUBLE_TAP_ONE_FINGER_DOWN,
		DOUBLE_TAP_ONE_FINGER_DRAG
	}

	private final CoordinateHelper mCoordinateHelper = new CoordinateHelper();
	private BoundsHelper mBoundsHelper = null;

	private TouchState mCurrentTouchState = null;

	private FingerTracker.Finger mDragFinger;
	private FingerTracker.Finger mPinchFinger1;
	private FingerTracker.Finger mPinchFinger2;
	private final Stack<FingerTracker.Finger> mSpareFingers = new Stack<>(8);

	private final UIThreadRepeatingTimer mDoubleTapGapTimer =
			new UIThreadRepeatingTimer(50, this);

	private long mFirstTapReleaseTime = -1;

	private ImageViewScaleAnimation mScaleAnimation = null;

	private ImageViewScrollbars mScrollbars;

	private float mScreenDensity = 1;

	public ImageViewDisplayListManager(
			final ImageTileSource imageTileSource,
			final Listener listener) {

		mImageTileSource = imageTileSource;
		mListener = listener;
		mHTileCount = mImageTileSource.getHTileCount();
		mVTileCount = mImageTileSource.getVTileCount();
		mTileSize = mImageTileSource.getTileSize();
		mTiles = new RRGLRenderableTexturedQuad[mHTileCount][mVTileCount];

		mTileLoaders = new MultiScaleTileManager[mHTileCount][mVTileCount];
		final ImageViewTileLoaderThread thread = new ImageViewTileLoaderThread();

		for(int x = 0; x < mHTileCount; x++) {
			for(int y = 0; y < mVTileCount; y++) {
				mTileLoaders[x][y] =
						new MultiScaleTileManager(imageTileSource, thread, x, y, this);
			}
		}
	}

	@Override
	public synchronized void onGLSceneCreate(
			final RRGLDisplayList scene,
			final RRGLContext glContext,
			final Refreshable refreshable) {

		mTileVisibility = new boolean[mHTileCount][mVTileCount];
		mTileLoaded = new boolean[mHTileCount][mVTileCount];
		mRefreshable = refreshable;
		mScreenDensity = glContext.getScreenDensity();

		mNotLoadedTexture = new RRGLTexture(glContext, NOT_LOADED_BITMAP, false);

		final RRGLRenderableGroup group = new RRGLRenderableGroup();

		mOverallScale = new RRGLRenderableScale(group);
		mOverallTranslation = new RRGLRenderableTranslation(mOverallScale);
		scene.add(mOverallTranslation);

		for(int x = 0; x < mHTileCount; x++) {
			for(int y = 0; y < mVTileCount; y++) {

				final RRGLRenderableTexturedQuad quad =
						new RRGLRenderableTexturedQuad(glContext, mNotLoadedTexture);
				mTiles[x][y] = quad;

				final RRGLRenderableScale scale = new RRGLRenderableScale(quad);

				int tileWidth = mTileSize;
				int tileHeight = mTileSize;

				final int imageWidth = mImageTileSource.getWidth();
				final int imageHeight = mImageTileSource.getHeight();

				if(x == mHTileCount - 1 && imageWidth % mTileSize != 0) {
					tileWidth = imageWidth % mTileSize;
				}

				if(y == mVTileCount - 1 && imageHeight % mTileSize != 0) {
					tileHeight = imageHeight % mTileSize;
				}

				scale.setScale(tileWidth, tileHeight);

				final RRGLRenderableTranslation translation =
						new RRGLRenderableTranslation(scale);
				translation.setPosition(x * mTileSize, y * mTileSize);

				group.add(translation);
			}
		}

		mScrollbars = new ImageViewScrollbars(
				glContext,
				mCoordinateHelper,
				mImageTileSource.getWidth(),
				mImageTileSource.getHeight()
		);

		scene.add(mScrollbars);
	}

	@Override
	public synchronized void onGLSceneResolutionChange(
			final RRGLDisplayList scene,
			final RRGLContext context,
			final int width,
			final int height) {

		mResolutionX = width;
		mResolutionY = height;

		final boolean setInitialScale = (mBoundsHelper == null);

		mBoundsHelper = new BoundsHelper(
				width, height,
				mImageTileSource.getWidth(), mImageTileSource.getHeight(),
				mCoordinateHelper);

		if(setInitialScale) {
			mBoundsHelper.applyMinScale();
		}

		mScrollbars.setResolution(width, height);
		mScrollbars.showBars();
	}

	@Override
	public synchronized boolean onGLSceneUpdate(
			final RRGLDisplayList scene,
			final RRGLContext context) {

		if(mScaleAnimation != null) {
			if(!mScaleAnimation.onStep()) {
				mScaleAnimation = null;
			}
		}

		if(mBoundsHelper != null) {
			mBoundsHelper.applyBounds();
		}

		final MutableFloatPoint2D positionOffset = mCoordinateHelper.getPositionOffset();
		final float scale = mCoordinateHelper.getScale();

		mOverallTranslation.setPosition(positionOffset);
		mOverallScale.setScale(scale, scale);

		mScrollbars.update();

		final int sampleSize = pickSampleSize();

		if(mLastSampleSize != sampleSize) {

			for(final boolean[] arr : mTileLoaded) {
				Arrays.fill(arr, false);
			}

			mLastSampleSize = sampleSize;
		}

		final float firstVisiblePixelX = -positionOffset.x / scale;
		final float firstVisiblePixelY = -positionOffset.y / scale;

		final int firstVisibleTileX = (int)Math.floor(firstVisiblePixelX / mTileSize);
		final int firstVisibleTileY = (int)Math.floor(firstVisiblePixelY / mTileSize);

		final float lastVisiblePixelX = firstVisiblePixelX + (float)mResolutionX / scale;
		final float lastVisiblePixelY = firstVisiblePixelY + (float)mResolutionY / scale;

		final int lastVisibleTileX = (int)Math.ceil(lastVisiblePixelX / mTileSize);
		final int lastVisibleTileY = (int)Math.ceil(lastVisiblePixelY / mTileSize);

		final int desiredScaleIndex =
				MultiScaleTileManager.sampleSizeToScaleIndex(sampleSize);

		for(int x = 0; x < mHTileCount; x++) {
			for(int y = 0; y < mVTileCount; y++) {

				final boolean isTileVisible =
						x >= firstVisibleTileX
								&& y >= firstVisibleTileY
								&& x <= lastVisibleTileX
								&& y <= lastVisibleTileY;

				final boolean isTileWanted =
						x >= firstVisibleTileX - 1
								&& y >= firstVisibleTileY - 1
								&& x <= lastVisibleTileX + 1
								&& y <= lastVisibleTileY + 1;

				if(isTileWanted && !mTileLoaded[x][y]) {
					mTileLoaders[x][y].markAsWanted(desiredScaleIndex);

				} else {
					mTileLoaders[x][y].markAsUnwanted();
				}

				if(isTileVisible != mTileVisibility[x][y] || !mTileLoaded[x][y]) {

					if(isTileVisible && !mTileLoaded[x][y]) {

						final Bitmap tile = mTileLoaders[x][y].getAtDesiredScale();

						if(tile != null) {

							try {
								final RRGLTexture texture =
										new RRGLTexture(context, tile, true);
								mTiles[x][y].setTexture(texture);
								texture.releaseReference();
								mTileLoaded[x][y] = true;
								tile.recycle();

							} catch(final Exception e) {
								Log.e(
										"ImageViewDisplayListMan",
										"Exception when creating texture",
										e);
							}
						}

					} else if(!isTileWanted) {
						mTiles[x][y].setTexture(mNotLoadedTexture);
					}

					mTileVisibility[x][y] = isTileVisible;
				}
			}
		}

		if(mScaleAnimation != null) {
			mScrollbars.showBars();
		}

		return mScaleAnimation != null;
	}

	@Override
	public void onUIAttach() {
	}

	@Override
	public void onUIDetach() {
		mImageTileSource.dispose();
	}

	@Override
	public synchronized void onFingerDown(final FingerTracker.Finger finger) {

		if(mScrollbars == null) {
			return;
		}

		mScaleAnimation = null;
		mScrollbars.showBars();

		if(mCurrentTouchState == null) {
			mCurrentTouchState = TouchState.ONE_FINGER_DOWN;
			mDragFinger = finger;

		} else {
			switch(mCurrentTouchState) {

				case DOUBLE_TAP_WAIT_NO_FINGERS_DOWN:
					mCurrentTouchState = TouchState.DOUBLE_TAP_ONE_FINGER_DOWN;
					mDragFinger = finger;
					mDoubleTapGapTimer.stopTimer();
					break;

				case ONE_FINGER_DRAG:
					mListener.onHorizontalSwipeEnd();

					// Deliberate fallthrough

				case ONE_FINGER_DOWN:
				case DOUBLE_TAP_ONE_FINGER_DOWN:
				case DOUBLE_TAP_ONE_FINGER_DRAG:

					mCurrentTouchState = TouchState.TWO_FINGER_PINCH;
					mPinchFinger1 = mDragFinger;
					mPinchFinger2 = finger;
					mDragFinger = null;
					break;

				default:
					mSpareFingers.push(finger);
					break;
			}
		}
	}

	private final MutableFloatPoint2D mTmpPoint1_onFingersMoved =
			new MutableFloatPoint2D();
	private final MutableFloatPoint2D mTmpPoint2_onFingersMoved =
			new MutableFloatPoint2D();

	@Override
	public synchronized void onFingersMoved() {

		if(mCurrentTouchState == null) {
			return;
		}

		if(mScrollbars == null) {
			return;
		}

		mScaleAnimation = null;
		mScrollbars.showBars();

		switch(mCurrentTouchState) {

			case DOUBLE_TAP_ONE_FINGER_DOWN: {

				if(mDragFinger.mTotalPosDifference.distanceSquared()
						>= 400f * mScreenDensity * mScreenDensity) {
					mCurrentTouchState = TouchState.DOUBLE_TAP_ONE_FINGER_DRAG;
				}

				break;
			}

			case DOUBLE_TAP_ONE_FINGER_DRAG: {

				final MutableFloatPoint2D screenCentre = mTmpPoint1_onFingersMoved;
				screenCentre.set(mResolutionX / 2, mResolutionY / 2);

				mCoordinateHelper.scaleAboutScreenPoint(
						screenCentre,
						(float)Math.pow(
								1.01,
								mDragFinger.mPosDifference.y / mScreenDensity)
				);

				break;
			}

			case ONE_FINGER_DOWN: {

				if(mDragFinger.mTotalPosDifference.distanceSquared()
						>= 100f * mScreenDensity * mScreenDensity) {
					mCurrentTouchState = TouchState.ONE_FINGER_DRAG;
				}

				// Deliberate fall-through
			}

			case ONE_FINGER_DRAG:
				if(mBoundsHelper.isMinScale()) {
					mListener.onHorizontalSwipe(mDragFinger.mTotalPosDifference.x);
				} else {
					mCoordinateHelper.translateScreen(
							mDragFinger.mLastPos,
							mDragFinger.mCurrentPos);
				}
				break;

			case TWO_FINGER_PINCH: {

				final double oldDistance =
						mPinchFinger1.mLastPos.euclideanDistanceTo(mPinchFinger2.mLastPos);
				final double newDistance =
						mPinchFinger1.mCurrentPos.euclideanDistanceTo(mPinchFinger2.mCurrentPos);

				final MutableFloatPoint2D oldCentre = mTmpPoint1_onFingersMoved;
				mPinchFinger1.mLastPos.add(mPinchFinger2.mLastPos, oldCentre);
				oldCentre.scale(0.5);

				final MutableFloatPoint2D newCentre = mTmpPoint2_onFingersMoved;
				mPinchFinger1.mCurrentPos.add(mPinchFinger2.mCurrentPos, newCentre);
				newCentre.scale(0.5);

				final float scaleDifference = (float)(newDistance / oldDistance);

				mCoordinateHelper.scaleAboutScreenPoint(newCentre, scaleDifference);
				mCoordinateHelper.translateScreen(oldCentre, newCentre);

				break;
			}
		}
	}

	@Override
	public synchronized void onFingerUp(final FingerTracker.Finger finger) {

		if(mScrollbars == null) {
			return;
		}

		mScaleAnimation = null;
		mScrollbars.showBars();

		if(mSpareFingers.remove(finger)) {
			return;
		}

		if(mCurrentTouchState == null) {
			return;
		}

		switch(mCurrentTouchState) {

			case DOUBLE_TAP_ONE_FINGER_DOWN:

				if(finger.mDownDuration < TAP_MAX_DURATION_MS) {
					onDoubleTap(finger.mCurrentPos);
				}

				mCurrentTouchState = null;
				mDragFinger = null;
				break;

			case ONE_FINGER_DOWN:

				if(finger.mDownDuration < TAP_MAX_DURATION_MS) {

					// Maybe a single tap
					mDoubleTapGapTimer.startTimer();

					mCurrentTouchState = TouchState.DOUBLE_TAP_WAIT_NO_FINGERS_DOWN;
					mFirstTapReleaseTime = System.currentTimeMillis();

				} else {
					mCurrentTouchState = null;
				}

				mDragFinger = null;
				break;

			case ONE_FINGER_DRAG:

				mListener.onHorizontalSwipeEnd();

				// Deliberate fallthrough

			case DOUBLE_TAP_ONE_FINGER_DRAG:

				if(mSpareFingers.isEmpty()) {
					mCurrentTouchState = null;
					mDragFinger = null;
				} else {
					mDragFinger = mSpareFingers.pop();
				}

				break;

			case TWO_FINGER_PINCH:

				if(mSpareFingers.isEmpty()) {
					mCurrentTouchState = TouchState.ONE_FINGER_DRAG;
					mDragFinger =
							(mPinchFinger1 == finger) ? mPinchFinger2 : mPinchFinger1;
					mPinchFinger1 = null;
					mPinchFinger2 = null;

				} else {
					if(mPinchFinger1 == finger) {
						mPinchFinger1 = mSpareFingers.pop();
					} else {
						mPinchFinger2 = mSpareFingers.pop();
					}
				}
				break;
		}
	}

	private void onDoubleTap(final MutableFloatPoint2D position) {

		final float minScale = mBoundsHelper.getMinScale();
		final float currentScale = mCoordinateHelper.getScale();

		float targetScale;

		if(currentScale > minScale * 1.01) {
			targetScale = minScale;

		} else {
			targetScale = Math.max(
					(float)mResolutionX / (float)mImageTileSource.getWidth(),
					(float)mResolutionY / (float)mImageTileSource.getHeight()
			);

			if(Math.abs((targetScale / currentScale) - 1.0) < 0.05) {
				targetScale = currentScale * 3;
			}
		}

		mScaleAnimation =
				new ImageViewScaleAnimation(targetScale, mCoordinateHelper, 15, position);
	}

	@Override
	public void onUIThreadRepeatingTimer(final UIThreadRepeatingTimer timer) {

		if(mCurrentTouchState == TouchState.DOUBLE_TAP_WAIT_NO_FINGERS_DOWN) {

			if(System.currentTimeMillis() - mFirstTapReleaseTime
					> DOUBLE_TAP_MAX_GAP_DURATION_MS) {
				mListener.onSingleTap();
				mCurrentTouchState = null;
				mDoubleTapGapTimer.stopTimer();
			}

		} else {
			mDoubleTapGapTimer.stopTimer();
		}
	}

	private int pickSampleSize() {

		int result = 1;

		while(result <= MultiScaleTileManager.MAX_SAMPLE_SIZE
				&& (1.0 / (result * 2)) > mCoordinateHelper.getScale()) {
			result *= 2;
		}

		return result;
	}

	@Override
	public void onTileLoaded(final int x, final int y, final int sampleSize) {
		mRefreshable.refresh();
	}

	@Override
	public void onTileLoaderOutOfMemory() {
		mListener.onImageViewDLMOutOfMemory();
	}

	@Override
	public void onTileLoaderException(final Throwable t) {
		mListener.onImageViewDLMException(t);
	}

	public void resetTouchState() {
		mCurrentTouchState = null;
	}
}
