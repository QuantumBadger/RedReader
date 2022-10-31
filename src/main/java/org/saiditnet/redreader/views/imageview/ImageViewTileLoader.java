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

import android.graphics.Bitmap;
import android.support.annotation.UiThread;
import android.util.Log;

import org.saiditnet.redreader.common.AndroidCommon;

public class ImageViewTileLoader {

	@UiThread
	public interface Listener {
		void onTileLoaded(int x, int y, int sampleSize);

		void onTileLoaderOutOfMemory();

		void onTileLoaderException(Throwable t);
	}

	private final ImageTileSource mSource;
	private final ImageViewTileLoaderThread mThread;
	private final int mX, mY, mSampleSize;

	private boolean mWanted;

	private Bitmap mResult;

	private final Listener mListener;

	private final Runnable mNotifyRunnable;

	private final Object mLock;

	public ImageViewTileLoader(
			ImageTileSource source,
			ImageViewTileLoaderThread thread,
			int x,
			int y,
			int sampleSize,
			Listener listener,
			final Object lock) {

		mSource = source;
		mThread = thread;
		mX = x;
		mY = y;
		mSampleSize = sampleSize;
		mListener = listener;
		mLock = lock;

		mNotifyRunnable = new Runnable() {
			@Override
			public void run() {
				mListener.onTileLoaded(mX, mY, mSampleSize);
			}
		};
	}

	// Caller must synchronize on mLock
	public void markAsWanted() {

		if(mWanted) {
			return;
		}

		if(mResult != null) {
			throw new RuntimeException("Not wanted, but the image is loaded anyway!");
		}

		mThread.enqueue(this);
		mWanted = true;
	}

	public void doPrepare() {

		synchronized(mLock) {

			if(!mWanted) {
				return;
			}

			if(mResult != null) {
				return;
			}
		}

		final Bitmap tile;

		try {
			tile = mSource.getTile(mSampleSize, mX, mY);

		} catch(OutOfMemoryError e) {
			AndroidCommon.UI_THREAD_HANDLER.post(new NotifyOOMRunnable());
			return;

		} catch(Throwable t) {
			Log.e("ImageViewTileLoader", "Exception in getTile()", t);
			AndroidCommon.UI_THREAD_HANDLER.post(new NotifyErrorRunnable(t));
			return;
		}

		synchronized(mLock) {
			if(mWanted) {
				mResult = tile;
			} else if(tile != null) {
				tile.recycle();
			}
		}

		AndroidCommon.UI_THREAD_HANDLER.post(mNotifyRunnable);
	}

	public Bitmap get() {

		synchronized(mLock) {

			if(!mWanted) {
				throw new RuntimeException("Attempted to get unwanted image!");
			}

			return mResult;
		}
	}

	// Caller must synchronize on mLock
	public void markAsUnwanted() {

		mWanted = false;

		if(mResult != null) {
			mResult.recycle();
			mResult = null;
		}
	}

	private class NotifyOOMRunnable implements Runnable {
		@Override
		public void run() {
			mListener.onTileLoaderOutOfMemory();
		}
	}

	private class NotifyErrorRunnable implements Runnable {

		private final Throwable mError;

		private NotifyErrorRunnable(Throwable mError) {
			this.mError = mError;
		}

		@Override
		public void run() {
			mListener.onTileLoaderException(mError);
		}
	}
}
