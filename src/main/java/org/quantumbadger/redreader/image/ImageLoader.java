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

package org.quantumbadger.redreader.image;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.util.Pair;
import android.widget.ImageView;

import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidApi;

import java.util.concurrent.LinkedBlockingDeque;

/**
 * Created by Mario Kosmiskas on 5/21/17.
 */

public class ImageLoader {

	private static ImageLoader mInstance = new ImageLoader();

	private final Thread mWorker;
	private final LinkedBlockingDeque<Pair<CacheManager.ReadableCacheFile,ImageView>> mQueue;

	private final Runnable mTask = new Runnable() {
		@Override
		public void run() {
			while (true) {
				try {
					final Pair task = mQueue.take();
					final Bitmap image = BitmapFactory.decodeStream(((CacheManager.ReadableCacheFile)task.first).getInputStream());

					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {
							((ImageView)task.second).setImageBitmap(image);
						}
					});
				} catch (Exception e) {
					Log.e("ImageLoader", "Error loading image from cache", e);
				}

			}
		}
	};

	private ImageLoader() {
		mQueue = new LinkedBlockingDeque<>();

		mWorker = new Thread(mTask, "Image Loader");
		mWorker.start();
	}

	public static void loadImage(CacheManager.ReadableCacheFile imageFile, ImageView target) {
		mInstance.mQueue.add(new Pair(imageFile, target));
	}
}
