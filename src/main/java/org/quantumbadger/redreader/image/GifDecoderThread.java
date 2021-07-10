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
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.widget.ImageView;
import androidx.annotation.NonNull;
import jp.tomorrowkey.android.gifplayer.GifDecoder;

import java.io.InputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class GifDecoderThread extends Thread {

	@NonNull private static final String TAG = "GifDecoderThread";

	private volatile boolean playing = true;
	private final InputStream is;
	private ImageView view;
	private final OnGifLoadedListener listener;

	public void setView(final ImageView view) {
		this.view = view;
	}

	public interface OnGifLoadedListener {
		void onGifLoaded();

		void onOutOfMemory();

		void onGifInvalid();
	}

	private final Handler handler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(@NonNull final Message msg) {
			if(playing && view != null) {
				view.setImageBitmap((Bitmap)msg.obj);
			}
		}
	};

	public GifDecoderThread(final InputStream is, final OnGifLoadedListener listener) {
		super("GIF playing thread");
		this.is = is;
		this.listener = listener;
	}

	public void stopPlaying() {
		playing = false;
		interrupt();

		try {
			is.close();
		} catch(final Throwable t) {
			Log.e(TAG, "Exception while stopping", t);
		}
	}

	@Override
	public void run() {

		final AtomicBoolean loaded = new AtomicBoolean(false);
		final AtomicBoolean failed = new AtomicBoolean(false);

		final GifDecoder decoder = new GifDecoder();

		new Thread("GIF decoding thread") {
			@Override
			public void run() {
				try {
					decoder.read(is);
					loaded.set(true);
				} catch(final Throwable t) {
					Log.i(TAG, "Got exception", t);
					failed.set(true);
				}
			}
		}.start();

		try {

			if(!playing) {
				return;
			}

			listener.onGifLoaded();

			int frame = 0;

			while(playing) {

				while(decoder.getFrameCount() <= frame + 1
						&& !loaded.get()
						&& !failed.get()) {
					try {
						sleep(100);
					} catch(final InterruptedException e) {
						return;
					}
				}

				frame = frame % decoder.getFrameCount();

				final Bitmap img = decoder.getFrame(frame);

				final Message msg = Message.obtain();
				msg.obj = img;
				handler.sendMessage(msg);

				try {
					sleep(Math.max(32, decoder.getDelay(frame)));
				} catch(final InterruptedException e) {
					return;
				}

				if(failed.get()) {
					listener.onGifInvalid();
					return;
				}

				frame++;
			}

		} catch(final OutOfMemoryError e) {
			listener.onOutOfMemory();

		} catch(final Throwable t) {
			listener.onGifInvalid();
		}
	}
}
