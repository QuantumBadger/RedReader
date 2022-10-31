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

package org.saiditnet.redreader.image;

import android.graphics.Bitmap;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.widget.ImageView;

import java.io.InputStream;
import java.util.concurrent.atomic.AtomicBoolean;

import jp.tomorrowkey.android.gifplayer.GifDecoder;

public class GifDecoderThread extends Thread {

	private volatile boolean playing = true;
	private final InputStream is;
	private ImageView view;
	private final OnGifLoadedListener listener;

	public void setView(ImageView view) {
		this.view = view;
	}

	public interface OnGifLoadedListener {
		void onGifLoaded();

		void onOutOfMemory();

		void onGifInvalid();
	}

	private final Handler handler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(Message msg) {
			if(playing && view != null) view.setImageBitmap((Bitmap)msg.obj);
		}
	};

	public GifDecoderThread(InputStream is, OnGifLoadedListener listener) {
		super("GIF playing thread");
		this.is = is;
		this.listener = listener;
	}

	public void stopPlaying() {
		playing = false;
		interrupt();

		try {
			is.close();
		} catch(Throwable t) {}
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
				} catch(Throwable t) {
					t.printStackTrace();
					failed.set(true);
				}
			}
		}.start();

		try {

			if(!playing) return;

			listener.onGifLoaded();

			int frame = 0;

			while(playing) {

				while(decoder.getFrameCount() <= frame + 1 && !loaded.get() && !failed.get()) {
					try {
						sleep(100);
					} catch(InterruptedException e) {
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
				} catch(InterruptedException e) {
					return;
				}

				if(failed.get()) {
					listener.onGifInvalid();
					return;
				}

				frame++;
			}

		} catch(OutOfMemoryError e) {
			listener.onOutOfMemory();

		} catch(Throwable t) {
			listener.onGifInvalid();
		}
	}
}
