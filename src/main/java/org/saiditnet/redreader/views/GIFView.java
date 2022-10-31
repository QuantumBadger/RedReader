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

package org.saiditnet.redreader.views;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Movie;
import android.graphics.Paint;
import android.os.SystemClock;
import android.view.View;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public final class GIFView extends View {

	private final Movie mMovie;
	private long movieStart;

	private final Paint paint = new Paint();

	public GIFView(Context context, InputStream is) {
		super(context);

		setLayerType(View.LAYER_TYPE_SOFTWARE, null);

		final byte[] data = streamToBytes(is); // workaround for strange Android bug
		mMovie = Movie.decodeByteArray(data, 0, data.length);

		if(mMovie.duration() < 1) {
			throw new RuntimeException("Invalid GIF");
		}

		paint.setAntiAlias(true);
		paint.setFilterBitmap(true);
	}

	protected void onDraw(final Canvas canvas) {
		canvas.drawColor(Color.TRANSPARENT);
		super.onDraw(canvas);
		long now = SystemClock.uptimeMillis();

		final float scale = Math.min((float)getWidth() / mMovie.width(), (float)getHeight() / mMovie.height());

		canvas.scale(scale, scale);
		canvas.translate(((float)getWidth() / scale - (float)mMovie.width())/2f,
				((float)getHeight() / scale - (float)mMovie.height())/2f);


		if(movieStart == 0) movieStart = (int)now;

		mMovie.setTime((int)((now - movieStart) % mMovie.duration()));
		mMovie.draw(canvas, 0, 0, paint);

		this.invalidate();
	}

	private static byte[] streamToBytes(final InputStream is) {

		final ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
		final byte[] buffer = new byte[1024];

		int len;

		try {
			while ((len = is.read(buffer)) >= 0) {
				baos.write(buffer, 0, len);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		return baos.toByteArray();
	}
}
