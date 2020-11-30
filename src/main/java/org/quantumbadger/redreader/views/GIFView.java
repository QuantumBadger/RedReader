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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Movie;
import android.graphics.Paint;
import android.os.SystemClock;
import android.view.View;
import androidx.annotation.NonNull;

public final class GIFView extends View {

	private final Movie mMovie;
	private long movieStart;

	private final Paint paint = new Paint();

	public static Movie prepareMovie(
			@NonNull final byte[] data,
			final int offset,
			final int length) {

		final Movie movie = Movie.decodeByteArray(data, offset, length);

		if(movie.duration() < 1) {
			throw new RuntimeException("Invalid GIF");
		}

		return movie;
	}

	// Accept as byte[] rather than stream due to Android bug workaround
	public GIFView(final Context context, final Movie movie) {
		super(context);

		setLayerType(View.LAYER_TYPE_SOFTWARE, null);

		mMovie = movie;

		paint.setAntiAlias(true);
		paint.setFilterBitmap(true);
	}

	@Override
	protected void onDraw(final Canvas canvas) {
		canvas.drawColor(Color.TRANSPARENT);
		super.onDraw(canvas);
		final long now = SystemClock.uptimeMillis();

		final float scale = Math.min(
				(float)getWidth() / mMovie.width(),
				(float)getHeight() / mMovie.height());

		canvas.scale(scale, scale);
		canvas.translate(
				((float)getWidth() / scale - (float)mMovie.width()) / 2f,
				((float)getHeight() / scale - (float)mMovie.height()) / 2f);


		if(movieStart == 0) {
			movieStart = (int)now;
		}

		mMovie.setTime((int)((now - movieStart) % mMovie.duration()));
		mMovie.draw(canvas, 0, 0, paint);

		this.invalidate();
	}
}
