package org.quantumbadger.redreader.views;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Movie;
import android.graphics.Paint;
import android.os.SystemClock;
import android.view.View;
import org.quantumbadger.redreader.common.AndroidApi;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public final class GIFView extends View {

	private final Movie mMovie;
	private long movieStart;

	private final Paint paint = new Paint();

	@SuppressLint("NewApi")
	public GIFView(Context context, InputStream is) {
		super(context);

		if (AndroidApi.isHoneyCombOrLater()) {
			setLayerType(View.LAYER_TYPE_SOFTWARE, null);
		}

        final byte[] data = streamToBytes(is); // workaround for strange Android bug
		mMovie = Movie.decodeByteArray(data, 0, data.length);

		if(mMovie.duration() < 1) {
			throw new RuntimeException("Invalid GIF");
		}

		paint.setAntiAlias(true);
		paint.setFilterBitmap(true);
		//paint.setDither(true);
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