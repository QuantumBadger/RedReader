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

package org.quantumbadger.redreader.cache;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.AndroidCommon;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class CacheActivityTracker {

	public static class ActiveRequest {

		private static final AtomicLong mLastRequestId = new AtomicLong(0);

		public final long id;
		public final String url;
		public final AtomicInteger progressPercent = new AtomicInteger(-1);

		public ActiveRequest(final String url) {
			this.url = url;
			id = mLastRequestId.incrementAndGet();
		}
	}

	@NonNull private static final ArrayList<ActiveRequest> mActiveRequests = new ArrayList<>();

	public static synchronized void registerRequest(@NonNull final ActiveRequest request) {
		mActiveRequests.add(request);
	}

	public static synchronized void unregisterRequest(@NonNull final ActiveRequest request) {
		mActiveRequests.remove(request);
	}

	public static synchronized ActiveRequest[] getCurrentRequests() {
		return mActiveRequests.toArray(new ActiveRequest[0]);
	}

	public static View getWidget(@NonNull final Context context) {

		final FrameLayout outer = new FrameLayout(context);

		final TextView tv = new TextView(context);
		outer.addView(tv);

		outer.setBackgroundColor(Color.argb(127, 50, 50, 50));
		tv.setTextColor(Color.WHITE);
		tv.setTextSize(14);
		tv.setText("Not updated yet");


		final AtomicBoolean attached = new AtomicBoolean(false);

		final Runnable timer = new Runnable() {

			@Override
			public void run() {

				if(!attached.get()) {
					return;
				}

				final StringBuilder sb = new StringBuilder("Cache Tracker");

				for(final ActiveRequest request : getCurrentRequests()) {

					if(sb.length() > 0) {
						sb.append("\n");
					}

					sb.append(request.id)
							.append(" (")
							.append(request.progressPercent)
							.append("%) ")
							.append(request.url);
				}

				tv.setText(sb.toString());

				AndroidCommon.UI_THREAD_HANDLER.post(this);
			}
		};

		outer.addOnAttachStateChangeListener(new View.OnAttachStateChangeListener() {
			@Override
			public void onViewAttachedToWindow(final View v) {
				attached.set(true);
				AndroidCommon.UI_THREAD_HANDLER.post(timer);
			}

			@Override
			public void onViewDetachedFromWindow(final View v) {
				attached.set(false);
				AndroidCommon.UI_THREAD_HANDLER.removeCallbacks(timer);
			}
		});

		return outer;
	}
}
