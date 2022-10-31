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

package org.saiditnet.redreader.views.liststatus;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.text.TextUtils;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;

public final class LoadingView extends StatusListItemView {

	private final TextView textView;

	private static final int LOADING_INDETERMINATE = -1, LOADING_DONE = -2;

	private final Handler loadingHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {

			if(textView != null) {
				textView.setText(((String) msg.obj).toUpperCase());
			}

			if(msg.what == LOADING_INDETERMINATE) {
				// TODO

			} else if(msg.what == LOADING_DONE) {
				// TODO
				hideNoAnim();

			} else {
				// TODO progress is msg.what
			}
		}
	};

	public void setIndeterminate(final int textRes) {
		sendMessage(getContext().getString(textRes), LOADING_INDETERMINATE);
	}

	public void setProgress(final int textRes, final float fraction) {
		sendMessage(getContext().getString(textRes), Math.round(fraction * 100));
	}

	public void setDone(final int textRes) {
		sendMessage(getContext().getString(textRes), LOADING_DONE);
	}

	private void sendMessage(final String text, final int what) {
		final Message msg = Message.obtain();
		msg.obj = text;
		msg.what = what;
		loadingHandler.sendMessage(msg);
	}

	public LoadingView(final Context context) {
		this(context, R.string.download_waiting, true, true);
	}

	public LoadingView(final Context context, final int initialTextRes, final boolean progressBarEnabled, final boolean indeterminate) {
		this(context, context.getString(initialTextRes), progressBarEnabled, indeterminate);
	}

	public LoadingView(final Context context, final String initialText, final boolean progressBarEnabled, final boolean indeterminate) {

		super(context);

		final LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.VERTICAL);

		textView = new TextView(context);
		textView.setText(initialText.toUpperCase());
		textView.setTextSize(13.0f);
		textView.setPadding((int)(15 * dpScale), (int)(10 * dpScale), (int)(10 * dpScale), (int)(10 * dpScale));
		textView.setSingleLine(true);
		textView.setEllipsize(TextUtils.TruncateAt.END);
		layout.addView(textView);

		setContents(layout);
	}
}
