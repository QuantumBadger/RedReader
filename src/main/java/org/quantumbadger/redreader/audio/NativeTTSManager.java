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

package org.quantumbadger.redreader.audio;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;
import android.util.Log;

import androidx.annotation.Nullable;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Locale;
import java.util.Queue;

/**
 * Reads a list of text items aloud using the system text-to-speech engine.
 * All methods must be called on the main thread; listener callbacks are also delivered there.
 */
public class NativeTTSManager {

	private static final String TAG = "NativeTTSManager";

	private static NativeTTSManager sInstance;

	private final Context mContext;
	private final Handler mHandler = new Handler(Looper.getMainLooper());
	private final Queue<TTSItem> mPendingItems = new ArrayDeque<>();

	@Nullable private TextToSpeech mTTS;
	private boolean mIsInitialized;
	private boolean mIsSpeaking;
	private float mSpeechRate = 1f;

	// Incremented on every stop/start so that late callbacks from old utterances are ignored
	private int mGeneration;

	@Nullable private Listener mListener;

	public static final class TTSItem {
		public final String text;
		public final int position;

		public TTSItem(final String text, final int position) {
			this.text = text;
			this.position = position;
		}
	}

	public interface Listener {
		void onTTSStateChanged(boolean isSpeaking);

		/** Called when an item with a non-negative position starts being spoken. */
		void onUtteranceStarted(int position);

		void onTTSError();
	}

	private NativeTTSManager(final Context context) {
		mContext = context.getApplicationContext();
	}

	public static synchronized NativeTTSManager getInstance(final Context context) {
		if(sInstance == null) {
			sInstance = new NativeTTSManager(context);
		}
		return sInstance;
	}

	public void setListener(@Nullable final Listener listener) {
		mListener = listener;
		if(listener != null) {
			listener.onTTSStateChanged(mIsSpeaking);
		}
	}

	/** Removes the listener, only if it is still the active one, and stops speaking. */
	public void clearListener(final Listener listener) {
		if(mListener == listener) {
			mListener = null;
			stop();
		}
	}

	public boolean isSpeaking() {
		return mIsSpeaking;
	}

	public void setSpeed(final float speed) {
		mSpeechRate = speed;
		if(mTTS != null && mIsInitialized) {
			mTTS.setSpeechRate(speed);
		}
	}

	public void readAloud(@Nullable final List<TTSItem> items) {

		stopInternal();

		if(items != null) {
			for(final TTSItem item : items) {
				if(item != null && !item.text.trim().isEmpty()) {
					mPendingItems.add(item);
				}
			}
		}

		if(mPendingItems.isEmpty()) {
			notifyState();
			return;
		}

		mIsSpeaking = true;
		notifyState();

		if(mTTS == null) {
			initialize();
		} else if(mIsInitialized) {
			speakNext();
		}
	}

	public void stop() {
		final boolean wasSpeaking = mIsSpeaking;
		stopInternal();
		if(wasSpeaking) {
			notifyState();
		}
	}

	private void stopInternal() {
		mGeneration++;
		mPendingItems.clear();
		mIsSpeaking = false;
		if(mTTS != null && mIsInitialized) {
			mTTS.stop();
		}
	}

	private void initialize() {

		final TextToSpeech tts = new TextToSpeech(
				mContext,
				status -> mHandler.post(() -> onInitialized(status)));

		tts.setOnUtteranceProgressListener(new UtteranceProgressListener() {
			@Override
			public void onStart(final String utteranceId) {
				mHandler.post(() -> onUtteranceStart(utteranceId));
			}

			@Override
			public void onDone(final String utteranceId) {
				mHandler.post(() -> onUtteranceEnd(utteranceId, false));
			}

			@Override
			public void onError(final String utteranceId) {
				mHandler.post(() -> onUtteranceEnd(utteranceId, true));
			}
		});

		mTTS = tts;
	}

	private void onInitialized(final int status) {

		final TextToSpeech tts = mTTS;

		if(tts == null) {
			return;
		}

		if(status != TextToSpeech.SUCCESS || tts.setLanguage(Locale.getDefault()) < 0) {
			Log.e(TAG, "TTS initialization failed (status " + status + ")");
			tts.shutdown();
			mTTS = null;
			mIsInitialized = false;
			fail();
			return;
		}

		mIsInitialized = true;
		tts.setSpeechRate(mSpeechRate);

		// The user may have stopped or restarted playback while the engine was starting
		if(mIsSpeaking) {
			speakNext();
		}
	}

	private void speakNext() {

		final TextToSpeech tts = mTTS;
		final TTSItem item = mPendingItems.poll();

		if(item == null || tts == null) {
			mIsSpeaking = false;
			notifyState();
			return;
		}

		// Engines reject input over a maximum length, so split long items up
		final int limit = TextToSpeech.getMaxSpeechInputLength();
		final String text = item.text;
		int start = 0;
		boolean first = true;

		while(start < text.length()) {

			int end = Math.min(start + limit, text.length());

			// Don't split a surrogate pair
			if(end < text.length() && Character.isHighSurrogate(text.charAt(end - 1))) {
				end--;
			}

			final int result = tts.speak(
					text.substring(start, end),
					first ? TextToSpeech.QUEUE_FLUSH : TextToSpeech.QUEUE_ADD,
					null,
					utteranceId(item.position, end >= text.length(), first));

			if(result != TextToSpeech.SUCCESS) {
				Log.e(TAG, "TTS speak() failed with code " + result);
				fail();
				return;
			}

			first = false;
			start = end;
		}
	}

	// Format: generation:position:isLastChunk:isFirstChunk
	private String utteranceId(final int position, final boolean last, final boolean first) {
		return mGeneration + ":" + position + ":" + (last ? 1 : 0) + ":" + (first ? 1 : 0);
	}

	private void onUtteranceStart(final String utteranceId) {

		final String[] parts = utteranceId.split(":");

		if(parts.length != 4 || Integer.parseInt(parts[0]) != mGeneration) {
			return;
		}

		final int position = Integer.parseInt(parts[1]);

		if(parts[3].equals("1") && position >= 0 && mListener != null) {
			mListener.onUtteranceStarted(position);
		}
	}

	private void onUtteranceEnd(final String utteranceId, final boolean error) {

		final String[] parts = utteranceId.split(":");

		if(parts.length != 4 || Integer.parseInt(parts[0]) != mGeneration) {
			return;
		}

		if(error) {
			fail();

		} else if(parts[2].equals("1")) {
			speakNext();
		}
	}

	private void fail() {
		stopInternal();
		notifyState();
		if(mListener != null) {
			mListener.onTTSError();
		}
	}

	private void notifyState() {
		if(mListener != null) {
			mListener.onTTSStateChanged(mIsSpeaking);
		}
	}
}
