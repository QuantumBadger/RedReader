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

package org.quantumbadger.redreader.test.audio;

import android.content.Context;
import android.os.Bundle;
import android.os.Looper;
import android.speech.tts.TextToSpeech;
import android.speech.tts.UtteranceProgressListener;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.audio.NativeTTSManager;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.Shadows;
import org.robolectric.annotation.Config;
import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.robolectric.annotation.LooperMode;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 35, shadows = {NativeTTSManagerTest.TestTTS.class})
@LooperMode(LooperMode.Mode.PAUSED)
public class NativeTTSManagerTest {

	private static final int MAX_LENGTH = 4000;

	private NativeTTSManager mManager;
	private RecordingListener mListener;

	@Before
	public void setUp() throws Exception {

		TestTTS.reset();

		// Each test needs a fresh manager, and therefore a fresh (fake) engine
		final Field singleton = NativeTTSManager.class.getDeclaredField("sInstance");
		singleton.setAccessible(true);
		singleton.set(null, null);

		mManager = NativeTTSManager.getInstance(RuntimeEnvironment.getApplication());
		mListener = new RecordingListener();
		mManager.setListener(mListener);
	}

	private void read(final String... texts) {

		final List<NativeTTSManager.TTSItem> items = new ArrayList<>();

		for(int i = 0; i < texts.length; i++) {
			items.add(new NativeTTSManager.TTSItem(texts[i], i));
		}

		mManager.readAloud(items);
	}

	private static void idle() {
		Shadows.shadowOf(Looper.getMainLooper()).idle();
	}

	private static void finishUtterance(final int index) {
		TestTTS.sProgress.onDone(TestTTS.sIds.get(index));
		idle();
	}

	@Test
	public void testNothingSpokenUntilEngineInitialized() {

		read("one");

		Assert.assertTrue(mManager.isSpeaking());
		Assert.assertTrue(TestTTS.sSpoken.isEmpty());

		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);
	}

	@Test
	public void testItemsSpokenInOrderOneAtATime() {

		read("one", "two", "three");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);

		finishUtterance(0);
		Assert.assertEquals(Arrays.asList("one", "two"), TestTTS.sSpoken);

		finishUtterance(1);
		Assert.assertEquals(Arrays.asList("one", "two", "three"), TestTTS.sSpoken);
		Assert.assertTrue(mManager.isSpeaking());

		finishUtterance(2);
		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(Boolean.FALSE, mListener.mLastState);
	}

	@Test
	public void testBlankItemsAreSkipped() {

		read("one", "   ", "", "two");
		TestTTS.init(TextToSpeech.SUCCESS);
		finishUtterance(0);

		Assert.assertEquals(Arrays.asList("one", "two"), TestTTS.sSpoken);
	}

	@Test
	public void testOnlyBlankItemsDoesNotStartSpeaking() {

		read(" ", "");

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertNull(TestTTS.sInitListener);
	}

	@Test
	public void testNullItemsStops() {

		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		mManager.readAloud(null);

		Assert.assertFalse(mManager.isSpeaking());
	}

	@Test
	public void testLongItemIsSplitAndNextItemWaitsForLastChunk() {

		final String text = "a".repeat(MAX_LENGTH * 2 + 10);

		read(text, "next");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(3, TestTTS.sSpoken.size());
		Assert.assertEquals(text, String.join("", TestTTS.sSpoken));

		for(final String chunk : TestTTS.sSpoken) {
			Assert.assertTrue(chunk.length() <= MAX_LENGTH);
		}

		Assert.assertEquals(TextToSpeech.QUEUE_FLUSH, (int)TestTTS.sQueueModes.get(0));
		Assert.assertEquals(TextToSpeech.QUEUE_ADD, (int)TestTTS.sQueueModes.get(1));

		// Finishing the earlier chunks must not move on to the next item
		finishUtterance(0);
		finishUtterance(1);
		Assert.assertEquals(3, TestTTS.sSpoken.size());

		finishUtterance(2);
		Assert.assertEquals("next", TestTTS.sSpoken.get(3));
	}

	@Test
	public void testSurrogatePairsAreNotSplit() {

		final String text = "a".repeat(MAX_LENGTH - 1) + "😀" + "end";

		read(text);
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(2, TestTTS.sSpoken.size());
		Assert.assertEquals(text, String.join("", TestTTS.sSpoken));

		for(final String chunk : TestTTS.sSpoken) {
			Assert.assertFalse(Character.isHighSurrogate(chunk.charAt(chunk.length() - 1)));
		}
	}

	@Test
	public void testPositionReportedOncePerItem() {

		final List<NativeTTSManager.TTSItem> items = new ArrayList<>();
		items.add(new NativeTTSManager.TTSItem("title", -1));
		items.add(new NativeTTSManager.TTSItem("a".repeat(MAX_LENGTH + 1), 7));

		mManager.readAloud(items);
		TestTTS.init(TextToSpeech.SUCCESS);

		TestTTS.sProgress.onStart(TestTTS.sIds.get(0));
		idle();
		Assert.assertTrue(mListener.mPositions.isEmpty());

		finishUtterance(0);

		TestTTS.sProgress.onStart(TestTTS.sIds.get(1));
		TestTTS.sProgress.onStart(TestTTS.sIds.get(2));
		idle();

		Assert.assertEquals(Arrays.asList(7), mListener.mPositions);
	}

	@Test
	public void testStopSilencesAndIgnoresLateCallbacks() {

		read("one", "two");
		TestTTS.init(TextToSpeech.SUCCESS);

		mManager.stop();

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(Boolean.FALSE, mListener.mLastState);
		Assert.assertEquals(1, TestTTS.sStops);

		// The engine may still report the old utterance finishing after stop()
		finishUtterance(0);
		TestTTS.sProgress.onStart(TestTTS.sIds.get(0));
		idle();

		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);
		Assert.assertTrue(mListener.mPositions.isEmpty());
	}

	@Test
	public void testRestartIgnoresCallbacksFromPreviousRun() {

		read("old one", "old two");
		TestTTS.init(TextToSpeech.SUCCESS);

		read("new one");
		Assert.assertEquals(Arrays.asList("old one", "new one"), TestTTS.sSpoken);

		finishUtterance(0);

		Assert.assertEquals(Arrays.asList("old one", "new one"), TestTTS.sSpoken);
		Assert.assertTrue(mManager.isSpeaking());
	}

	@Test
	public void testStopBeforeInitializationDoesNotSpeakLater() {

		read("one");
		mManager.stop();
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertTrue(TestTTS.sSpoken.isEmpty());
		Assert.assertFalse(mManager.isSpeaking());
	}

	@Test
	public void testInitializationFailureReportsErrorAndAllowsRetry() {

		read("one");
		TestTTS.init(TextToSpeech.ERROR);

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(1, mListener.mErrors);
		Assert.assertEquals(1, TestTTS.sShutdowns);

		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);
	}

	@Test
	public void testUnsupportedLanguageReportsError() {

		TestTTS.sLanguage = TextToSpeech.LANG_NOT_SUPPORTED;

		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(1, mListener.mErrors);
	}

	@Test
	public void testSpeakFailureReportsError() {

		TestTTS.sSpeakResult = TextToSpeech.ERROR;

		read("one", "two");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(1, mListener.mErrors);
		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);
	}

	@Test
	public void testEngineErrorCallbackReportsError() {

		read("one", "two");
		TestTTS.init(TextToSpeech.SUCCESS);

		TestTTS.sProgress.onError(TestTTS.sIds.get(0));
		idle();

		Assert.assertFalse(mManager.isSpeaking());
		Assert.assertEquals(1, mListener.mErrors);
		Assert.assertEquals(Arrays.asList("one"), TestTTS.sSpoken);
	}

	@Test
	public void testClearingActiveListenerStopsSpeech() {

		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		mManager.clearListener(mListener);

		Assert.assertFalse(mManager.isSpeaking());
	}

	@Test
	public void testClearingStaleListenerKeepsSpeaking() {

		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		final RecordingListener replacement = new RecordingListener();
		mManager.setListener(replacement);
		mManager.clearListener(mListener);

		Assert.assertTrue(mManager.isSpeaking());
	}

	@Test
	public void testSpeedAppliedAfterInitialization() {

		mManager.setSpeed(1.5f);
		read("one");
		TestTTS.init(TextToSpeech.SUCCESS);

		Assert.assertEquals(1.5f, TestTTS.sSpeechRate, 0f);

		mManager.setSpeed(0.5f);
		Assert.assertEquals(0.5f, TestTTS.sSpeechRate, 0f);
	}

	private static final class RecordingListener implements NativeTTSManager.Listener {

		private Boolean mLastState;
		private int mErrors;
		private final List<Integer> mPositions = new ArrayList<>();

		@Override
		public void onTTSStateChanged(final boolean isSpeaking) {
			mLastState = isSpeaking;
		}

		@Override
		public void onUtteranceStarted(final int position) {
			mPositions.add(position);
		}

		@Override
		public void onTTSError() {
			mErrors++;
		}
	}

	/** A fake engine, so the tests don't depend on a real text-to-speech service. */
	@Implements(value = TextToSpeech.class, callThroughByDefault = false)
	public static class TestTTS {

		static TextToSpeech.OnInitListener sInitListener;
		static UtteranceProgressListener sProgress;
		static int sLanguage;
		static int sSpeakResult;
		static int sStops;
		static int sShutdowns;
		static float sSpeechRate;
		static final List<String> sSpoken = new ArrayList<>();
		static final List<String> sIds = new ArrayList<>();
		static final List<Integer> sQueueModes = new ArrayList<>();

		static void reset() {
			sInitListener = null;
			sProgress = null;
			sLanguage = TextToSpeech.LANG_AVAILABLE;
			sSpeakResult = TextToSpeech.SUCCESS;
			sStops = 0;
			sShutdowns = 0;
			sSpeechRate = 0;
			sSpoken.clear();
			sIds.clear();
			sQueueModes.clear();
		}

		static void init(final int status) {
			sInitListener.onInit(status);
			idle();
		}

		@Implementation
		protected void __constructor__(
				final Context context,
				final TextToSpeech.OnInitListener callback) {
			sInitListener = callback;
		}

		@Implementation
		protected static int getMaxSpeechInputLength() {
			return MAX_LENGTH;
		}

		@Implementation
		protected int setLanguage(final Locale locale) {
			return sLanguage;
		}

		@Implementation
		protected int setOnUtteranceProgressListener(final UtteranceProgressListener listener) {
			sProgress = listener;
			return TextToSpeech.SUCCESS;
		}

		@Implementation
		protected int setSpeechRate(final float rate) {
			sSpeechRate = rate;
			return TextToSpeech.SUCCESS;
		}

		@Implementation
		protected int speak(
				final CharSequence text,
				final int queueMode,
				final Bundle params,
				final String utteranceId) {
			sSpoken.add(text.toString());
			sIds.add(utteranceId);
			sQueueModes.add(queueMode);
			return sSpeakResult;
		}

		@Implementation
		protected int stop() {
			sStops++;
			return TextToSpeech.SUCCESS;
		}

		@Implementation
		protected void shutdown() {
			sShutdowns++;
		}
	}
}
