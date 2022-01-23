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

package org.quantumbadger.redreader.views.video;

import android.content.Context;
import android.graphics.Color;
import android.os.Build;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;
import androidx.annotation.DrawableRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import com.google.android.exoplayer2.ExoPlayer;
import com.google.android.exoplayer2.PlaybackException;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.source.MediaSource;
import com.google.android.exoplayer2.trackselection.DefaultTrackSelector;
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout;
import com.google.android.exoplayer2.ui.DefaultTimeBar;
import com.google.android.exoplayer2.ui.PlayerView;
import com.google.android.exoplayer2.ui.TimeBar;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRTime;

import java.util.concurrent.atomic.AtomicReference;

public class ExoPlayerWrapperView extends FrameLayout {

	public interface Listener {
		void onError();
	}

	private static final String TAG = "ExoPlayerWrapperView";

	@NonNull private final Listener mListener;

	@NonNull private final ExoPlayer mVideoPlayer;

	@Nullable private final RelativeLayout mControlView;

	@Nullable private final DefaultTimeBar mTimeBarView;
	@Nullable private final TextView mTimeTextView;

	private boolean mReleased;

	public ExoPlayerWrapperView(
			@NonNull final Context context,
			@NonNull final MediaSource mediaSource,
			@NonNull final Listener listener,
			final int controlsMarginRightDp) {

		super(context);
		mListener = listener;

		final DefaultTrackSelector trackSelector = new DefaultTrackSelector(context);

		mVideoPlayer = new ExoPlayer.Builder(context)
				.setTrackSelector(trackSelector)
				.build();

		final PlayerView videoPlayerView = new PlayerView(context);

		addView(videoPlayerView);

		videoPlayerView.setPlayer(mVideoPlayer);
		videoPlayerView.setShowBuffering(PlayerView.SHOW_BUFFERING_ALWAYS);
		videoPlayerView.requestFocus();

		mVideoPlayer.setMediaSource(mediaSource);
		mVideoPlayer.prepare();

		mVideoPlayer.setRepeatMode(Player.REPEAT_MODE_ONE);

		mVideoPlayer.setPlayWhenReady(true);
		videoPlayerView.setUseController(false);

		if(PrefsUtility.pref_behaviour_video_zoom_default()) {
			videoPlayerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_ZOOM);
		} else {
			videoPlayerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
		}

		if(PrefsUtility.pref_behaviour_video_playback_controls()) {

			mControlView = new RelativeLayout(context);
			addView(mControlView);

			final LinearLayout controlBar = new LinearLayout(context);
			mControlView.addView(controlBar);
			controlBar.setBackgroundColor(Color.argb(127, 127, 127, 127));
			controlBar.setOrientation(LinearLayout.VERTICAL);

			{
				final RelativeLayout.LayoutParams controlBarLayoutParams
						= (RelativeLayout.LayoutParams)controlBar.getLayoutParams();
				controlBarLayoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
				controlBarLayoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
				controlBarLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
				controlBarLayoutParams.rightMargin = General.dpToPixels(
						context,
						controlsMarginRightDp);
			}

			final LinearLayout buttons = new LinearLayout(context);
			controlBar.addView(buttons);
			buttons.setOrientation(LinearLayout.HORIZONTAL);
			General.setLayoutMatchWidthWrapHeight(buttons);

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_previous,
					R.string.video_restart,
					view -> {
						mVideoPlayer.seekTo(0);
						updateProgress();
					}), buttons);

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_rewind,
					R.string.video_rewind,
					view -> {
						if(mVideoPlayer.getCurrentPosition() > 3000) {
							mVideoPlayer.seekTo(mVideoPlayer.getCurrentPosition() - 3000);
						} else {
							mVideoPlayer.seekTo(0);
						}

						updateProgress();
					}), buttons);

			{
				final AtomicReference<ImageButton> playButton = new AtomicReference<>();

				playButton.set(createButton(
						context,
						mControlView,
						R.drawable.exo_controls_pause,
						R.string.video_pause,
						view -> {
							mVideoPlayer.setPlayWhenReady(!mVideoPlayer.getPlayWhenReady());

							if(mVideoPlayer.getPlayWhenReady()) {
								playButton.get()
										.setImageResource(R.drawable.exo_controls_pause);
								playButton.get().setContentDescription(
										context.getString(R.string.video_pause));
							} else {
								playButton.get()
										.setImageResource(R.drawable.exo_controls_play);
								playButton.get().setContentDescription(
										context.getString(R.string.video_play));
							}

							updateProgress();
						}));

				addButton(playButton.get(), buttons);
			}

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_fastforward,
					R.string.video_fast_forward,
					view -> {
						mVideoPlayer.seekTo(mVideoPlayer.getCurrentPosition() + 3000);
						updateProgress();
					}), buttons);

			{
				final AtomicReference<ImageButton> zoomButton = new AtomicReference<>();

				zoomButton.set(createButton(
						context,
						mControlView,
						R.drawable.ic_zoom_in_dark,
						R.string.video_zoom_in,
						v -> {
							if (videoPlayerView.getResizeMode()
									== AspectRatioFrameLayout.RESIZE_MODE_FIT) {
								videoPlayerView.setResizeMode(
										AspectRatioFrameLayout.RESIZE_MODE_ZOOM);
								zoomButton.get().setImageResource(R.drawable.ic_zoom_out_dark);
								zoomButton.get().setContentDescription(
										context.getString(R.string.video_zoom_out));
							} else {
								videoPlayerView.setResizeMode(
										AspectRatioFrameLayout.RESIZE_MODE_FIT);
								zoomButton.get().setImageResource(R.drawable.ic_zoom_in_dark);
								zoomButton.get().setContentDescription(
										context.getString(R.string.video_zoom_in));
							}
						}));

				if(videoPlayerView.getResizeMode() == AspectRatioFrameLayout.RESIZE_MODE_ZOOM) {
					zoomButton.get().setImageResource(R.drawable.ic_zoom_out_dark);
					zoomButton.get().setContentDescription(
							context.getString(R.string.video_zoom_out));
				}

				addButton(zoomButton.get(), buttons);
			}

			mTimeBarView = new DefaultTimeBar(context, null);
			controlBar.addView(mTimeBarView);

			{
				final LinearLayout.LayoutParams seekBarLayoutParams
						= (LinearLayout.LayoutParams)mTimeBarView.getLayoutParams();

				final int marginPx = General.dpToPixels(context, 8);

				seekBarLayoutParams.setMargins(marginPx, marginPx, marginPx, marginPx);
			}

			mTimeBarView.addListener(new TimeBar.OnScrubListener() {
				@Override
				public void onScrubStart(final TimeBar timeBar, final long position) {

				}

				@Override
				public void onScrubMove(final TimeBar timeBar, final long position) {
					mVideoPlayer.seekTo(position);
				}

				@Override
				public void onScrubStop(
						final TimeBar timeBar,
						final long position,
						final boolean canceled) {
					mVideoPlayer.seekTo(position);
				}
			});

			final Runnable updateProgressRunnable = new Runnable() {
				@Override
				public void run() {
					updateProgress();

					if(!mReleased) {
						AndroidCommon.UI_THREAD_HANDLER.postDelayed(this, 150);
					}
				}
			};

			updateProgressRunnable.run();

			{
				mTimeTextView = new TextView(context);
				controlBar.addView(mTimeTextView);
				mTimeTextView.setTextColor(Color.WHITE);
				mTimeTextView.setTextSize(18);

				final int marginSidesPx = General.dpToPixels(context, 16);
				final int marginBottomPx = General.dpToPixels(context, 8);

				((MarginLayoutParams)mTimeTextView.getLayoutParams())
						.setMargins(marginSidesPx, 0, marginSidesPx, marginBottomPx);

				if(Build.VERSION.SDK_INT >= 19) {
					mTimeTextView.setImportantForAccessibility(
							IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS);
				}
			}

			mControlView.setVisibility(GONE);

		} else {
			mControlView = null;
			mTimeBarView = null;
			mTimeTextView = null;
		}

		videoPlayerView.setLayoutParams(new FrameLayout.LayoutParams(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.MATCH_PARENT));

		mVideoPlayer.addListener(new Player.Listener() {
			@Override
			public void onPlayerError(@NonNull final PlaybackException error) {
				Log.e(TAG, "ExoPlayer error", error);
				mListener.onError();
			}

			@Override
			public void onPositionDiscontinuity(
					@NonNull final Player.PositionInfo oldPosition,
					@NonNull final Player.PositionInfo newPosition,
					final int reason) {

				updateProgress();
			}
		});
	}

	public void handleTap() {

		if(mControlView == null) {
			return;
		}

		if(mControlView.getVisibility() != VISIBLE) {
			mControlView.setVisibility(VISIBLE);
		} else {
			mControlView.setVisibility(GONE);
		}
	}

	public void release() {

		if(!mReleased) {
			removeAllViews();
			mVideoPlayer.release();
			mReleased = true;
		}
	}

	private static ImageButton createButton(
			@NonNull final Context context,
			@NonNull final ViewGroup root,
			@DrawableRes final int image,
			@StringRes final int description,
			@NonNull final OnClickListener clickListener) {

		final ImageButton ib = (ImageButton)LayoutInflater.from(context).inflate(
				R.layout.flat_image_button,
				root,
				false);

		final int buttonPadding = General.dpToPixels(context, 14);
		ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding);

		ib.setImageResource(image);
		ib.setContentDescription(context.getString(description));

		ib.setOnClickListener(clickListener);

		return ib;
	}

	private static void addButton(final ImageButton button, final LinearLayout layout) {

		layout.addView(button);

		final LinearLayout.LayoutParams layoutParams =
				(LinearLayout.LayoutParams)button.getLayoutParams();

		layoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
		layoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
	}

	public void updateProgress() {

		if(mTimeBarView != null && mTimeTextView != null && !mReleased) {

			final long durationMs = mVideoPlayer.getDuration();

			if(durationMs > 0) {

				final long currentPositionMs = mVideoPlayer.getCurrentPosition();

				mTimeBarView.setDuration(durationMs);
				mTimeBarView.setPosition(currentPositionMs);
				mTimeBarView.setBufferedPosition(mVideoPlayer.getBufferedPosition());

				final String newText = RRTime.msToMinutesAndSecondsString(currentPositionMs)
						+ " / "
						+ RRTime.msToMinutesAndSecondsString(durationMs);

				if(!newText.contentEquals(mTimeTextView.getText())) {
					mTimeTextView.setText(newText);
				}

			} else {
				mTimeBarView.setDuration(0);
				mTimeBarView.setPosition(0);
				mTimeBarView.setBufferedPosition(0);
			}
		}
	}

	public boolean isMuted() {
		return mVideoPlayer.getVolume() < 0.01f;
	}

	public void setMuted(final boolean mute) {
		mVideoPlayer.setVolume(mute ? 0 : 1);
	}

	public int isControlViewVisible() {
		return mControlView != null ? mControlView.getVisibility() : GONE;
	}
}
