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

package org.saiditnet.redreader.views.video;

import android.content.Context;
import android.graphics.Color;
import android.preference.PreferenceManager;
import android.support.annotation.DrawableRes;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import com.google.android.exoplayer2.ExoPlaybackException;
import com.google.android.exoplayer2.ExoPlayer;
import com.google.android.exoplayer2.ExoPlayerFactory;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.source.MediaSource;
import com.google.android.exoplayer2.trackselection.DefaultTrackSelector;
import com.google.android.exoplayer2.ui.DefaultTimeBar;
import com.google.android.exoplayer2.ui.PlayerView;
import com.google.android.exoplayer2.ui.TimeBar;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;

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

	private boolean mReleased;

	public ExoPlayerWrapperView(
			@NonNull final Context context,
			@NonNull final MediaSource mediaSource,
			@NonNull final Listener listener,
			final int controlsMarginRightDp) {

		super(context);
		mListener = listener;

		final DefaultTrackSelector trackSelector = new DefaultTrackSelector();

		mVideoPlayer = ExoPlayerFactory.newSimpleInstance(context, trackSelector);

		PlayerView videoPlayerView = new PlayerView(context);

		addView(videoPlayerView);

		videoPlayerView.setPlayer(mVideoPlayer);
		videoPlayerView.requestFocus();

		mVideoPlayer.prepare(mediaSource);

		mVideoPlayer.setPlayWhenReady(true);
		videoPlayerView.setUseController(false);

		if(PrefsUtility.pref_behaviour_video_playback_controls(
				context,
				PreferenceManager.getDefaultSharedPreferences(context))) {

			mControlView = new RelativeLayout(context);
			addView(mControlView);

			final LinearLayout controlBar = new LinearLayout(context);
			mControlView.addView(controlBar);
			controlBar.setBackgroundColor(Color.argb(127, 127, 127, 127));
			controlBar.setOrientation(LinearLayout.VERTICAL);

			{
				final RelativeLayout.LayoutParams controlBarLayoutParams
						= (RelativeLayout.LayoutParams) controlBar.getLayoutParams();
				controlBarLayoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
				controlBarLayoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
				controlBarLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
				controlBarLayoutParams.rightMargin = General.dpToPixels(context, controlsMarginRightDp);
			}

			final LinearLayout buttons = new LinearLayout(context);
			controlBar.addView(buttons);
			buttons.setOrientation(LinearLayout.HORIZONTAL);

			{
				final LinearLayout.LayoutParams buttonsLayoutParams
						= (LinearLayout.LayoutParams) buttons.getLayoutParams();
				buttonsLayoutParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
				buttonsLayoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
			}

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_previous,
					new OnClickListener() {
						@Override
						public void onClick(View view) {
							mVideoPlayer.seekTo(0);
							updateProgress();
						}
					}), buttons);

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_rewind,
					new OnClickListener() {
						@Override
						public void onClick(View view) {
							mVideoPlayer.seekTo(mVideoPlayer.getCurrentPosition() - 3000);
							updateProgress();
						}
					}), buttons);

			final AtomicReference<ImageButton> playButton = new AtomicReference<>();

			playButton.set(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_pause,
					new OnClickListener() {
						@Override
						public void onClick(View view) {
							mVideoPlayer.setPlayWhenReady(!mVideoPlayer.getPlayWhenReady());

							if(mVideoPlayer.getPlayWhenReady()) {
								playButton.get().setImageResource(R.drawable.exo_controls_pause);
							} else {
								playButton.get().setImageResource(R.drawable.exo_controls_play);
							}

							updateProgress();
						}
					}));

			addButton(playButton.get(), buttons);

			addButton(createButton(
					context,
					mControlView,
					R.drawable.exo_controls_fastforward,
					new OnClickListener() {
						@Override
						public void onClick(View view) {
							mVideoPlayer.seekTo(mVideoPlayer.getCurrentPosition() + 3000);
							updateProgress();
						}
					}), buttons);

			mTimeBarView = new DefaultTimeBar(context, null);
			controlBar.addView(mTimeBarView);

			{
				final LinearLayout.LayoutParams seekBarLayoutParams
						= (LinearLayout.LayoutParams) mTimeBarView.getLayoutParams();

				final int marginPx = General.dpToPixels(context, 8);

				seekBarLayoutParams.setMargins(marginPx, marginPx, marginPx, marginPx);
			}

			mTimeBarView.addListener(new TimeBar.OnScrubListener() {
				@Override
				public void onScrubStart(TimeBar timeBar, long position) {

				}

				@Override
				public void onScrubMove(TimeBar timeBar, long position) {
					mVideoPlayer.seekTo(position);
				}

				@Override
				public void onScrubStop(TimeBar timeBar, long position, boolean canceled) {

				}
			});

			final Runnable updateProgressRunnable = new Runnable() {
				@Override
				public void run() {
					updateProgress();

					if(!mReleased) {
						AndroidCommon.UI_THREAD_HANDLER.postDelayed(this, 250);
					}
				}
			};

			updateProgressRunnable.run();

			mControlView.setVisibility(GONE);

		} else {
			mControlView = null;
			mTimeBarView = null;
		}

		videoPlayerView.setLayoutParams(new FrameLayout.LayoutParams(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.MATCH_PARENT));

		mVideoPlayer.addListener(new Player.EventListener() {
			@Override
			public void onPlayerStateChanged(final boolean playWhenReady, final int playbackState) {

				// Loop
				if(playbackState == Player.STATE_ENDED) {
					mVideoPlayer.seekTo(0);
				}

				updateProgress();
			}

			@Override
			public void onPlayerError(final ExoPlaybackException error) {

				Log.e(TAG, "ExoPlayer error", error);
				mListener.onError();
			}

			@Override
			public void onSeekProcessed() {
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
			@NonNull final OnClickListener clickListener) {

		final ImageButton ib = (ImageButton)LayoutInflater.from(context).inflate(R.layout.flat_image_button, root, false);

		final int buttonPadding = General.dpToPixels(context, 14);
		ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding);

		ib.setImageResource(image);

		ib.setOnClickListener(clickListener);

		return ib;
	}

	private static void addButton(final ImageButton button, final LinearLayout layout) {

		layout.addView(button);

		final LinearLayout.LayoutParams layoutParams = (LinearLayout.LayoutParams) button.getLayoutParams();

		layoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
		layoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
	}

	public void updateProgress() {

		if(mTimeBarView != null && !mReleased) {

			final long duration = mVideoPlayer.getDuration();

			if(duration > 0) {
				mTimeBarView.setDuration(duration);
				mTimeBarView.setPosition(mVideoPlayer.getCurrentPosition());
				mTimeBarView.setBufferedPosition(mVideoPlayer.getBufferedPosition());
			} else {
				mTimeBarView.setDuration(0);
				mTimeBarView.setPosition(0);
				mTimeBarView.setBufferedPosition(0);
			}
		}
	}
}
