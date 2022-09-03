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
import android.content.Intent;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.TooltipCompat;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.PostSubmitActivity;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.reddit.SubredditDetails;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;

import java.util.Objects;

public class SubredditToolbar extends LinearLayout implements
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener,
		SharedPrefsWrapper.OnSharedPreferenceChangeListener {

	@NonNull private final Context mContext;

	// Field can't be local because the listener gets put in a weak map, and we want to stop it
	// being garbage collected.
	@Nullable private RedditSubredditSubscriptionManager.ListenerContext
			mSubscriptionListenerContext;

	private Runnable mRunnableOnAttach;
	private Runnable mRunnableOnDetach;
	private Runnable mRunnableOnSubscriptionsChange;
	private Runnable mRunnableOnPinnedChange;

	@NonNull private Optional<SubredditDetails> mSubredditDetails = Optional.empty();
	@NonNull private Optional<String> mUrl = Optional.empty();

	private ImageButton mButtonInfo;

	public void bindSubreddit(
			@NonNull final SubredditDetails subreddit,
			@NonNull final Optional<String> url) {

		mSubredditDetails = Optional.of(subreddit);
		mUrl = url;

		if(subreddit.hasSidebar()) {
			mButtonInfo.setVisibility(VISIBLE);
		} else {
			mButtonInfo.setVisibility(GONE);
		}

		mRunnableOnSubscriptionsChange.run();
		mRunnableOnPinnedChange.run();
	}

	public SubredditToolbar(final Context context) {
		this(context, null);
	}

	public SubredditToolbar(
			final Context context,
			@Nullable final AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public SubredditToolbar(
			final Context context,
			@Nullable final AttributeSet attrs,
			final int defStyleAttr) {

		super(context, attrs, defStyleAttr);

		mContext = context;
	}

	@Override
	protected void onFinishInflate() {
		super.onFinishInflate();

		final AppCompatActivity activity = (AppCompatActivity)mContext;

		final SharedPrefsWrapper sharedPreferences
				= General.getSharedPrefs(mContext);

		final RedditAccount currentUser =
				RedditAccountManager.getInstance(mContext).getDefaultAccount();

		final ImageButton buttonSubscribe = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_subscribe));
		final ImageButton buttonUnsubscribe = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_unsubscribe));
		final FrameLayout buttonSubscribeLoading = Objects.requireNonNull(
				(FrameLayout)findViewById(R.id.subreddit_toolbar_button_subscribe_loading));

		final ImageButton buttonPin = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_pin));
		final ImageButton buttonUnpin = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_unpin));

		final ImageButton buttonSubmit = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_submit));
		final ImageButton buttonShare = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_share));
		mButtonInfo = Objects.requireNonNull(
				(ImageButton)findViewById(R.id.subreddit_toolbar_button_info));

		for(int i = 0; i < getChildCount(); i++) {
			final View button = getChildAt(i);
			TooltipCompat.setTooltipText(button, button.getContentDescription());
		}

		buttonSubscribeLoading.addView(new ButtonLoadingSpinnerView(mContext));

		final RedditSubredditSubscriptionManager subscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(
						mContext,
						currentUser);

		mRunnableOnSubscriptionsChange = () -> {

			final SubredditSubscriptionState
					subscriptionState = subscriptionManager.getSubscriptionState(
					mSubredditDetails.get().id);

			if(subscriptionState == SubredditSubscriptionState.SUBSCRIBED) {

				buttonSubscribe.setVisibility(GONE);
				buttonUnsubscribe.setVisibility(VISIBLE);
				buttonSubscribeLoading.setVisibility(GONE);

			} else if(subscriptionState == SubredditSubscriptionState.NOT_SUBSCRIBED) {

				buttonSubscribe.setVisibility(VISIBLE);
				buttonUnsubscribe.setVisibility(GONE);
				buttonSubscribeLoading.setVisibility(GONE);

			} else {
				buttonSubscribe.setVisibility(GONE);
				buttonUnsubscribe.setVisibility(GONE);
				buttonSubscribeLoading.setVisibility(VISIBLE);
			}
		};

		mRunnableOnPinnedChange = () -> {

			final boolean pinned = PrefsUtility.pref_pinned_subreddits_check(
					mSubredditDetails.get().id);

			if(pinned) {
				buttonPin.setVisibility(GONE);
				buttonUnpin.setVisibility(VISIBLE);
			} else {
				buttonPin.setVisibility(VISIBLE);
				buttonUnpin.setVisibility(GONE);
			}
		};

		mRunnableOnAttach = () -> {
			mSubscriptionListenerContext = subscriptionManager.addListener(this);
			sharedPreferences.registerOnSharedPreferenceChangeListener(this);

			mRunnableOnSubscriptionsChange.run();
			mRunnableOnPinnedChange.run();
		};

		mRunnableOnDetach = () -> {
			mSubscriptionListenerContext.removeListener();
			mSubscriptionListenerContext = null;

			sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
		};

		if(currentUser.isAnonymous()) {

			final OnClickListener mustBeLoggedInListener
					= v -> General.showMustBeLoggedInDialog(activity);

			buttonSubscribe.setOnClickListener(mustBeLoggedInListener);
			buttonUnsubscribe.setOnClickListener(mustBeLoggedInListener);
			buttonSubmit.setOnClickListener(mustBeLoggedInListener);

		} else {
			buttonSubscribe.setOnClickListener(v -> subscriptionManager.subscribe(
					mSubredditDetails.get().id,
					activity));

			buttonUnsubscribe.setOnClickListener(v -> subscriptionManager.unsubscribe(
					mSubredditDetails.get().id,
					activity));

			buttonSubmit.setOnClickListener(v -> {
				final Intent intent = new Intent(
						activity,
						PostSubmitActivity.class);
				intent.putExtra("subreddit", mSubredditDetails.get().id.toString());
				activity.startActivity(intent);
			});
		}

		buttonPin.setOnClickListener(v -> PrefsUtility.pref_pinned_subreddits_add(
				mContext,
				mSubredditDetails.get().id));

		buttonUnpin.setOnClickListener(v -> PrefsUtility.pref_pinned_subreddits_remove(
				mContext,
				mSubredditDetails.get().id));

		buttonShare.setOnClickListener(v -> LinkHandler.shareText(
				activity,
				mSubredditDetails.get().id.toString(),
				mUrl.orElse(mSubredditDetails.get().url)));

		mButtonInfo.setOnClickListener(
				v -> mSubredditDetails.get().showSidebarActivity(activity));
	}

	@Override
	protected void onAttachedToWindow() {
		super.onAttachedToWindow();

		if(mRunnableOnAttach != null) {
			mRunnableOnAttach.run();
		}
	}

	@Override
	protected void onDetachedFromWindow() {
		super.onDetachedFromWindow();

		if(mRunnableOnDetach != null) {
			mRunnableOnDetach.run();
		}
	}

	@Override
	public void onSubredditSubscriptionListUpdated(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {

		if(mRunnableOnSubscriptionsChange != null) {
			AndroidCommon.UI_THREAD_HANDLER.post(mRunnableOnSubscriptionsChange);
		}
	}

	@Override
	public void onSubredditSubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {

		if(mRunnableOnSubscriptionsChange != null) {
			AndroidCommon.UI_THREAD_HANDLER.post(mRunnableOnSubscriptionsChange);
		}
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {

		if(mRunnableOnSubscriptionsChange != null) {
			AndroidCommon.UI_THREAD_HANDLER.post(mRunnableOnSubscriptionsChange);
		}
	}

	@Override
	public void onSharedPreferenceChanged(
			@NonNull final SharedPrefsWrapper sharedPreferences,
			@NonNull final String key) {

		if(mRunnableOnPinnedChange != null
				&& key.equals(mContext.getString(R.string.pref_pinned_subreddits_key))) {
			mRunnableOnPinnedChange.run();
		}
	}
}
