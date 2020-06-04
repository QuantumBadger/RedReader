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
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Typeface;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.PostSubmitActivity;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.PostListingURL;


public final class PostListingHeader extends LinearLayout
		implements RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener,
		SharedPreferences.OnSharedPreferenceChangeListener {

	@NonNull private final Context mContext;

	// Field can't be local because the listener gets put in a weak map, and we want to stop it
	// being garbage collected.
	@Nullable private RedditSubredditSubscriptionManager.ListenerContext mSubscriptionListenerContext;

	@Nullable private final Runnable mRunnableOnAttach;
	@Nullable private final Runnable mRunnableOnDetach;
	@Nullable private final Runnable mRunnableOnSubscriptionsChange;
	@Nullable private final Runnable mRunnableOnPinnedChange;

	public PostListingHeader(
			final AppCompatActivity activity,
			final String titleText,
			final String subtitleText,
			final PostListingURL url,
			@Nullable final RedditSubreddit subreddit) {

		super(activity);

		mContext = activity.getApplicationContext();

		final float dpScale = activity.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final LinearLayout greyHeader = new LinearLayout(activity);
		greyHeader.setOrientation(LinearLayout.VERTICAL);

		{
			final TypedArray appearance = activity.obtainStyledAttributes(new int[]{
					R.attr.rrPostListHeaderBackgroundCol});

			greyHeader.setBackgroundColor(appearance.getColor(0, General.COLOR_INVALID));

			appearance.recycle();
		}

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		greyHeader.setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final Typeface tf = Typeface.createFromAsset(activity.getAssets(), "fonts/Roboto-Light.ttf");

		final TextView title = new TextView(activity);
		title.setText(titleText);
		title.setTextSize(22.0f);
		title.setTypeface(tf);
		title.setTextColor(Color.WHITE);
		greyHeader.addView(title);

		final TextView subtitle = new TextView(activity);
		subtitle.setTextSize(14.0f);
		subtitle.setText(subtitleText);
		subtitle.setTextColor(Color.rgb(200, 200, 200));
		greyHeader.addView(subtitle);

		addView(greyHeader);

		final RedditAccount currentUser = RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(subreddit != null) {

			final LinearLayout buttons = (LinearLayout)inflate(
					activity,
					R.layout.subreddit_header_toolbar,
					this);

			final ImageButton buttonSubscribe = buttons.findViewById(R.id.subreddit_toolbar_button_subscribe);
			final ImageButton buttonUnsubscribe = buttons.findViewById(R.id.subreddit_toolbar_button_unsubscribe);
			final FrameLayout buttonSubscribeLoading = buttons.findViewById(R.id.subreddit_toolbar_button_subscribe_loading);

			final ImageButton buttonPin = buttons.findViewById(R.id.subreddit_toolbar_button_pin);
			final ImageButton buttonUnpin = buttons.findViewById(R.id.subreddit_toolbar_button_unpin);

			final ImageButton buttonSubmit = buttons.findViewById(R.id.subreddit_toolbar_button_submit);
			final ImageButton buttonShare = buttons.findViewById(R.id.subreddit_toolbar_button_share);
			final ImageButton buttonInfo = buttons.findViewById(R.id.subreddit_toolbar_button_info);

			buttonSubscribeLoading.addView(new ButtonLoadingSpinnerView(activity));

			final RedditSubredditSubscriptionManager subscriptionManager
					= RedditSubredditSubscriptionManager.getSingleton(activity, currentUser);

			final SharedPreferences sharedPreferences
					= PreferenceManager.getDefaultSharedPreferences(activity);

			final SubredditCanonicalId subredditCanonicalId;

			try {
				subredditCanonicalId = subreddit.getCanonicalId();

			} catch(final InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}

			mRunnableOnSubscriptionsChange = () -> {

				final RedditSubredditSubscriptionManager.SubredditSubscriptionState subscriptionState
						= subscriptionManager.getSubscriptionState(subredditCanonicalId);

				if(subscriptionState
						== RedditSubredditSubscriptionManager.SubredditSubscriptionState.SUBSCRIBED) {

					buttonSubscribe.setVisibility(GONE);
					buttonUnsubscribe.setVisibility(VISIBLE);
					buttonSubscribeLoading.setVisibility(GONE);

				} else if(subscriptionState
						== RedditSubredditSubscriptionManager.SubredditSubscriptionState.NOT_SUBSCRIBED) {

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
						mContext,
						sharedPreferences,
						subredditCanonicalId);

				if(pinned) {
					buttonPin.setVisibility(GONE);
					buttonUnpin.setVisibility(VISIBLE);
				} else {
					buttonPin.setVisibility(VISIBLE);
					buttonUnpin.setVisibility(GONE);
				}
			};

			mRunnableOnSubscriptionsChange.run();
			mRunnableOnPinnedChange.run();

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

			if(!subreddit.hasSidebar()) {
				buttonInfo.setVisibility(GONE);
			}

			if(currentUser.isAnonymous()) {

				final OnClickListener mustBeLoggedInListener
						= v -> General.showMustBeLoggedInDialog(activity);

				buttonSubscribe.setOnClickListener(mustBeLoggedInListener);
				buttonUnsubscribe.setOnClickListener(mustBeLoggedInListener);
				buttonSubmit.setOnClickListener(mustBeLoggedInListener);

			} else {
				buttonSubscribe.setOnClickListener(v -> subscriptionManager.subscribe(
						subredditCanonicalId,
						activity));

				buttonUnsubscribe.setOnClickListener(v -> subscriptionManager.unsubscribe(
						subredditCanonicalId,
						activity));

				buttonSubmit.setOnClickListener(v -> {
							final Intent intent = new Intent(activity, PostSubmitActivity.class);
							intent.putExtra("subreddit", subredditCanonicalId.toString());
							activity.startActivity(intent);
				});
			}

			buttonPin.setOnClickListener(v -> PrefsUtility.pref_pinned_subreddits_add(
					mContext,
					sharedPreferences,
					subredditCanonicalId));

			buttonUnpin.setOnClickListener(v -> PrefsUtility.pref_pinned_subreddits_remove(
					mContext,
					sharedPreferences,
					subredditCanonicalId));

			buttonShare.setOnClickListener(v -> {
				LinkHandler.shareText(activity, subredditCanonicalId.toString(), url.browserUrl());
			});

			buttonInfo.setOnClickListener(v -> subreddit.showSidebarActivity(activity));

		} else {
			mSubscriptionListenerContext = null;
			mRunnableOnAttach = null;
			mRunnableOnDetach = null;
			mRunnableOnSubscriptionsChange = null;
			mRunnableOnPinnedChange = null;
		}
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
			final SharedPreferences sharedPreferences,
			final String key) {

		if(mRunnableOnPinnedChange != null
				&& key != null
				&& key.equals(mContext.getString(R.string.pref_pinned_subreddits_key))) {

			mRunnableOnPinnedChange.run();
		}
	}
}
