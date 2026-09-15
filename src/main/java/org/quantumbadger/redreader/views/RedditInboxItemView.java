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

import android.animation.ArgbEvaluator;
import android.animation.ValueAnimator;
import android.content.res.TypedArray;
import android.transition.ChangeBounds;
import android.transition.TransitionManager;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.button.MaterialButton;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;

import java.util.Set;

/**
 * A card showing a single inbox item (a private message or a comment reply),
 * with buttons to reply, view the context, and mark the item as read/unread.
 */
public class RedditInboxItemView extends FrameLayout {

	private static final int COLLAPSED_MAX_HEIGHT_DP = 200;
	private static final int READ_STATUS_ANIM_DURATION_MS = 200;

	private final BaseActivity mActivity;
	private final RRThemeAttributes mTheme;
	private final boolean mShowLinkButtons;
	private final boolean mCollapseLongMessages;

	// False for the "sent" folder, where read status doesn't apply
	private final boolean mAllowMarkRead;

	// Shared with the activity, so that expanded state survives view recycling
	private final Set<RedditIdAndType> mExpandedItems;

	private final ViewGroup mCard;
	private final View mUnreadIndicator;
	private final TextView mHeader;
	private final MaxHeightFrameLayout mBodyHolder;
	private final FrameLayout mBody;
	private final View mBodyFade;
	private final MaterialButton mShowMoreButton;
	private final MaterialButton mReplyButton;
	private final MaterialButton mContextButton;
	private final MaterialButton mMarkReadButton;

	private final int mReadIndicatorCol;

	// Tracked so that colour changes can be animated from the current value
	private int mCurrentIndicatorCol;
	@Nullable private ValueAnimator mIndicatorColAnimator = null;

	@Nullable private RedditRenderableInboxItem mCurrentItem = null;
	@Nullable private RedditChangeDataManager mChangeDataManager = null;

	public RedditInboxItemView(
			final BaseActivity activity,
			final RRThemeAttributes theme,
			final boolean allowMarkRead,
			final Set<RedditIdAndType> expandedItems) {

		super(activity);

		mActivity = activity;
		mTheme = theme;
		mAllowMarkRead = allowMarkRead;
		mExpandedItems = expandedItems;

		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons();
		mCollapseLongMessages = PrefsUtility.pref_appearance_inbox_collapse_long_messages();

		inflate(activity, R.layout.inbox_item_card, this);

		mCard = findViewById(R.id.inbox_item_card);
		mUnreadIndicator = findViewById(R.id.inbox_item_unread_indicator);
		mHeader = findViewById(R.id.inbox_item_header);
		mBodyHolder = findViewById(R.id.inbox_item_body_holder);
		mBody = findViewById(R.id.inbox_item_body);
		mBodyFade = findViewById(R.id.inbox_item_body_fade);
		mShowMoreButton = findViewById(R.id.inbox_item_show_more);
		mReplyButton = findViewById(R.id.inbox_item_button_reply);
		mContextButton = findViewById(R.id.inbox_item_button_context);
		mMarkReadButton = findViewById(R.id.inbox_item_button_mark_read);

		{
			final TypedArray attrs = activity.obtainStyledAttributes(new int[] {
					R.attr.rrListDividerCol
			});

			mReadIndicatorCol = attrs.getColor(0, 0);

			attrs.recycle();
		}

		mCurrentIndicatorCol = mReadIndicatorCol;

		mHeader.setTextSize(11.0f * theme.rrCommentHeaderFontScale);
		mHeader.setTextColor(theme.rrCommentHeaderCol);

		mBodyHolder.setMaxHeightPx(General.dpToPixels(activity, COLLAPSED_MAX_HEIGHT_DP));
		mBodyHolder.setOverflowListener(contentExceedsMaxHeight -> updateCollapseUi());

		mCard.setOnClickListener(v -> handleInboxClick(mActivity));

		mCard.setOnLongClickListener(v -> {
			handleInboxLongClick(mActivity);
			return true;
		});

		mReplyButton.setOnClickListener(v -> {
			if(mCurrentItem != null && mChangeDataManager != null) {
				mCurrentItem.handleInboxReply(mActivity, mChangeDataManager);
			}
		});

		mContextButton.setOnClickListener(v -> {
			if(mCurrentItem != null) {
				mCurrentItem.handleInboxContext(mActivity);
			}
		});

		mMarkReadButton.setOnClickListener(v -> markAsRead());

		mShowMoreButton.setOnClickListener(v -> toggleExpanded());
	}

	public void reset(
			final BaseActivity context,
			final RedditChangeDataManager changeDataManager,
			final RRThemeAttributes theme,
			final RedditRenderableInboxItem item) {

		mCurrentItem = item;
		mChangeDataManager = changeDataManager;

		mHeader.setText(item.getHeader(
				theme,
				changeDataManager,
				context,
				PrefsUtility.appearance_inbox_age_units(),
				null,
				null).get());

		final View body = item.getBody(
				context,
				mTheme.rrCommentBodyCol,
				13.0f * mTheme.rrCommentFontScale,
				mShowLinkButtons);

		mBody.removeAllViews();
		mBody.addView(body);
		General.setLayoutMatchWidthWrapHeight(body);

		mReplyButton.setVisibility(item.canReply(context) ? VISIBLE : GONE);
		mContextButton.setVisibility(item.hasContext() ? VISIBLE : GONE);

		mBodyHolder.setCollapsed(
				mCollapseLongMessages && !mExpandedItems.contains(item.getIdAndType()));

		updateCollapseUi();
		updateReadStatusUi(false);
	}

	private void updateReadStatusUi(final boolean animate) {

		if(mCurrentItem == null || mChangeDataManager == null) {
			return;
		}

		final String accessibilityHeader = mCurrentItem.getAccessibilityHeader(
				mTheme,
				mChangeDataManager,
				mActivity,
				PrefsUtility.appearance_inbox_age_units(),
				null,
				null,
				false,
				Optional.empty());

		if(!mAllowMarkRead) {
			mUnreadIndicator.setVisibility(GONE);
			mMarkReadButton.setVisibility(GONE);
			mHeader.setContentDescription(accessibilityHeader);
			return;
		}

		final boolean read = mChangeDataManager.isRead(mCurrentItem.getIdAndType());

		mHeader.setContentDescription(accessibilityHeader
				+ mActivity.getString(read
						? R.string.accessibility_inbox_read
						: R.string.accessibility_inbox_unread));

		// The indicator is always present, so that marking an item as read
		// doesn't cause the card to be re-laid out
		mUnreadIndicator.setVisibility(VISIBLE);
		mMarkReadButton.setVisibility(VISIBLE);

		final int indicatorCol = read ? mReadIndicatorCol : mTheme.colorAccent;

		if(mIndicatorColAnimator != null) {
			mIndicatorColAnimator.cancel();
			mIndicatorColAnimator = null;
		}

		mMarkReadButton.animate().cancel();

		if(!animate) {
			setIndicatorCol(indicatorCol);
			mMarkReadButton.setAlpha(1f);
			applyReadButtonState(read);
			return;
		}

		final ValueAnimator colAnimator = ValueAnimator.ofObject(
				new ArgbEvaluator(),
				mCurrentIndicatorCol,
				indicatorCol);
		colAnimator.setDuration(READ_STATUS_ANIM_DURATION_MS);
		colAnimator.addUpdateListener(
				animation -> setIndicatorCol((Integer)animation.getAnimatedValue()));
		colAnimator.start();
		mIndicatorColAnimator = colAnimator;

		// Fade the button out, change it (animating the resulting movement of
		// the button row), then fade it back in
		mMarkReadButton.animate()
				.alpha(0f)
				.setDuration(READ_STATUS_ANIM_DURATION_MS / 2)
				.withEndAction(() -> {

					final ChangeBounds changeBounds = new ChangeBounds();
					changeBounds.setDuration(READ_STATUS_ANIM_DURATION_MS);
					TransitionManager.beginDelayedTransition(mCard, changeBounds);

					applyReadButtonState(read);

					mMarkReadButton.animate()
							.alpha(1f)
							.setDuration(READ_STATUS_ANIM_DURATION_MS / 2);
				});
	}

	private void setIndicatorCol(final int col) {
		mCurrentIndicatorCol = col;
		mUnreadIndicator.setBackgroundColor(col);
	}

	private void applyReadButtonState(final boolean read) {

		// Reddit no longer provides an API for marking items as unread, so
		// once read, the button becomes a greyed-out status indicator
		if(read) {
			mMarkReadButton.setText(R.string.inbox_button_read);
			mMarkReadButton.setEnabled(false);
		} else {
			mMarkReadButton.setText(R.string.action_mark_read);
			mMarkReadButton.setEnabled(true);
		}
	}

	private void updateCollapseUi() {

		if(!mCollapseLongMessages) {
			mBodyFade.setVisibility(GONE);
			mShowMoreButton.setVisibility(GONE);
			return;
		}

		final boolean overflows = mBodyHolder.contentExceedsMaxHeight();
		final boolean collapsed = mBodyHolder.isCollapsed();

		mBodyFade.setVisibility(overflows && collapsed ? VISIBLE : GONE);

		if(overflows) {
			mShowMoreButton.setVisibility(VISIBLE);
			mShowMoreButton.setText(collapsed
					? R.string.inbox_show_more
					: R.string.inbox_show_less);
		} else {
			mShowMoreButton.setVisibility(GONE);
		}
	}

	private void toggleExpanded() {

		if(mCurrentItem == null) {
			return;
		}

		final RedditIdAndType id = mCurrentItem.getIdAndType();
		final boolean expand = mBodyHolder.isCollapsed();

		if(expand) {
			mExpandedItems.add(id);
		} else {
			mExpandedItems.remove(id);
		}

		mBodyHolder.setCollapsed(!expand);
		updateCollapseUi();
	}

	private void markAsRead() {

		if(mCurrentItem == null || mChangeDataManager == null) {
			return;
		}

		final RedditChangeDataManager changeDataManager = mChangeDataManager;
		final RedditIdAndType id = mCurrentItem.getIdAndType();

		if(changeDataManager.isRead(id)) {
			return;
		}

		// Update locally straight away, and revert if the request fails
		changeDataManager.markRead(TimestampUTC.now(), id, true);
		updateReadStatusUi(true);

		RedditAPI.markMessageAsRead(
				CacheManager.getInstance(mActivity),
				new APIResponseHandler.ActionResponseHandler(mActivity) {
					@Override
					protected void onSuccess() {
						// The local state has already been updated
					}

					@Override
					protected void onFailure(@NonNull final RRError error) {

						// Revert the local change
						changeDataManager.markRead(TimestampUTC.now(), id, false);

						AndroidCommon.runOnUiThread(() -> {
							General.quickToast(mActivity, R.string.inbox_mark_read_failed);

							// This view may have been recycled in the meantime
							if(mCurrentItem != null && mCurrentItem.getIdAndType().equals(id)) {
								updateReadStatusUi(true);
							}
						});
					}

					@Override
					protected void onCallbackException(final Throwable t) {
						BugReportActivity.handleGlobalError(mActivity, t);
					}
				},
				RedditAccountManager.getInstance(mActivity).getDefaultAccount(),
				id,
				mActivity);
	}

	public void handleInboxClick(final BaseActivity activity) {
		if(mCurrentItem != null) {
			mCurrentItem.handleInboxClick(activity);
		}
	}

	public void handleInboxLongClick(final BaseActivity activity) {
		if(mCurrentItem != null) {
			mCurrentItem.handleInboxLongClick(activity);
		}
	}
}
