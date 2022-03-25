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

import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;

public class RedditInboxItemView extends LinearLayout {

	private final View mDivider;
	private final TextView mHeader;
	private final FrameLayout mBodyHolder;

	private final RRThemeAttributes mTheme;

	private final boolean showLinkButtons;

	private RedditRenderableInboxItem currentItem = null;

	private final BaseActivity mActivity;

	public RedditInboxItemView(
			final BaseActivity activity,
			final RRThemeAttributes theme) {

		super(activity);

		mActivity = activity;
		mTheme = theme;

		setOrientation(VERTICAL);

		mDivider = new View(activity);
		mDivider.setBackgroundColor(Color.argb(128, 128, 128, 128)); // TODO better
		addView(mDivider);
		General.setLayoutWidthHeight(mDivider, ViewGroup.LayoutParams.MATCH_PARENT, 1);

		final LinearLayout inner = new LinearLayout(activity);
		inner.setOrientation(VERTICAL);

		mHeader = new TextView(activity);
		mHeader.setTextSize(11.0f * theme.rrCommentHeaderFontScale);
		mHeader.setTextColor(theme.rrCommentHeaderCol);
		inner.addView(mHeader);
		General.setLayoutMatchWidthWrapHeight(mHeader);

		mBodyHolder = new FrameLayout(activity);
		mBodyHolder.setPadding(0, General.dpToPixels(activity, 2), 0, 0);
		inner.addView(mBodyHolder);
		General.setLayoutMatchWidthWrapHeight(mBodyHolder);

		final int paddingPixels = General.dpToPixels(activity, 8.0f);
		inner.setPadding(
				paddingPixels + paddingPixels,
				paddingPixels,
				paddingPixels,
				paddingPixels);

		addView(inner);
		General.setLayoutMatchWidthWrapHeight(mBodyHolder);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		showLinkButtons = PrefsUtility.pref_appearance_linkbuttons();

		setOnClickListener(v -> handleInboxClick(mActivity));

		setOnLongClickListener(v -> {
			handleInboxLongClick(mActivity);
			return true;
		});
	}

	public void reset(
			final BaseActivity context,
			final RedditChangeDataManager changeDataManager,
			final RRThemeAttributes theme,
			final RedditRenderableInboxItem item,
			final boolean showDividerAtTop) {

		currentItem = item;

		mDivider.setVisibility(showDividerAtTop ? VISIBLE : GONE);
		mHeader.setText(item.getHeader(
				theme,
				changeDataManager,
				context,
				PrefsUtility.appearance_inbox_age_units(),
				RedditRenderableComment.NO_TIMESTAMP,
				RedditRenderableComment.NO_TIMESTAMP));

		mHeader.setContentDescription(item.getAccessibilityHeader(
				theme,
				changeDataManager,
				context,
				PrefsUtility.appearance_inbox_age_units(),
				RedditRenderableComment.NO_TIMESTAMP,
				RedditRenderableComment.NO_TIMESTAMP,
				false,
				Optional.empty()));

		final View body = item.getBody(
				context,
				mTheme.rrCommentBodyCol,
				13.0f * mTheme.rrCommentFontScale,
				showLinkButtons);

		mBodyHolder.removeAllViews();
		mBodyHolder.addView(body);
		General.setLayoutMatchWidthWrapHeight(body);
	}

	public void handleInboxClick(final BaseActivity activity) {
		if(currentItem != null) {
			currentItem.handleInboxClick(activity);
		}
	}

	public void handleInboxLongClick(final BaseActivity activity) {
		if(currentItem != null) {
			currentItem.handleInboxLongClick(activity);
		}
	}
}
