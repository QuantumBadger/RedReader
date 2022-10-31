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

package org.saiditnet.redreader.views;

import android.graphics.Color;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRThemeAttributes;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableInboxItem;

public class RedditInboxItemView extends LinearLayout {

	private final View mDivider;
	private final TextView mHeader;
	private final FrameLayout mBodyHolder;

	private final RRThemeAttributes mTheme;

	private final boolean showLinkButtons;

	private RedditRenderableInboxItem currentItem = null;

	private final AppCompatActivity mActivity;

	public RedditInboxItemView(
			final AppCompatActivity activity,
			final RRThemeAttributes theme) {

		super(activity);

		mActivity = activity;
		mTheme = theme;

		setOrientation(VERTICAL);

		mDivider = new View(activity);
		mDivider.setBackgroundColor(Color.argb(128, 128, 128, 128)); // TODO better
		addView(mDivider);

		mDivider.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		mDivider.getLayoutParams().height = 1;

		final LinearLayout inner = new LinearLayout(activity);
		inner.setOrientation(VERTICAL);

		mHeader = new TextView(activity);
		mHeader.setTextSize(11.0f * theme.rrCommentFontScale);
		mHeader.setTextColor(theme.rrCommentHeaderCol);
		inner.addView(mHeader);
		mHeader.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		mBodyHolder = new FrameLayout(activity);
		mBodyHolder.setPadding(0, General.dpToPixels(activity, 2), 0, 0);
		inner.addView(mBodyHolder);
		mBodyHolder.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final int paddingPixels = General.dpToPixels(activity, 8.0f);
		inner.setPadding(paddingPixels + paddingPixels, paddingPixels, paddingPixels, paddingPixels);

		addView(inner);
		inner.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		showLinkButtons = PrefsUtility.pref_appearance_linkbuttons(activity, PreferenceManager.getDefaultSharedPreferences(activity));

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				handleInboxClick(mActivity);
			}
		});

		setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				handleInboxLongClick(mActivity);
				return true;
			}
		});
	}

	public void reset(
			final AppCompatActivity context,
			final RedditChangeDataManager changeDataManager,
			final RRThemeAttributes theme,
			final RedditRenderableInboxItem item,
			final boolean showDividerAtTop) {

		currentItem = item;

		mDivider.setVisibility(showDividerAtTop ? VISIBLE : GONE);
		mHeader.setText(item.getHeader(theme, changeDataManager, context));

		final View body = item.getBody(
			context,
			mTheme.rrCommentBodyCol,
			13.0f * mTheme.rrCommentFontScale,
			showLinkButtons);

		mBodyHolder.removeAllViews();
		mBodyHolder.addView(body);
		body.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	public void handleInboxClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxClick(activity);
	}

	public void handleInboxLongClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxLongClick(activity);
	}
}
