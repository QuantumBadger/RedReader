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

import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManagerVolatile;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;

public class RedditInboxItemView extends LinearLayout {

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final RRThemeAttributes mTheme;

	private final boolean showLinkButtons;

	private RedditRenderableInboxItem currentItem = null;

	private final AppCompatActivity mActivity;

	public RedditInboxItemView(final AppCompatActivity activity, final RRThemeAttributes theme) {

		super(activity);

		mActivity = activity;
		mTheme = theme;

		setOrientation(VERTICAL);

		header = new TextView(activity);
		header.setTextSize(11.0f * theme.rrCommentFontScale);
		header.setTextColor(theme.rrCommentHeaderCol);
		addView(header);
		header.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		bodyHolder = new FrameLayout(activity);
		bodyHolder.setPadding(0, General.dpToPixels(activity, 2), 0, 0);
		addView(bodyHolder);
		bodyHolder.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final int paddingPixels = General.dpToPixels(activity, 8.0f);
		setPadding(paddingPixels + paddingPixels, paddingPixels, paddingPixels, paddingPixels);

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
			final RedditChangeDataManagerVolatile changeDataManager,
			final RRThemeAttributes theme,
			final RedditRenderableInboxItem item) {

		currentItem = item;

		header.setText(item.getHeader(theme, changeDataManager, context));

		final View body = item.getBody(
			context,
			mTheme.rrCommentBodyCol,
			13.0f * mTheme.rrCommentFontScale,
			showLinkButtons);

		bodyHolder.removeAllViews();
		bodyHolder.addView(body);
		body.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	public void handleInboxClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxClick(activity);
	}

	public void handleInboxLongClick(AppCompatActivity activity) {
		if(currentItem != null) currentItem.handleInboxLongClick(activity);
	}
}
