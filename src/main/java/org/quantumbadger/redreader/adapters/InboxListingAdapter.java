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

package org.quantumbadger.redreader.adapters;

import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManagerVolatile;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.quantumbadger.redreader.views.RedditInboxItemView;

import java.util.ArrayList;

public final class InboxListingAdapter extends BaseAdapter {

	private final ArrayList<RedditRenderableInboxItem> items = new ArrayList<>(128);

	private final AppCompatActivity mParentActivity;
	private final RRThemeAttributes mTheme;

	private final RedditChangeDataManagerVolatile mChangeDataManager;

	public InboxListingAdapter(
			final AppCompatActivity parentActivity,
			final RRThemeAttributes theme) {

		mParentActivity = parentActivity;
		mTheme = theme;

		mChangeDataManager = RedditChangeDataManagerVolatile.getInstance(
				RedditAccountManager.getInstance(parentActivity).getDefaultAccount());
	}

	public int getCount() {
		return items.size();
	}

	public RedditRenderableInboxItem getItem(final int i) {
		return items.get(i);
	}

	public long getItemId(int position) {
		return position;
	}

	@Override
	public int getViewTypeCount() {
		return 1;
	}

	@Override
	public int getItemViewType(final int position) {
		return 0;
	}

	public View getView(final int i, View convertView, final ViewGroup viewGroup) {

		if(convertView == null) {
			convertView = new RedditInboxItemView(viewGroup.getContext(), mTheme);
		}

		((RedditInboxItemView)convertView).reset(
				mParentActivity,
				mChangeDataManager,
				mTheme,
				items.get(i));

		return convertView;
	}

	public void addItem(final RedditRenderableInboxItem comment) {
		items.add(comment);
		notifyDataSetChanged();
	}
}