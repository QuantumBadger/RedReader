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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.views.list.ListItemView;

import java.util.ArrayList;

public class AccountListAdapter extends BaseAdapter {

	private final ArrayList<RedditAccount> accounts;

	private final Drawable rrIconAdd;

	public AccountListAdapter(final Context context) {
		accounts = RedditAccountManager.getInstance(context).getAccounts();

		final TypedArray attr = context.obtainStyledAttributes(new int[] {R.attr.rrIconAdd});
		rrIconAdd = context.getResources().getDrawable(attr.getResourceId(0, 0));
	}

	public int getCount() {
		return accounts.size() + 1;
	}

	public RedditAccount getItem(final int i) {
		if(i == 0) return null;
		return accounts.get(i - 1);
	}

	public long getItemId(final int i) {
		return i;
	}

	@Override
	public int getItemViewType(final int position) {
		return 0;
	}

	@Override
	public int getViewTypeCount() {
		return 1;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	public View getView(final int i, View convertView, final ViewGroup viewGroup) {

		final Context context = viewGroup.getContext();

		if(convertView == null) {
			convertView = new ListItemView(context);
		}

		if(i == 0) {
			((ListItemView)convertView).reset(rrIconAdd, context.getString(R.string.accounts_add), true);

		} else {

			final RedditAccount account = accounts.get(i - 1);
			final BetterSSB username = new BetterSSB();

			if(account.isAnonymous()) {
				username.append(context.getString(R.string.accounts_anon), 0);
			} else {
				username.append(account.username, 0);
			}

			if(account.equals(RedditAccountManager.getInstance(context).getDefaultAccount())) {

				final TypedArray attr = context.obtainStyledAttributes(new int[] {R.attr.rrListSubtitleCol});
				final int col = attr.getColor(0, 0);

				username.append("  (" + context.getString(R.string.accounts_active) + ")", BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE, col, 0, 0.8f);
			}

			((ListItemView)convertView).reset(null, username.get(), true);
		}

		return convertView;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}
}