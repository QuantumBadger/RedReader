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
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;

import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManagerVolatile;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.quantumbadger.redreader.viewholders.VH;
import org.quantumbadger.redreader.views.RedditInboxItemView;

import java.util.ArrayList;

public final class InboxListingAdapter extends RecyclerView.Adapter<VH> {

	private final ArrayList<RedditRenderableInboxItem> items = new ArrayList<>(128);

	private final AppCompatActivity parentActivity;
	private final RRThemeAttributes theme;

	private final RedditChangeDataManagerVolatile changeDataManager;

	public InboxListingAdapter(
			final AppCompatActivity parentActivity,
			final RRThemeAttributes theme) {

		this.parentActivity = parentActivity;
		this.theme = theme;

		changeDataManager = RedditChangeDataManagerVolatile.getInstance(
				RedditAccountManager.getInstance(parentActivity).getDefaultAccount());
	}

	@Override
	public VH onCreateViewHolder(ViewGroup parent, int viewType) {
		return new VH(new RedditInboxItemView(parent.getContext(), theme));
	}

	@Override
	public void onBindViewHolder(final VH holder, final int position) {
		((RedditInboxItemView) holder.itemView).reset(
			parentActivity, changeDataManager, theme, items.get(position));

		holder.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final RedditRenderableInboxItem item = items.get(holder.getAdapterPosition());
				item.handleInboxClick(parentActivity);
			}
		});
	}

	@Override
	public int getItemCount() {
		return items.size();
	}

	public void addItem(final RedditRenderableInboxItem comment) {
		items.add(comment);
		notifyDataSetChanged();
	}
}
