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
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatDialogFragment;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.cache.CacheEntry;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.viewholders.VH1Text;

import java.net.URI;
import java.util.ArrayList;
import java.util.UUID;

public class SessionListAdapter extends HeaderRecyclerAdapter<RecyclerView.ViewHolder> {

	private final Context context;
	private final UUID current;
	private final SessionChangeListener.SessionChangeType type;
	private final AppCompatDialogFragment fragment;

	private final ArrayList<CacheEntry> sessions;
	private final Drawable rrIconRefresh;

	public SessionListAdapter(final Context context, final URI url, final UUID current, SessionChangeListener.SessionChangeType type, final AppCompatDialogFragment fragment) {
		this.context = context;
		this.current = current;
		this.type = type;
		this.fragment = fragment;

		sessions = new ArrayList<>(
			CacheManager.getInstance(context).getSessions(url, RedditAccountManager.getInstance(context).getDefaultAccount()));

		final TypedArray attr = context.obtainStyledAttributes(new int[]{R.attr.rrIconRefresh,});
		rrIconRefresh = ContextCompat.getDrawable(context, attr.getResourceId(0, 0));
		attr.recycle();
	}

	@Override
	protected RecyclerView.ViewHolder onCreateHeaderItemViewHolder(ViewGroup parent) {
		View v = LayoutInflater.from(parent.getContext())
			.inflate(R.layout.list_item_1_text, parent, false);
		return new VH1Text(v);
	}

	@Override
	protected RecyclerView.ViewHolder onCreateContentItemViewHolder(ViewGroup parent) {
		View v = LayoutInflater.from(parent.getContext())
			.inflate(R.layout.list_item_1_text, parent, false);
		return new VH1Text(v);
	}

	@Override
	protected void onBindHeaderItemViewHolder(RecyclerView.ViewHolder holder, final int position) {
		final VH1Text vh = (VH1Text) holder;
		vh.text.setText(context.getString(R.string.options_refresh));
		vh.text.setCompoundDrawablesWithIntrinsicBounds(rrIconRefresh, null, null, null);
		vh.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				((SessionChangeListener) context).onSessionRefreshSelected(type);
				fragment.dismiss();
			}
		});
	}

	@Override
	protected void onBindContentItemViewHolder(RecyclerView.ViewHolder holder, final int position) {
		final VH1Text vh = (VH1Text) holder;
		final CacheEntry session = sessions.get(position);
		final BetterSSB name = new BetterSSB();

		if (RRTime.utcCurrentTimeMillis() - session.timestamp < 1000 * 120) {
			name.append(RRTime.formatDurationFrom(context, session.timestamp), 0);
		} else {
			name.append(RRTime.formatDateTime(session.timestamp, context), 0);
		}

		if (session.session.equals(current)) {
			final TypedArray attr = context.obtainStyledAttributes(new int[]{R.attr.rrListSubtitleCol});
			final int col = attr.getColor(0, 0);
			attr.recycle();

			name.append("  (" + context.getString(R.string.session_active) + ")", BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE, col, 0, 0.8f);
		}

		vh.text.setText(name.get());

		vh.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final CacheEntry ce = sessions.get(position);
				((SessionChangeListener) context).onSessionSelected(ce.session, type);
				fragment.dismiss();
			}
		});
	}

	@Override
	protected int getContentItemCount() {
		return sessions.size();
	}
}
