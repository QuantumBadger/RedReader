package org.quantumbadger.redreader.adapters;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheEntry;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.views.list.ListItemView;

import java.net.URI;
import java.util.ArrayList;
import java.util.UUID;

public class SessionListAdapter extends BaseAdapter {

	private final ArrayList<CacheEntry> sessions;
	private final UUID current;

	private final Drawable rrIconRefresh;

	public SessionListAdapter(final Context context, final URI url, final UUID current) {

		this.current = current;

		sessions = new ArrayList<CacheEntry>(
				CacheManager.getInstance(context).getSessions(url, RedditAccountManager.getInstance(context).getDefaultAccount()));

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrIconRefresh,
		});

		rrIconRefresh = context.getResources().getDrawable(attr.getResourceId(0, 0));
	}

	public int getCount() {
		return sessions.size() + 1;
	}

	public CacheEntry getItem(final int i) {
		if(i == 0) return null;
		return sessions.get(i - 1);
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
			((ListItemView)convertView).reset(rrIconRefresh, context.getString(R.string.options_refresh), true);
		} else {

			final CacheEntry session = sessions.get(i - 1);
			final BetterSSB name = new BetterSSB();

			if(RRTime.utcCurrentTimeMillis() - session.timestamp < 1000 * 120) {
				name.append(RRTime.formatDurationMsAgo(context, RRTime.utcCurrentTimeMillis() - session.timestamp), 0);
			} else {
				name.append(RRTime.formatDateTime(session.timestamp, context), 0);
			}

			if(session.session.equals(current)) {

				final TypedArray attr = context.obtainStyledAttributes(new int[] {R.attr.rrListSubtitleCol});
				final int col = attr.getColor(0, 0);

				name.append("  (" + context.getString(R.string.session_active) + ")", BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE, col, 0, 0.8f);
			}

			((ListItemView)convertView).reset(null, name.get(), true);
		}

		return convertView;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}
}