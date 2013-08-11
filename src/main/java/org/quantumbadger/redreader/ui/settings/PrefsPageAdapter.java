package org.quantumbadger.redreader.ui.settings;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.settings.RRPreference;
import org.quantumbadger.redreader.settings.RRPreferenceBoolean;
import org.quantumbadger.redreader.settings.RRPreferenceHeader;
import org.quantumbadger.redreader.settings.RRPreferenceLink;
import org.quantumbadger.redreader.ui.RRFragmentContext;
import org.quantumbadger.redreader.views.list.ListSectionHeaderView;

import java.util.List;

public class PrefsPageAdapter extends BaseAdapter implements RRPreference.Listener {

	private final RRPreference[] prefsPage;
	private final RRFragmentContext context;

	private static final int PREF_HEADER = 0, PREF_BOOLEAN = 1, PREF_LINK = 2, PREF_LINK_SUBTITLE = 3;

	public PrefsPageAdapter(final RRFragmentContext context, final List<RRPreference> prefsPage) {

		this.prefsPage = prefsPage.toArray(new RRPreference[prefsPage.size()]);
		this.context = context;

		for(final RRPreference pref : prefsPage) {
			pref.addListener(this);
		}
	}

	public boolean areAllItemsEnabled() {
		return false;
	}

	public boolean isEnabled(int position) {
		return !prefsPage[position].isGreyedOut();
	}

	public int getCount() {
		return prefsPage.length;
	}

	public Object getItem(int position) {
		return prefsPage[position];
	}

	public long getItemId(int position) {
		return position;
	}

	public boolean hasStableIds() {
		return true;
	}

	public View getView(final int position, View convertView, final ViewGroup parent) {

		if(convertView == null) {
			convertView = generateViewOfType(position);
		}

		if(prefsPage[position] instanceof RRPreferenceHeader) {
			((ListSectionHeaderView)convertView).reset(context.activity.getString(prefsPage[position].titleString));

		} else {
			final boolean hideDivider = position == 0 || prefsPage[position - 1] instanceof RRPreferenceHeader;
			((PreferenceView)convertView).reset(prefsPage[position], hideDivider);
		}

		return convertView;
	}

	private View generateViewOfType(int position) {

		switch(getItemViewType(position)) {

			case PREF_HEADER:
				final ListSectionHeaderView headerView = new ListSectionHeaderView(context.activity);
				final int padding = General.dpToPixels(context.activity, 12);
				headerView.setPadding(padding, 0, padding, 0);
				return headerView;

			case PREF_BOOLEAN:
			case PREF_LINK:
			case PREF_LINK_SUBTITLE:
				return new PreferenceView(context);

			default:
				return null;
		}
	}

	public int getItemViewType(int position) {

		final RRPreference preference = prefsPage[position];

		if(preference instanceof RRPreferenceHeader) {
			return PREF_HEADER;
		} else if(preference instanceof RRPreferenceBoolean) {
			return PREF_BOOLEAN;
		} else if(preference instanceof RRPreferenceLink) {
			return PREF_LINK;
		} else {
			return PREF_LINK_SUBTITLE;
		}
	}

	public int getViewTypeCount() {
		return 4;
	}

	public boolean isEmpty() {
		return false;
	}

	public void onPreferenceChanged(RRPreference preference) {
		General.runOnUiThread(new Runnable() {
			public void run() {
				notifyDataSetChanged();
			}
		});
	}
}
