package org.quantumbadger.redreader.ui.settings;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.settings.RRPreference;
import org.quantumbadger.redreader.ui.RRFragmentContext;

public class PrefAdapter extends BaseAdapter implements RRPreference.Listener {

	private final RRPreference preference;
	private final RRPreference.Item[] values;
	private final RRFragmentContext context;

	public PrefAdapter(final RRFragmentContext context, final RRPreference preference) {

		this.values = preference.getItems();
		this.preference = preference;
		this.context = context;

		preference.addListener(this);
	}

	public boolean areAllItemsEnabled() {
		return true;
	}

	public int getCount() {
		return values.length;
	}

	public Object getItem(int position) {
		return values[position];
	}

	public long getItemId(int position) {
		return position;
	}

	public boolean hasStableIds() {
		return true;
	}

	public View getView(final int position, View convertView, final ViewGroup parent) {

		if(convertView == null) {
			convertView = new PreferenceView(context);
		}

		((PreferenceView)convertView).reset(preference, values[position], position == 0);

		return convertView;
	}

	public int getItemViewType(int position) {
		return 0;
	}

	public int getViewTypeCount() {
		return 1;
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
