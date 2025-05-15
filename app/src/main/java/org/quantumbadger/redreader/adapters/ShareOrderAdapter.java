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
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.appcompat.app.AppCompatDialogFragment;
import org.quantumbadger.redreader.R;

import java.util.List;

public class ShareOrderAdapter extends BaseAdapter {

	private final Context context;
	private final List<ResolveInfo> appList;
	private final PackageManager packageManager;
	private final AppCompatDialogFragment fragment;

	public ShareOrderAdapter(
			final Context context,
			final List<ResolveInfo> appList,
			final AppCompatDialogFragment fragment) {
		this.context = context;
		this.appList = appList;
		this.packageManager = context.getPackageManager();
		this.fragment = fragment;
	}

	@Override
	public int getCount() {
		return appList.size();
	}

	@Override
	public Object getItem(final int position) {
		return appList.get(position);
	}

	@Override
	public long getItemId(final int position) {
		return position;
	}

	@Override
	public View getView(final int position, final View convertView, final ViewGroup parent) {
		final LayoutInflater inflater = (LayoutInflater)context
				.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View rowView = null;
		if(inflater != null) {
			rowView = inflater.inflate(R.layout.list_item_share_dialog, parent, false);
			final TextView label = rowView.findViewById(R.id.list_item_share_dialog_text);
			label.setText(appList.get(position).loadLabel(packageManager).toString());
			final ImageView icon = rowView.findViewById(R.id.list_item_share_dialog_icon);
			icon.setImageDrawable(appList.get(position).loadIcon(packageManager));
			final View divider = rowView.findViewById(R.id.list_item_share_dialog_divider);
			divider.setVisibility(View.INVISIBLE);

			rowView.setOnClickListener(v -> {
				((ShareOrderCallbackListener)fragment).onSelectedIntent(position);
				fragment.dismiss();
			});
		}

		return rowView;
	}
}
