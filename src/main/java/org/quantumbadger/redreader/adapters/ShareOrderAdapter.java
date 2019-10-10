package org.quantumbadger.redreader.adapters;

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.support.v7.app.AppCompatDialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import org.quantumbadger.redreader.R;

import java.util.List;

public class ShareOrderAdapter extends BaseAdapter {

	private final Context context;
	private final List<ResolveInfo> appList;
	private final PackageManager packageManager;
	private final AppCompatDialogFragment fragment;

	public ShareOrderAdapter(Context context, List<ResolveInfo> appList, AppCompatDialogFragment fragment) {
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
	public Object getItem(int position) {
		return appList.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		LayoutInflater inflater = (LayoutInflater) context
				.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View rowView = null;
		if(inflater != null) {
			rowView = inflater.inflate(R.layout.list_item, parent, false);
			TextView label = rowView.findViewById(R.id.list_item_text);
			label.setText(appList.get(position).loadLabel(packageManager).toString());
			ImageView icon = rowView.findViewById(R.id.list_item_icon);
			icon.setImageDrawable(appList.get(position).loadIcon(packageManager));
			View divider = rowView.findViewById(R.id.list_item_divider);
			divider.setVisibility(View.INVISIBLE);

			rowView.setOnClickListener(new View.OnClickListener(){
				@Override
				public void onClick(View v) {
					((ShareOrderCallbackListener) fragment).onSelectedIntent(position);
					fragment.dismiss();
				}
			});
		}

		return rowView;
	}
}
