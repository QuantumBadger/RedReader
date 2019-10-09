package org.quantumbadger.redreader.fragments;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatDialogFragment;
import android.widget.ListView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.adapters.ShareOrderAdapter;
import org.quantumbadger.redreader.adapters.ShareOrderCallbackListener;

import java.util.LinkedList;
import java.util.List;


public class ShareOrderDialog extends AppCompatDialogFragment implements ShareOrderCallbackListener {
	private PackageManager packageManager;
	private Intent shareIntent;
	private List<ResolveInfo> orderedAppList;

	public static ShareOrderDialog newInstance(final Intent shareIntent) {
		final ShareOrderDialog dialog = new ShareOrderDialog();

		final Bundle args = new Bundle(1);
		args.putParcelable("intent", shareIntent);
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState){
		super.onCreate(savedInstanceState);

		packageManager = getActivity().getPackageManager();
		shareIntent = getArguments().getParcelable("intent");
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState){
		super.onCreateDialog(savedInstanceState);

		List<ResolveInfo> appList = packageManager.queryIntentActivities(shareIntent, 0);
		orderedAppList = prioritizeTopApps(appList);

		final Context context = this.getContext();

		final AlertDialog.Builder builder = new AlertDialog.Builder(context);
		builder.setTitle(context.getString(R.string.share_dialog_title));

		ListView listView = new ListView(context);
		builder.setView(listView);

		listView.setAdapter(new ShareOrderAdapter(context, orderedAppList, this));

		return builder.create();
	}

	private List<ResolveInfo> prioritizeTopApps(List<ResolveInfo> unorderedList){
		List<ResolveInfo> orderedList = new LinkedList<ResolveInfo>(unorderedList);

		//TODO: implement preference for Order and ordering

		return orderedList;
	}


	@Override
	public void onSelectedIntent(int position) {
		ActivityInfo info = orderedAppList.get(position).activityInfo;
		shareIntent.addCategory(Intent.CATEGORY_LAUNCHER);
		shareIntent.setClassName(info.applicationInfo.packageName, info.name);
		shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);
		startActivity(shareIntent);
	}
}
