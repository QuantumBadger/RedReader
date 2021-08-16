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

package org.quantumbadger.redreader.fragments;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.os.Bundle;
import android.widget.ListView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDialogFragment;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.adapters.ShareOrderAdapter;
import org.quantumbadger.redreader.adapters.ShareOrderCallbackListener;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.StringUtils;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;


public class ShareOrderDialog extends AppCompatDialogFragment
		implements ShareOrderCallbackListener {
	private static final int amountOfPrioritizedApps = 3;
	private PackageManager packageManager;
	private Intent shareIntent;
	private List<ResolveInfo> orderedAppList;
	private Context context;

	public static ShareOrderDialog newInstance(final Intent shareIntent) {
		final ShareOrderDialog dialog = new ShareOrderDialog();

		final Bundle args = new Bundle(1);
		args.putParcelable("intent", shareIntent);
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		context = getContext();
		packageManager = getActivity().getPackageManager();
		shareIntent = getArguments().getParcelable("intent");
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		super.onCreateDialog(savedInstanceState);

		orderedAppList = prioritizeTopApps(packageManager.queryIntentActivities(
				shareIntent,
				0));

		final AlertDialog.Builder builder = new AlertDialog.Builder(context);
		builder.setTitle(context.getString(
				R.string.pref_behaviour_sharing_share_dialog_dialogtitle));
		final ListView listView = new ListView(context);
		builder.setView(listView);
		listView.setAdapter(new ShareOrderAdapter(context, orderedAppList, this));

		return builder.create();
	}

	private List<ResolveInfo> prioritizeTopApps(final List<ResolveInfo> unorderedList) {
		if(unorderedList.isEmpty()) {
			General.quickToast(context, R.string.error_toast_no_share_app_installed);
			dismiss();
		}

		// Make a copy of the list since the original is not modifiable
		final LinkedList<ResolveInfo> orderedList = new LinkedList<>(unorderedList);

		final List<String> prioritizedAppNames
				= Arrays.asList(PrefsUtility.pref_behaviour_sharing_dialog_data_get()
				.split(";"));
		final ResolveInfo[] prioritizedApps = new ResolveInfo[prioritizedAppNames.size()];

		// get the ResolveInfos for the available prioritized Apps and save them in order
		int count = 0;
		final Iterator<ResolveInfo> iterator = orderedList.iterator();
		while(iterator.hasNext()) {
			final ResolveInfo currentApp = iterator.next();
			final String currentAppName = currentApp.activityInfo.name;
			if(prioritizedAppNames.contains(currentAppName)) {
				prioritizedApps[prioritizedAppNames.indexOf(currentAppName)] = currentApp;
				iterator.remove();
				// Exit early if all apps matched
				if(++count >= prioritizedAppNames.size()) {
					break;
				}
			}
		}

		// Combine the two lists in order, respecting unavailable apps (null values in the Array)
		for(int i = prioritizedApps.length - 1; i >= 0; i--) {
			if(prioritizedApps[i] != null) {
				orderedList.addFirst(prioritizedApps[i]);
			}
		}

		return orderedList;
	}

	@Override
	public void onSelectedIntent(final int position) {
		final ActivityInfo info = orderedAppList.get(position).activityInfo;
		persistPriority(info);
		shareIntent.addCategory(Intent.CATEGORY_LAUNCHER);
		shareIntent.setClassName(info.applicationInfo.packageName, info.name);
		shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK
				| Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);
		startActivity(shareIntent);
	}

	private void persistPriority(final ActivityInfo selectedApplication) {
		final LinkedList<String> priorityAppList =
				new LinkedList<>(Arrays.asList(PrefsUtility.pref_behaviour_sharing_dialog_data_get()
						.split(";")));
		priorityAppList.remove(selectedApplication.name);
		priorityAppList.add(0, selectedApplication.name);
		if(priorityAppList.size() > amountOfPrioritizedApps) {
			priorityAppList.removeLast();
		}

		PrefsUtility.pref_behaviour_sharing_dialog_data_set(
				context,
				StringUtils.join(priorityAppList, ";"));
	}
}
