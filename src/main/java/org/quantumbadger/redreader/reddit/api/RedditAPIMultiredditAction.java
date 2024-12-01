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

package org.quantumbadger.redreader.reddit.api;

import android.content.Context;
import android.util.Log;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.EnumSet;

public class RedditAPIMultiredditAction {

	private static final String TAG = "MultiredditAction";

	public enum MultiredditAction {
		DELETE_MULTIREDDIT;
	}

	private static class RCVMenuItem {
		public final String title;
		public final MultiredditAction action;

		private RCVMenuItem(
				final Context context,
				final int titleRes,
				final MultiredditAction action) {

			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public static void showActionMenu(
			final AppCompatActivity activity,
			final String multiredditName) {

		final EnumSet<MultiredditAction> itemPref
				= PrefsUtility.pref_menus_multireddit_context_items();

		if(itemPref.isEmpty()) {
			return;
		}

		final RedditAccount user =
				RedditAccountManager.getInstance(activity).getDefaultAccount();

		if (user.isAnonymous()) {
			return;
		}

		final ArrayList<RCVMenuItem> menu = new ArrayList<>();

		if(itemPref.contains(MultiredditAction.DELETE_MULTIREDDIT)) {
			menu.add(new RCVMenuItem(
					activity,
					R.string.delete_multireddit,
					MultiredditAction.DELETE_MULTIREDDIT));
		}

		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(activity);

		builder.setItems(menuText, (dialog, which) -> onActionMenuItemSelected(
				activity,
				multiredditName,
				user,
				menu.get(which).action));

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	private static void onActionMenuItemSelected(
			final AppCompatActivity activity,
			final String multiredditName,
			final RedditAccount user,
			final MultiredditAction action) {

		switch(action) {
			case DELETE_MULTIREDDIT:
				new MaterialAlertDialogBuilder(activity)
						.setTitle(activity.getString(R.string.delete_multireddit_confirmation))
						.setMessage(activity.getString(R.string.are_you_sure_delete_multireddit))
						.setPositiveButton(
								activity.getString(R.string.dialog_yes),
								((dialog, which) -> {
									Toast.makeText(
											activity,
											String.format("Deleting %s", multiredditName),
											Toast.LENGTH_SHORT).show();
									RedditAPI.deleteMultireddit(
											CacheManager.getInstance(activity),
											new APIResponseHandler.ActionResponseHandler(
													activity) {

												@Override
												protected void onCallbackException(
														final Throwable t) {
													Log.e(
															TAG,
															"Error while deleting multireddit",
															t);
													throw new RuntimeException(t);
												}

												@Override
												protected void onFailure(
														@NonNull final RRError error) {
													activity.runOnUiThread(() -> {
														final MaterialAlertDialogBuilder builder
																= new MaterialAlertDialogBuilder(
																		activity);
														builder.setView(
																new ErrorView(activity, error));
														builder.create().show();
													});
												}

												@Override
												protected void onSuccess() {
													activity.runOnUiThread(() -> Toast.makeText(
															activity,
															String.format(
																	"Deleted %s", multiredditName),
															Toast.LENGTH_SHORT).show());

													RedditMultiredditSubscriptionManager
															.getSingleton(activity,
																	RedditAccountManager
																			.getInstance(activity)
																			.getDefaultAccount())
															.triggerUpdate(
																	null,
																	TimestampBound.NONE);
												}
											},
											user,
											multiredditName,
											activity
									);
									dialog.dismiss();
								}))
						.setNegativeButton(activity.getString(R.string.dialog_cancel), null)
						.show();
		}
	}
}
