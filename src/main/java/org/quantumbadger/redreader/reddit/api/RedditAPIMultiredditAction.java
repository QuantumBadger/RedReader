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

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.RemoveSubredditFromMultiDialog;

import java.util.ArrayList;
import java.util.EnumSet;

public class RedditAPIMultiredditAction {

	public enum MultiredditAction {
		REMOVE_SUBREDDIT
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

		if(itemPref.contains(MultiredditAction.REMOVE_SUBREDDIT)) {
			menu.add(new RCVMenuItem(
				activity,
				R.string.remove_from_multireddit,
				MultiredditAction.REMOVE_SUBREDDIT));
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
			case REMOVE_SUBREDDIT:
				RemoveSubredditFromMultiDialog.show(activity, multiredditName, user);
		}
	}
}
