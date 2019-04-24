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

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.MainActivity;
import org.quantumbadger.redreader.activities.OAuthLoginActivity;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.viewholders.VH1Text;

import java.util.ArrayList;

public class AccountListAdapter extends HeaderRecyclerAdapter<RecyclerView.ViewHolder> {

	private final AppCompatActivity activity;
	private final Fragment fragment;

	private final ArrayList<RedditAccount> accounts;
	private final Drawable rrIconAdd;

	public AccountListAdapter(final AppCompatActivity activity, final Fragment fragment) {
		this.activity = activity;
		this.fragment = fragment;

		accounts = RedditAccountManager.getInstance(this.activity).getAccounts();

		final TypedArray attr = this.activity.obtainStyledAttributes(new int[]{R.attr.rrIconAdd});
		rrIconAdd = ContextCompat.getDrawable(this.activity, attr.getResourceId(0, 0));
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
	protected void onBindHeaderItemViewHolder(RecyclerView.ViewHolder holder, int position) {
		final VH1Text vh = (VH1Text) holder;
		vh.text.setText(activity.getString(R.string.accounts_add));
		vh.text.setCompoundDrawablesWithIntrinsicBounds(rrIconAdd, null, null, null);
		holder.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final Intent loginIntent = new Intent(activity, OAuthLoginActivity.class);
				fragment.startActivityForResult(loginIntent, 123);
			}
		});
	}

	@Override
	protected void onBindContentItemViewHolder(RecyclerView.ViewHolder holder, final int position) {
		final VH1Text vh = (VH1Text) holder;
		final RedditAccount account = accounts.get(position);
		final BetterSSB username = new BetterSSB();

		if (account.isAnonymous()) {
			username.append(activity.getString(R.string.accounts_anon), 0);
		} else {
			username.append(account.username, 0);
		}

		if (account.equals(RedditAccountManager.getInstance(activity).getDefaultAccount())) {
			final TypedArray attr = activity.obtainStyledAttributes(new int[]{R.attr.rrListSubtitleCol});
			final int col = attr.getColor(0, 0);
			attr.recycle();

			username.append("  (" + activity.getString(R.string.accounts_active) + ")", BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE, col, 0, 0.8f);
		}

		vh.text.setText(username.get());

		vh.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final RedditAccount account = accounts.get(position);
				final String[] items = account.isAnonymous()
					? new String[]{activity.getString(R.string.accounts_setactive)}
					: new String[]{
					activity.getString(R.string.accounts_setactive),
					activity.getString(R.string.accounts_delete)
				};

				final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

				builder.setItems(items, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						final String selected = items[which];

						if (selected.equals(activity.getString(R.string.accounts_setactive))) {
							RedditAccountManager.getInstance(activity).setDefaultAccount(account);
							if(PrefsUtility.appearance_navigation_type(activity, PreferenceManager.getDefaultSharedPreferences(activity)).equals("drawer_tabs")) {
								activity.startActivity(new Intent(activity, MainActivity.class));
								activity.finish();
							}
						} else if (selected.equals(activity.getString(R.string.accounts_delete))) {
							new AlertDialog.Builder(activity)
								.setTitle(R.string.accounts_delete)
								.setMessage(R.string.accounts_delete_sure)
								.setPositiveButton(R.string.accounts_delete,
									new DialogInterface.OnClickListener() {
										@Override
										public void onClick(final DialogInterface dialog, final int which) {
											RedditAccountManager.getInstance(activity).deleteAccount(account);
										}
									})
								.setNegativeButton(R.string.dialog_cancel, null)
								.show();
						}
					}
				});

				builder.setNeutralButton(R.string.dialog_cancel, null);

				final AlertDialog alert = builder.create();
				alert.setTitle(account.isAnonymous() ? activity.getString(R.string.accounts_anon) : account.username);
				alert.setCanceledOnTouchOutside(true);
				alert.show();
			}
		});
	}

	@Override
	protected int getContentItemCount() {
		return accounts.size();
	}
}
