/*******************************************************************************
 * This file is part of RedReader.
 * <p>
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.adapters;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.support.v4.app.Fragment;
import android.support.v4.content.ContextCompat;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.OAuthLoginActivity;
import org.quantumbadger.redreader.common.BetterSSB;

import java.util.ArrayList;

public class AccountListAdapter extends HeaderRecyclerAdapter<AccountListAdapter.VH> {

	private final Context context;
	private final Fragment fragment;

	private final ArrayList<RedditAccount> accounts;
	private final Drawable rrIconAdd;

	public AccountListAdapter(final Context context, final Fragment fragment) {
		this.context = context;
		this.fragment = fragment;

		accounts = RedditAccountManager.getInstance(context).getAccounts();

		final TypedArray attr = context.obtainStyledAttributes(new int[]{R.attr.rrIconAdd});
		rrIconAdd = ContextCompat.getDrawable(context, attr.getResourceId(0, 0));
		attr.recycle();
	}

	@Override
	protected VH onCreateHeaderItemViewHolder(ViewGroup parent) {
		View v = LayoutInflater.from(parent.getContext())
			.inflate(R.layout.recycler_item, parent, false);
		return new VH(v, context);
	}

	@Override
	protected VH onCreateContentItemViewHolder(ViewGroup parent) {
		View v = LayoutInflater.from(parent.getContext())
			.inflate(R.layout.recycler_item, parent, false);
		return new VH(v, context);
	}

	@Override
	protected void onBindHeaderItemViewHolder(VH holder, int position) {
		holder.text.setText(context.getString(R.string.accounts_add));
		holder.icon.setVisibility(View.VISIBLE);
		holder.icon.setImageDrawable(rrIconAdd);
		holder.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final Intent loginIntent = new Intent(context, OAuthLoginActivity.class);
				fragment.startActivityForResult(loginIntent, 123);
			}
		});
	}

	@Override
	protected void onBindContentItemViewHolder(VH holder, final int position) {
		final RedditAccount account = accounts.get(position);
		final BetterSSB username = new BetterSSB();

		if (account.isAnonymous()) {
			username.append(context.getString(R.string.accounts_anon), 0);
		} else {
			username.append(account.username, 0);
		}

		if (account.equals(RedditAccountManager.getInstance(context).getDefaultAccount())) {
			final TypedArray attr = context.obtainStyledAttributes(new int[]{R.attr.rrListSubtitleCol});
			final int col = attr.getColor(0, 0);
			attr.recycle();

			username.append("  (" + context.getString(R.string.accounts_active) + ")", BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE, col, 0, 0.8f);
		}

		holder.text.setText(username.get());
		holder.icon.setVisibility(View.GONE);

		holder.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				final RedditAccount account = accounts.get(position);
				final String[] items = account.isAnonymous()
					? new String[]{context.getString(R.string.accounts_setactive)}
					: new String[]{
					context.getString(R.string.accounts_setactive),
					context.getString(R.string.accounts_delete)
				};

				final AlertDialog.Builder builder = new AlertDialog.Builder(context);

				builder.setItems(items, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						final String selected = items[which];

						if (selected.equals(context.getString(R.string.accounts_setactive))) {
							RedditAccountManager.getInstance(context).setDefaultAccount(account);
						} else if (selected.equals(context.getString(R.string.accounts_delete))) {
							new AlertDialog.Builder(context)
								.setTitle(R.string.accounts_delete)
								.setMessage(R.string.accounts_delete_sure)
								.setPositiveButton(R.string.accounts_delete,
									new DialogInterface.OnClickListener() {
										@Override
										public void onClick(final DialogInterface dialog, final int which) {
											RedditAccountManager.getInstance(context).deleteAccount(account);
										}
									})
								.setNegativeButton(R.string.dialog_cancel, null)
								.show();
						}
					}
				});

				builder.setNeutralButton(R.string.dialog_cancel, null);

				final AlertDialog alert = builder.create();
				alert.setTitle(account.isAnonymous() ? context.getString(R.string.accounts_anon) : account.username);
				alert.setCanceledOnTouchOutside(true);
				alert.show();
			}
		});
	}

	@Override
	protected int getContentItemCount() {
		return accounts.size();
	}

	static class VH extends RecyclerView.ViewHolder {

		final Context context;
		final ImageView icon;
		final TextView text;

		public VH(View itemView, Context context) {
			super(itemView);
			this.context = context;

			icon = (ImageView) itemView.findViewById(R.id.recycler_item_icon);
			text = (TextView) itemView.findViewById(R.id.recycler_item_text);
		}
	}
}
