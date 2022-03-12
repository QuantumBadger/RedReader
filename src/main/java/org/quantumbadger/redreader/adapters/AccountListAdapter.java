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
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.FrameLayout;
import androidx.appcompat.app.AlertDialog;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.RecyclerView;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.OAuthLoginActivity;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;
import org.quantumbadger.redreader.viewholders.VH1Text;

import java.util.ArrayList;
import java.util.Locale;

public class AccountListAdapter extends HeaderRecyclerAdapter<RecyclerView.ViewHolder> {

	private final Context context;
	private final Fragment fragment;

	private final ArrayList<RedditAccount> accounts;
	private final Drawable rrIconAdd;

	public AccountListAdapter(final Context context, final Fragment fragment) {
		this.context = context;
		this.fragment = fragment;

		accounts = RedditAccountManager.getInstance(context).getAccounts();

		final TypedArray attr
				= context.obtainStyledAttributes(new int[] {R.attr.rrIconAdd});
		rrIconAdd = ContextCompat.getDrawable(context, attr.getResourceId(0, 0));
		attr.recycle();
	}

	@Override
	protected RecyclerView.ViewHolder onCreateHeaderItemViewHolder(final ViewGroup parent) {
		final View v = LayoutInflater.from(parent.getContext())
				.inflate(R.layout.list_item_1_text, parent, false);
		return new VH1Text(v);
	}

	@Override
	protected RecyclerView.ViewHolder onCreateContentItemViewHolder(final ViewGroup parent) {
		final View v = LayoutInflater.from(parent.getContext())
				.inflate(R.layout.list_item_1_text, parent, false);
		return new VH1Text(v);
	}

	@Override
	protected void onBindHeaderItemViewHolder(
			final RecyclerView.ViewHolder holder,
			final int position) {
		final VH1Text vh = (VH1Text)holder;
		vh.text.setText(context.getString(R.string.accounts_add));
		vh.text.setCompoundDrawablesWithIntrinsicBounds(rrIconAdd, null, null, null);
		holder.itemView.setOnClickListener(v -> showLoginWarningDialog());
	}

	private void showLoginWarningDialog() {

		final FrameLayout dialogView = new FrameLayout(context);

		final CheckBox browserCheckbox = new CheckBox(context);
		browserCheckbox.setChecked(true);
		dialogView.addView(browserCheckbox);

		final boolean internalRecommended
				= (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M);

		final int margin = General.dpToPixels(context, 24);
		((ViewGroup.MarginLayoutParams)browserCheckbox.getLayoutParams())
				.setMargins(margin, margin, margin, margin);

		if(internalRecommended) {
			browserCheckbox.setText(R.string.reddit_login_browser_popup_use_internal_browser);
		} else {
			browserCheckbox.setText(R.string.reddit_login_browser_popup_use_external_browser);
		}

		new MaterialAlertDialogBuilder(context)
				.setMessage(String.format(
						Locale.US,
						"%s\n\n%s",
						context.getString(R.string.reddit_login_browser_popup_line_1),
						context.getString(R.string.reddit_login_browser_popup_line_2)))
				.setCancelable(true)
				.setPositiveButton(
						R.string.dialog_continue,
						(dialog, which) -> {

							final boolean useInternal
									= (browserCheckbox.isChecked() && internalRecommended)
									|| (!browserCheckbox.isChecked() && !internalRecommended);

							launchLogin(useInternal);
						})
				.setNegativeButton(
						R.string.dialog_close,
						(dialog, which) -> {})
				.setView(dialogView)
				.show();
	}

	private void launchLogin(final boolean useInternalBrowser) {

		if(useInternalBrowser) {
			final Intent loginIntent = new Intent(context, OAuthLoginActivity.class);
			fragment.startActivityForResult(loginIntent, 123);

		} else {
			final Intent intent = new Intent(Intent.ACTION_VIEW);
			intent.setData(RedditOAuth.getPromptUri());
			intent.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
			intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			fragment.startActivity(intent);
		}
	}

	@Override
	protected void onBindContentItemViewHolder(
			final RecyclerView.ViewHolder holder,
			final int position) {
		final VH1Text vh = (VH1Text)holder;
		final RedditAccount account = accounts.get(position);
		final BetterSSB username = new BetterSSB();

		if(account.isAnonymous()) {
			username.append(context.getString(R.string.accounts_anon), 0);
		} else {
			username.append(account.username, 0);
		}

		if(account.equals(RedditAccountManager.getInstance(context)
				.getDefaultAccount())) {
			final TypedArray attr
					= context.obtainStyledAttributes(new int[] {R.attr.rrListSubtitleCol});
			final int col = attr.getColor(0, 0);
			attr.recycle();

			username.append(
					"  (" + context.getString(R.string.accounts_active) + ")",
					BetterSSB.FOREGROUND_COLOR | BetterSSB.SIZE,
					col,
					0,
					0.8f);
		}

		vh.text.setText(username.get());

		vh.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(final View v) {
				final RedditAccount account = accounts.get(position);
				final String[] items = account.isAnonymous()
						? new String[] {context.getString(R.string.accounts_setactive)}
						: new String[] {
								context.getString(R.string.accounts_setactive),
								context.getString(R.string.accounts_delete)
						};

				final MaterialAlertDialogBuilder builder
						= new MaterialAlertDialogBuilder(context);

				builder.setItems(items, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(final DialogInterface dialog, final int which) {
						final String selected = items[which];

						if(selected.equals(context.getString(R.string.accounts_setactive))) {
							RedditAccountManager.getInstance(context)
									.setDefaultAccount(account);
						} else if(selected.equals(context.getString(R.string.accounts_delete))) {
							new MaterialAlertDialogBuilder(context)
									.setTitle(R.string.accounts_delete)
									.setMessage(R.string.accounts_delete_sure)
									.setPositiveButton(
											R.string.accounts_delete,
											new DialogInterface.OnClickListener() {
												@Override
												public void onClick(
														final DialogInterface dialog,
														final int which) {
													RedditAccountManager.getInstance(context)
															.deleteAccount(account);
												}
											})
									.setNegativeButton(R.string.dialog_cancel, null)
									.show();
						}
					}
				});

				builder.setNeutralButton(R.string.dialog_cancel, null);

				final AlertDialog alert = builder.create();
				alert.setTitle(account.isAnonymous()
						? context.getString(R.string.accounts_anon)
						: account.username);
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
