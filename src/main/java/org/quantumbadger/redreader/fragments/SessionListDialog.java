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

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.adapters.SessionListAdapter;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;

import java.net.URI;
import java.util.UUID;

public class SessionListDialog extends AppCompatDialogFragment
		implements RedditAccountChangeListener {

	private URI url;
	private UUID current;
	private SessionChangeListener.SessionChangeType type;

	private RecyclerView rv;

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	public static SessionListDialog newInstance(
			final Uri url,
			final UUID current,
			final SessionChangeListener.SessionChangeType type) {

		final SessionListDialog dialog = new SessionListDialog();

		final Bundle args = new Bundle(3);
		args.putString("url", url.toString());
		if(current != null) {
			args.putString("current", current.toString());
		}
		args.putString("type", type.name());
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		url = General.uriFromString(getArguments().getString("url"));

		if(getArguments().containsKey("current")) {
			current = UUID.fromString(getArguments().getString("current"));
		} else {
			current = null;
		}

		type = SessionChangeListener.SessionChangeType.valueOf(getArguments().getString(
				"type"));
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		super.onCreateDialog(savedInstanceState);

		if(alreadyCreated) {
			return getDialog();
		}
		alreadyCreated = true;

		final Context context = getContext();

		final AlertDialog.Builder builder = new AlertDialog.Builder(context);
		builder.setTitle(context.getString(R.string.options_past));

		rv = new RecyclerView(context);
		builder.setView(rv);

		rv.setLayoutManager(new LinearLayoutManager(context));
		rv.setAdapter(new SessionListAdapter(context, url, current, type, this));
		rv.setHasFixedSize(true);

		RedditAccountManager.getInstance(context).addUpdateListener(this);

		builder.setNeutralButton(context.getString(R.string.dialog_close), null);

		return builder.create();
	}

	@SuppressLint("NotifyDataSetChanged")
	@Override
	public void onRedditAccountChanged() {
		AndroidCommon.UI_THREAD_HANDLER.post(() -> rv.getAdapter().notifyDataSetChanged());
	}
}
