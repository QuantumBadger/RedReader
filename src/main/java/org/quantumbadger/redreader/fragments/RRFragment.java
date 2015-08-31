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

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.view.*;

public abstract class RRFragment {

	private final Activity mParent;

	protected RRFragment(final Activity parent) {
		mParent = parent;
	}

	protected final Context getContext() {
		return mParent;
	}

	protected final Activity getActivity() {
		return mParent;
	}

	protected final String getString(final int resource) {
		return mParent.getApplicationContext().getString(resource);
	}

	protected final void startActivity(final Intent intent) {
		mParent.startActivity(intent);
	}

	protected final void startActivityForResult(final Intent intent, final int requestCode) {
		mParent.startActivityForResult(intent, requestCode);
	}

	// TODO remove
	protected final void registerForContextMenu(View view) {
		mParent.registerForContextMenu(view);
	}

	public void onCreateContextMenu(ContextMenu menu, View v, android.view.ContextMenu.ContextMenuInfo menuInfo) {}
	public boolean onContextItemSelected(MenuItem item) {return false;}

	public void onCreateOptionsMenu(Menu menu) {}
	public boolean onOptionsItemSelected(MenuItem item) {return false;}

	public abstract View onCreateView();
}
