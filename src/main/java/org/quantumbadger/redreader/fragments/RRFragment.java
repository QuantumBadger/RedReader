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
