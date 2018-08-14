package org.quantumbadger.redreader.activities;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.preference.PreferenceManager;
import android.text.InputType;
import android.util.Log;
import android.view.View;
import android.widget.*;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.util.List;

public class BlocklistListingActivity extends BaseActivity {
	private static final String TAG = BlocklistListingActivity.class.getSimpleName();

	private List<String> blockedPostUrls;

	private ListAdapter mAdapter;
	private ListView mList;

	private boolean mFinishedStart = false;

	private final Handler mHandler = new Handler();
	private final Runnable mRequestFocus = new Runnable() {
		public void run() {
			mList.focusableViewAvailable(mList);
		}
	};
	private final AdapterView.OnItemClickListener mOnClickListener = new AdapterView.OnItemClickListener() {
		public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
			onListItemClick((ListView) parent, v, position, id);
		}
	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		setBaseActivityContentView(R.layout.blocklist_list);
		refresh();

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		blockedPostUrls = PrefsUtility.pref_blocked_post_urls(this, sharedPreferences);
		setListAdapter(new ArrayAdapter<>(this, android.R.layout.simple_list_item_1, blockedPostUrls));

		final String title = getString(R.string.pref_blocked_post_urls_title);
		setTitle(title);
	}

	public void addItems(View v) {
		final EditText urlEditView = new EditText(this);
		urlEditView.setInputType(InputType.TYPE_CLASS_TEXT);

		final AlertDialog dialog = new AlertDialog.Builder(this)
				.setTitle(R.string.pref_blocked_post_enter_domain)
				.setView(urlEditView)
				.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						final String url = urlEditView.getText().toString();

						final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(BlocklistListingActivity.this);
						PrefsUtility.pref_blocked_post_urls_add(BlocklistListingActivity.this, sharedPreferences, url);

						blockedPostUrls.add(url);
						BlocklistListingActivity.this.onContentChanged();
					}
				})
				.setNegativeButton(android.R.string.cancel, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						dialog.cancel();
					}
				})
				.create();
		dialog.show();
	}

	@Override
	protected void onRestoreInstanceState(Bundle state) {
		ensureList();
		super.onRestoreInstanceState(state);
	}

	@Override
	protected void onDestroy() {
		mHandler.removeCallbacks(mRequestFocus);
		super.onDestroy();
	}

	private void refresh() {
		super.onContentChanged();
		final View baseView = getLayoutInflater().inflate(R.layout.blocklist_list, null);
		final View emptyView = baseView.findViewById(android.R.id.empty);
		mList = (ListView) baseView.findViewById(R.id.blocklist_list);
		if (mList == null) {
			throw new RuntimeException(
					"Your content must have a ListView whose id attribute is " +
							"'blocklist_list'");
		}
		if (emptyView != null) {
			mList.setEmptyView(emptyView);
		}
		mList.setOnItemClickListener(mOnClickListener);
		if (mFinishedStart) {
			setListAdapter(mAdapter);
		}
		mHandler.post(mRequestFocus);
		mFinishedStart = true;
	}

	public void setListAdapter(ListAdapter adapter) {
		synchronized (this) {
			ensureList();
			mAdapter = adapter;
			mList.setAdapter(adapter);
		}
	}

	protected void onListItemClick(ListView l, View v, int position, long id) {
		final String url = (String) getListAdapter().getItem(position);
		Log.d(TAG, "Removed: " + url);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		PrefsUtility.pref_blocked_post_urls_remove(this, sharedPreferences, url);

		blockedPostUrls.remove(url);
		this.onContentChanged();
	}

	public void setSelection(int position) {
		mList.setSelection(position);
	}

	public int getSelectedItemPosition() {
		return mList.getSelectedItemPosition();
	}

	public long getSelectedItemId() {
		return mList.getSelectedItemId();
	}

	public ListView getListView() {
		ensureList();
		return mList;
	}

	public ListAdapter getListAdapter() {
		return mAdapter;
	}

	private void ensureList() {
		if (mList != null) {
			return;
		}
		setContentView(android.R.layout.list_content);
	}
}
