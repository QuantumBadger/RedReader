package org.quantumbadger.redreader.activities;

import android.app.AlertDialog;
import android.app.ListActivity;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.text.InputType;
import android.util.Log;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.util.List;

public class BlocklistListingActivity extends ListActivity {
	private static final String TAG = BlocklistListingActivity.class.getSimpleName();

	private List<String> blockedPostUrls;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		setContentView(R.layout.blocklist_list);

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
				.setTitle("Enter URL")
				.setView(urlEditView)
				.setPositiveButton("OK", new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						final String url = urlEditView.getText().toString();

						final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(BlocklistListingActivity.this);
						PrefsUtility.pref_blocked_post_urls_add(BlocklistListingActivity.this, sharedPreferences, url);

						blockedPostUrls.add(url);
						BlocklistListingActivity.this.onContentChanged();
					}
				})
				.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						dialog.cancel();
					}
				})
				.create();
		dialog.show();
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		final String url = (String) getListAdapter().getItem(position);
		Log.d(TAG, "Removed: " + url);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		PrefsUtility.pref_blocked_post_urls_remove(this, sharedPreferences, url);

		blockedPostUrls.remove(url);
		this.onContentChanged();
	}
}
