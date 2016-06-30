package org.quantumbadger.redreader.activities;

import android.os.Bundle;
import android.support.v7.app.ActionBar;
import android.view.MenuItem;
import android.widget.LinearLayout;
import android.widget.ScrollView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.ChangelogManager;
import org.quantumbadger.redreader.common.PrefsUtility;

public class ChangelogActivity extends BaseActivity {

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applySettingsTheme(this);
		setToolbarActionBarEnabled(false);

		super.onCreate(savedInstanceState);

		OptionsMenuUtility.fixActionBar(this, getString(R.string.title_changelog));
		ActionBar ab = getSupportActionBar();
		if (ab != null) {
			ab.setHomeButtonEnabled(true);
			ab.setDisplayHomeAsUpEnabled(true);
		}

		final LinearLayout items = new LinearLayout(this);
		items.setOrientation(LinearLayout.VERTICAL);

		ChangelogManager.generateViews(this, items);

		final ScrollView sv = new ScrollView(this);
		sv.addView(items);
		setBaseActivityContentView(sv);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				finish();
				return true;
			default:
				return false;
		}
	}
}
