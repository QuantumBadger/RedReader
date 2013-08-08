package org.quantumbadger.redreader.ui.settings;

import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.view.View;
import android.widget.AdapterView;
import org.holoeverywhere.widget.ListView;
import org.quantumbadger.redreader.settings.RRPreference;
import org.quantumbadger.redreader.settings.RRPrefs;
import org.quantumbadger.redreader.ui.RRContext;
import org.quantumbadger.redreader.ui.frag.RRFragment;

public class PrefFragment extends RRFragment {

	public PrefFragment(RRContext context, Uri uri, Bundle args, Parcelable state) {
		super(context, uri, args, state);
	}

	@Override
	public int maxWidthPx(float dpScale) {
		return (int) (400 * dpScale);
	}

	@Override
	protected View buildContentView() {

		final ListView lv = new ListView(context.activity);

		final RRPreference preference = RRPrefs.getPrefs(context.activity).getPref(getUri());
		final PrefAdapter adapter = new PrefAdapter(context, preference);
		lv.setAdapter(adapter);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				if(view instanceof PreferenceView) ((PreferenceView) view).onClick();
			}
		});

		lv.setDivider(null);

		return lv;
	}
}
