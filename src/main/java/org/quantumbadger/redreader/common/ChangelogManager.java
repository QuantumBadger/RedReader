package org.quantumbadger.redreader.common;

import android.support.v7.app.AppCompatActivity;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.R;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ChangelogManager {

	public static void generateViews(AppCompatActivity context, LinearLayout items) {

		final RRThemeAttributes attr = new RRThemeAttributes(context);

		final int outerPaddingPx = General.dpToPixels(context, 12);
		items.setPadding(outerPaddingPx, 0, outerPaddingPx, outerPaddingPx);

		try {
			final BufferedReader br = new BufferedReader(
				new InputStreamReader(context.getAssets().open("changelog.txt")));

			int curVersionCode = -1;
			String curVersionName = null;

			String line;
			while((line = br.readLine()) != null) {

				if(line.length() == 0) {

					curVersionCode = -1;
					curVersionName = null;

				} else if(curVersionName == null) {

					final String[] lineSplit = line.split("/");
					curVersionCode = Integer.parseInt(lineSplit[0]);
					curVersionName = lineSplit[1];

					final TextView header = (TextView) LayoutInflater.from(context)
						.inflate(R.layout.list_sectionheader, items, false);
					header.setText(curVersionName);
					header.setTextColor(attr.colorAccent);
					items.addView(header);

				} else {

					final LinearLayout bulletItem = new LinearLayout(context);
					final int paddingPx = General.dpToPixels(context, 6);
					bulletItem.setPadding(paddingPx, paddingPx, paddingPx, 0);

					final TextView bullet = new TextView(context);
					bullet.setText("•  ");
					bulletItem.addView(bullet);

					final TextView text = new TextView(context);
					text.setText(line);
					bulletItem.addView(text);

					items.addView(bulletItem);
				}

			}

		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}
}
