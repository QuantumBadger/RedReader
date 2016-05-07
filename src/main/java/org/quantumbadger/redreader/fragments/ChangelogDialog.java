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

import android.content.Context;
import android.support.v7.app.AppCompatActivity;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRThemeAttributes;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public final class ChangelogDialog extends PropertiesDialog {

	public static ChangelogDialog newInstance() {
		return new ChangelogDialog();
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.title_changelog);
	}

	@Override
	protected void prepare(AppCompatActivity context, LinearLayout items) {

		final RRThemeAttributes attr = new RRThemeAttributes(context);

		final int outerPaddingPx = General.dpToPixels(context, 12);
		items.setPadding(outerPaddingPx, 0, outerPaddingPx, outerPaddingPx);

		try {
			final BufferedReader br = new BufferedReader(new InputStreamReader(context.getAssets().open("changelog.txt")));

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
