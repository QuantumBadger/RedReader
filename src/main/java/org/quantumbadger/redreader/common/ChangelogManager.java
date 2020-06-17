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

package org.quantumbadger.redreader.common;

import androidx.appcompat.app.AppCompatActivity;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.R;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ChangelogManager {

	public static void generateViews(AppCompatActivity context, LinearLayout items, boolean showAll) {

		final RRThemeAttributes attr = new RRThemeAttributes(context);

		final int outerPaddingPx = General.dpToPixels(context, 12);
		items.setPadding(outerPaddingPx, 0, outerPaddingPx, outerPaddingPx);

		try {
			final BufferedReader br = new BufferedReader(
				new InputStreamReader(context.getAssets().open("changelog.txt")),
				128 * 1024);

			String curVersionName = null;

			int itemsToShow = 10;

			String line;
			while((line = br.readLine()) != null) {

				if(line.length() == 0) {

					curVersionName = null;

					if(!showAll) {
						itemsToShow--;
						if(itemsToShow <= 0) {
							break;
						}
					}

				} else if(curVersionName == null) {

					final String[] lineSplit = line.split("/");
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
