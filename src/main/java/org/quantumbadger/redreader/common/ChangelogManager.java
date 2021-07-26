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

import android.os.Build;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.AccessibilityDelegateCompat;
import androidx.core.view.ViewCompat;
import androidx.core.view.accessibility.AccessibilityNodeInfoCompat;
import org.quantumbadger.redreader.R;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ChangelogManager {

	public static void generateViews(
			final AppCompatActivity context,
			final LinearLayout items,
			final boolean showAll) {

		final RRThemeAttributes attr = new RRThemeAttributes(context);

		final int outerPaddingPx = General.dpToPixels(context, 12);
		items.setPadding(outerPaddingPx, 0, outerPaddingPx, outerPaddingPx);

		final String filename;

		if(context.getPackageName().contains("alpha")) {
			filename = "changelog-alpha.txt";
		} else {
			filename = "changelog.txt";
		}

		try(BufferedReader br = new BufferedReader(
				new InputStreamReader(context.getAssets().open(filename)),
				128 * 1024)) {

			String curVersionName = null;

			int itemsToShow = 10;

			String line;
			while((line = br.readLine()) != null) {

				if(line.isEmpty()) {

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

					final TextView header = (TextView)LayoutInflater.from(context)
							.inflate(
									R.layout.list_sectionheader,
									items,
									false);
					header.setText(curVersionName);
					header.setTextColor(attr.colorAccent);

					//From https://stackoverflow.com/a/54082384
					ViewCompat.setAccessibilityDelegate(
							header,
							new AccessibilityDelegateCompat() {
								@Override
								public void onInitializeAccessibilityNodeInfo(
										final View host,
										final AccessibilityNodeInfoCompat info) {
									super.onInitializeAccessibilityNodeInfo(host, info);
									info.setHeading(true);
								}
							});

					items.addView(header);

				} else {

					final LinearLayout bulletItem = new LinearLayout(context);
					final int paddingPx = General.dpToPixels(context, 6);
					bulletItem.setPadding(paddingPx, paddingPx, paddingPx, 0);
					bulletItem.setFocusable(true);

					final TextView bullet = new TextView(context);
					bullet.setText("•  ");
					bulletItem.addView(bullet);
					if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
						bullet.setImportantForAccessibility(View.IMPORTANT_FOR_ACCESSIBILITY_NO);
					}

					final TextView text = new TextView(context);
					text.setText(line);
					bulletItem.addView(text);

					items.addView(bulletItem);
				}

			}

		} catch(final IOException e) {
			throw new RuntimeException(e);
		}
	}
}
