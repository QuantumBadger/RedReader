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

package org.quantumbadger.redreader.activities;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import android.util.Log;
import org.quantumbadger.redreader.common.LinkHandler;

public class LinkDispatchActivity extends AppCompatActivity {

	private static final String TAG = "LinkDispatchActivity";

	@Override
	protected void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		if(intent == null) {
			Log.e(TAG, "Got null intent");
			finish();
			return;
		}

		final Uri data = intent.getData();

		if(data == null) {
			Log.e(TAG, "Got null intent data");
			finish();
			return;
		}

		LinkHandler.onLinkClicked(this, data.toString(), false, null, null, 0, true);
		finish();
	}
}
