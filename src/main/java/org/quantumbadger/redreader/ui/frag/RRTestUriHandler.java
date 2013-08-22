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

package org.quantumbadger.redreader.ui.frag;

import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import java.util.Random;
import java.util.Set;

public class RRTestUriHandler extends RRUriHandler {

	@Override
	public Result handle(final RRContext context, Uri uri, Mode mode, Bundle arguments) {

		if(!uri.getScheme().equals("rr")) return null;

		if(uri.getHost().equals("testfragment")) {

			Log.e("RRTF", uri.toString());

			final Set<String> queryFields = uri.getQueryParameterNames();

			final int col;
			final String text;

			if(queryFields.contains("col")) {
				col = Color.parseColor("#" + uri.getQueryParameter("col"));
			} else {
				final Random r = new Random();
				col = Color.rgb(r.nextInt(255), r.nextInt(255), r.nextInt(255));
			}

			if(queryFields.contains("text")) {
				text = uri.getQueryParameter("text");
			} else {
				text = uri.toString();
			}

			final RRFragment fragment = new RRFragment(context, uri, arguments, null) {
				@Override
				public int preferredWidthLeftcolPx(float dpScale) {
					return (int) (dpScale * 100);
				}

				@Override
				protected View buildContentView() {
					final TextView tv = new TextView(context.activity);
					tv.setText(text);
					tv.setBackgroundColor(col);
					return tv;
				}
			};

			return new Result(fragment);
		}

		if(uri.getHost().equals("listtest")) {

			return new Result(new RRListTestFragment(context, uri, null, null));

		}

		if(uri.getHost().equals("touchtest")) {

			return new Result(new RRTouchTestFragment(context, uri, null, null));

		}

		return null;
	}
}
