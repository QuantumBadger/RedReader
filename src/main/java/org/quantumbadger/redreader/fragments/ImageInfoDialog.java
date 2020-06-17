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
import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import android.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.image.ImageInfo;

public final class ImageInfoDialog extends PropertiesDialog {

	public static ImageInfoDialog newInstance(final ImageInfo info) {

		final ImageInfoDialog pp = new ImageInfoDialog();

		final Bundle args = new Bundle();
		args.putParcelable("info", info);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.props_image_title);
	}

	@Override
	protected void prepare(AppCompatActivity context, LinearLayout items) {

		final ImageInfo info = getArguments().getParcelable("info");

		boolean first = true;

		if(info.title != null && info.title.trim().length() > 0) {
			items.addView(propView(context, R.string.props_title, info.title.trim(), first));
			first = false;
		}

		if(info.caption != null && info.caption.trim().length() > 0) {
			items.addView(propView(context, R.string.props_caption, info.caption.trim(), first));
			first = false;
		}

		items.addView(propView(context, R.string.props_url, info.urlOriginal, first));

		if(info.width != null && info.height != null) {
			items.addView(propView(context, R.string.props_resolution, info.width + " x " + info.height, false));
		}
	}
}
