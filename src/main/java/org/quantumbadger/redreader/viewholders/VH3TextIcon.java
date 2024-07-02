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

package org.quantumbadger.redreader.viewholders;

import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.recyclerview.widget.RecyclerView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementLinkButton;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlRawElement;

/**
 * A view holder for a three line, text and icon list item, which can have link buttons.
 */
public class VH3TextIcon extends RecyclerView.ViewHolder {

	public final LinearLayout textHoldingLayout;

	public final TextView text;
	public final TextView text2;
	public final TextView text3;
	public final ImageView icon;
	public final LinearLayoutCompat extra;

	public long bindingId = 0;

	public VH3TextIcon(final View itemView) {
		super(itemView);

		textHoldingLayout = itemView.findViewById(R.id.recycler_text_layout);

		text = itemView.findViewById(R.id.recycler_item_text);
		text2 = itemView.findViewById(R.id.recycler_item_2_text);
		text3 = itemView.findViewById(R.id.recycler_item_3_text);
		icon = itemView.findViewById(R.id.recycler_item_icon);
		extra = itemView.findViewById(R.id.recycler_item_extra);
	}

	public void removeExtras() {
		extra.removeAllViews();
	}

	public void addLinkButton(final BaseActivity activity, final UriString url) {
		final BodyElementLinkButton linkButton
				= new BodyElementLinkButton(new HtmlRawElement.LinkButtonDetails(url.value, url));

		final View linkButtonView =
				linkButton.generateView(
						activity,
						new RRThemeAttributes(activity.getApplicationContext()).rrCommentBodyCol,
						13.0f,
						true);

		extra.addView(linkButtonView);
	}
}
