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
import android.view.ViewGroup;
import com.laurencedawson.activetextview.ActiveTextView;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.prepared.RedditCommentTextParser;

public class MarkdownPreviewDialog extends PropertiesDialog {

	public static MarkdownPreviewDialog newInstance(String markdown) {

		final MarkdownPreviewDialog dialog = new MarkdownPreviewDialog();

		final Bundle args = new Bundle(1);
		args.putString("markdown", markdown);
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.comment_reply_preview);
	}

	@Override
	protected void prepare(Context context, LinearLayout items) {

		final RedditCommentTextParser.ViewGenerator parsedGen = RedditCommentTextParser.parse(getArguments().getString("markdown"));

		final ViewGroup parsed = parsedGen.generate(context, 14f, null, new ActiveTextView.OnLinkClickedListener() {
			public void onClickUrl(String url) {
				if(url != null) LinkHandler.onLinkClicked(getSupportActivity(), url, false);
			}

			public void onClickText(Object attachment) {}
		}, null);

		final int paddingPx = General.dpToPixels(context, 10);
		parsed.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		items.addView(parsed);
	}
}
