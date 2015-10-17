/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.fragments;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import com.konneh.scroll.R;
import com.konneh.scroll.common.General;
import com.konneh.scroll.reddit.prepared.markdown.MarkdownParagraphGroup;
import com.konneh.scroll.reddit.prepared.markdown.MarkdownParser;

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
	protected void prepare(Activity context, LinearLayout items) {

		final MarkdownParagraphGroup parsedGen
				= MarkdownParser.parse(getArguments().getString("markdown").toCharArray());

		final ViewGroup parsed = parsedGen.buildView(context, null, 14f, false);

		final int paddingPx = General.dpToPixels(context, 10);
		parsed.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		items.addView(parsed);
	}
}
