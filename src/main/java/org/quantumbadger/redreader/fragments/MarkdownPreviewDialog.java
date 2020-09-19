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
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParagraphGroup;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParser;

public class MarkdownPreviewDialog extends PropertiesDialog {

	public static MarkdownPreviewDialog newInstance(final String markdown) {

		final MarkdownPreviewDialog dialog = new MarkdownPreviewDialog();

		final Bundle args = new Bundle(1);
		args.putString("markdown", markdown);
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	protected String getTitle(final Context context) {
		return context.getString(R.string.comment_reply_preview);
	}

	@Override
	protected void prepare(
			@NonNull final BaseActivity activity,
			@NonNull final LinearLayout items) {

		final MarkdownParagraphGroup parsedGen
				= MarkdownParser.parse(getArguments().getString("markdown")
				.toCharArray());

		final ViewGroup parsed = parsedGen.buildView(activity, null, 14f, false);

		final int paddingPx = General.dpToPixels(activity, 10);
		parsed.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		items.addView(parsed);
	}
}
