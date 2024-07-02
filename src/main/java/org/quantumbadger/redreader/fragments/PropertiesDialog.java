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

import android.app.Dialog;
import android.content.Context;
import android.content.res.TypedArray;
import android.os.Bundle;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatDialogFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textview.MaterialTextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;

public abstract class PropertiesDialog extends AppCompatDialogFragment {

	protected int colorPrimary;
	protected int rrCommentBodyCol;

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	protected abstract String getTitle(Context context);

	protected abstract void prepare(
			@NonNull BaseActivity context,
			@NonNull LinearLayout items);

	@NonNull
	@Override
	public final Dialog onCreateDialog(final Bundle savedInstanceState) {
		super.onCreateDialog(savedInstanceState);

		if(alreadyCreated) {
			return getDialog();
		}
		alreadyCreated = true;

		final BaseActivity activity = (BaseActivity)getActivity();

		final TypedArray attr = activity.obtainStyledAttributes(new int[] {
				com.google.android.material.R.attr.colorPrimary,
				R.attr.rrMainTextCol
		});

		colorPrimary = attr.getColor(0, 0);
		rrCommentBodyCol = attr.getColor(1, 0);

		attr.recycle();

		final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(activity);

		final LinearLayout items = new LinearLayout(activity);
		items.setOrientation(LinearLayout.VERTICAL);

		final int hPaddingPx = General.dpToPixels(activity, 12);
		items.setPadding(hPaddingPx, 0, hPaddingPx, 0);

		prepare(activity, items);
		builder.setTitle(getTitle(activity));

		final ScrollView sv = new ScrollView(activity);
		sv.addView(items);
		builder.setView(sv);

		builder.setNeutralButton(R.string.dialog_close, null);

		interceptBuilder(builder);

		return builder.create();
	}

	protected void interceptBuilder(@NonNull final MaterialAlertDialogBuilder builder) {
		// Do nothing by default
	}

	protected final LinearLayout propView(
			final Context context,
			final int titleRes,
			final int textRes,
			final boolean firstInList) {
		return propView(
				context,
				context.getString(titleRes),
				getString(textRes),
				firstInList);
	}

	protected final LinearLayout propView(
			final Context context,
			final int titleRes,
			@Nullable final CharSequence text,
			final boolean firstInList) {
		return propView(context, context.getString(titleRes), text, firstInList);
	}

	// TODO xml?
	protected final LinearLayout propView(
			final Context context,
			final String title,
			@Nullable final CharSequence text,
			final boolean firstInList) {

		final int paddingPixels = General.dpToPixels(context, 12);

		final LinearLayout prop = new LinearLayout(context);
		prop.setOrientation(LinearLayout.VERTICAL);

		final TextView titleView = new MaterialTextView(context);
		titleView.setText(title);
		titleView.setTextColor(colorPrimary);
		titleView.setTextSize(14.0f);
		titleView.setPadding(paddingPixels, paddingPixels, paddingPixels, 0);
		prop.addView(titleView);

		final TextView textView = new MaterialTextView(context);
		textView.setText(text == null ? "<null>" : text);
		textView.setTextColor(rrCommentBodyCol);
		textView.setTextSize(16.0f);
		textView.setPadding(paddingPixels, 0, paddingPixels, 0);
		textView.setTextIsSelectable(true);
		textView.setImportantForAccessibility(View.IMPORTANT_FOR_ACCESSIBILITY_NO);
		prop.addView(textView);

		prop.setContentDescription(title + "\n" + text);

		return prop;
	}
}
