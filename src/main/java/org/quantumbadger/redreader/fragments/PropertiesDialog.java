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

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.res.TypedArray;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDialogFragment;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;

import java.util.Locale;

public abstract class PropertiesDialog extends AppCompatDialogFragment {

	protected int rrListHeaderTextCol;
	protected int rrListDividerCol;
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
				R.attr.rrListHeaderTextCol,
				R.attr.rrListDividerCol,
				R.attr.rrMainTextCol
		});

		rrListHeaderTextCol = attr.getColor(0, 0);
		rrListDividerCol = attr.getColor(1, 0);
		rrCommentBodyCol = attr.getColor(2, 0);

		attr.recycle();

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		final LinearLayout items = new LinearLayout(activity);
		items.setOrientation(LinearLayout.VERTICAL);

		prepare(activity, items);
		builder.setTitle(getTitle(activity));

		final ScrollView sv = new ScrollView(activity);
		sv.addView(items);
		builder.setView(sv);

		builder.setNeutralButton(R.string.dialog_close, null);

		interceptBuilder(builder);

		return builder.create();
	}

	protected void interceptBuilder(@NonNull final AlertDialog.Builder builder) {
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
			final CharSequence text,
			final boolean firstInList) {
		return propView(context, context.getString(titleRes), text, firstInList);
	}

	// TODO xml?
	protected final LinearLayout propView(
			final Context context,
			final String title,
			final CharSequence text,
			final boolean firstInList) {

		final int paddingPixels = General.dpToPixels(context, 12);

		final LinearLayout prop = new LinearLayout(context);
		prop.setOrientation(LinearLayout.VERTICAL);

		if(!firstInList) {
			final View divider = new View(context);
			divider.setMinimumHeight(General.dpToPixels(context, 1));
			divider.setBackgroundColor(rrListDividerCol);
			prop.addView(divider);
		}

		final TextView titleView = new TextView(context);
		titleView.setText(title.toUpperCase(Locale.getDefault()));
		titleView.setTextColor(rrListHeaderTextCol);
		titleView.setTextSize(12.0f);
		titleView.setPadding(paddingPixels, paddingPixels, paddingPixels, 0);
		prop.addView(titleView);

		final TextView textView = new TextView(context);
		textView.setText(text);
		textView.setTextColor(rrCommentBodyCol);
		textView.setTextSize(15.0f);
		textView.setPadding(paddingPixels, 0, paddingPixels, paddingPixels);
		textView.setTextIsSelectable(true);
		prop.addView(textView);

		prop.setContentDescription(title + "\n" + text);

		if(Build.VERSION.SDK_INT >= 16) {
			textView.setImportantForAccessibility(View.IMPORTANT_FOR_ACCESSIBILITY_NO);
		}

		return prop;
	}
}
