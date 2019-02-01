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

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.support.annotation.Nullable;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.TextView;
import org.apache.commons.lang3.StringUtils;
import org.quantumbadger.redreader.R;

public class DialogUtils {
	public interface OnSearchListener {
		void onSearch(@Nullable String query);
	}

	public static void showSearchDialog (Context context, final OnSearchListener listener) {
		showSearchDialog(context, R.string.action_search, listener);
	}
	public static void showSearchDialog (Context context, int titleRes, final OnSearchListener listener) {
		final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(context);
		final EditText editText = (EditText) LayoutInflater.from(context).inflate(R.layout.dialog_editbox, null);

		TextView.OnEditorActionListener onEnter = new TextView.OnEditorActionListener() {
			@Override
			public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
				performSearch(editText, listener);
				return true;
			}
		};
		editText.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
		editText.setOnEditorActionListener(onEnter);

		alertBuilder.setView(editText);
		alertBuilder.setTitle(titleRes);

		alertBuilder.setPositiveButton(R.string.action_search, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				performSearch(editText, listener);
			}
		});

		alertBuilder.setNegativeButton(R.string.dialog_cancel, null);

		final AlertDialog alertDialog = alertBuilder.create();
		alertDialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
		alertDialog.show();
	}

	private static void performSearch(final EditText editText, final OnSearchListener listener) {
		final String query = General.asciiLowercase(editText.getText().toString()).trim();
		if (StringUtils.isEmpty(query)) {
			listener.onSearch(null);
		} else {
			listener.onSearch(query);
		}
	}
}
