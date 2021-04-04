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

import android.os.Handler;
import android.os.Looper;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.AdapterView;
import android.widget.TextView;
import androidx.annotation.NonNull;

public class AndroidCommon {
	public static final Handler UI_THREAD_HANDLER = new Handler(Looper.getMainLooper());

	public static void runOnUiThread(@NonNull final Runnable runnable) {

		if(General.isThisUIThread()) {
			runnable.run();
		} else {
			UI_THREAD_HANDLER.post(runnable);
		}
	}

	public static void onTextChanged(
			@NonNull final TextView textBox,
			@NonNull final Runnable action) {

		textBox.addTextChangedListener(new TextWatcher() {
			@Override
			public void beforeTextChanged(
					final CharSequence s,
					final int start,
					final int count,
					final int after) {}

			@Override
			public void onTextChanged(
					final CharSequence s,
					final int start,
					final int before,
					final int count) {}

			@Override
			public void afterTextChanged(final Editable s) {
				action.run();
			}
		});
	}

	public static void onSelectedItemChanged(
			@NonNull final AdapterView<?> view,
			@NonNull final Runnable action) {

		view.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {

			@Override
			public void onItemSelected(
					final AdapterView<?> parent,
					final View view,
					final int position,
					final long id) {

				action.run();
			}

			@Override
			public void onNothingSelected(final AdapterView<?> parent) {
				action.run();
			}
		});
	}
}
