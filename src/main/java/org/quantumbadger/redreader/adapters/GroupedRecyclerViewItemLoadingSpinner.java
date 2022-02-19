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

package org.quantumbadger.redreader.adapters;

import android.content.Context;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LoadingSpinnerView;

public class GroupedRecyclerViewItemLoadingSpinner extends GroupedRecyclerViewItemView {

	public GroupedRecyclerViewItemLoadingSpinner(@NonNull final Context context) {
		super(LoadingSpinnerView.class, input -> {

			final LoadingSpinnerView loadingSpinnerView = new LoadingSpinnerView(context);

			final int paddingPx = General.dpToPixels(context, 30);
			loadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

			return loadingSpinnerView;
		});
	}
}
