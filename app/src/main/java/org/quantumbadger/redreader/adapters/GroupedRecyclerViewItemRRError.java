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

import android.view.ViewGroup;
import android.widget.FrameLayout;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

public class GroupedRecyclerViewItemRRError extends GroupedRecyclerViewAdapter.Item<
		GroupedRecyclerViewItemRRError.ErrorHolder> {

	@NonNull private final AppCompatActivity mActivity;
	@NonNull private final RRError mError;

	public class ErrorHolder extends RecyclerView.ViewHolder {

		public ErrorHolder() {
			super(new FrameLayout(mActivity));
		}

		public void bind(@NonNull final RRError error) {
			final FrameLayout itemView = (FrameLayout)this.itemView;
			itemView.removeAllViews();
			itemView.addView(new ErrorView(mActivity, error));
		}
	}

	public GroupedRecyclerViewItemRRError(
			@NonNull final AppCompatActivity activity,
			@NonNull final RRError error) {

		mActivity = activity;
		mError = error;
	}

	@Override
	public Class<?> getViewType() {
		return GroupedRecyclerViewItemRRError.class;
	}

	@Override
	public ErrorHolder onCreateViewHolder(final ViewGroup viewGroup) {
		return new ErrorHolder();
	}

	@Override
	public void onBindViewHolder(final ErrorHolder viewHolder) {
		viewHolder.bind(mError);
	}

	@Override
	public boolean isHidden() {
		return false;
	}
}
