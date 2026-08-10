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

import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.StaggeredGridLayoutManager;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

@SuppressWarnings("ForLoopReplaceableByForEach")
public class GroupedRecyclerViewAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

	private static final AtomicLong ITEM_UNIQUE_ID_GENERATOR = new AtomicLong(100_000);

	// Used by staggered (masonry) grid layouts to decide which items should span
	// the full width instead of occupying a single column.
	public interface FullSpanChecker {
		boolean isFullSpan(final int position);
	}

	public static abstract class Item<VH extends RecyclerView.ViewHolder> {

		private final long mUniqueId = ITEM_UNIQUE_ID_GENERATOR.incrementAndGet();
		private boolean mCurrentlyHidden = false;

		public abstract Class<?> getViewType();

		public abstract VH onCreateViewHolder(final ViewGroup viewGroup);

		public abstract void onBindViewHolder(final VH viewHolder);

		public abstract boolean isHidden();

		private void onBindViewHolderInner(
				final RecyclerView.ViewHolder viewHolder) {
			//noinspection unchecked
			onBindViewHolder((VH)viewHolder);
		}
	}

	private final ArrayList<Item<?>>[] mItems;
	private final HashMap<Class<?>, Integer> mItemViewTypeMap = new HashMap<>();
	private final HashMap<Integer, Item<?>> mViewTypeItemMap = new HashMap<>();
	private FullSpanChecker mFullSpanChecker;

	public GroupedRecyclerViewAdapter(final int groups) {
		//noinspection unchecked
		mItems = (ArrayList<Item<?>>[])new ArrayList[groups];

		for(int i = 0; i < groups; i++) {
			mItems[i] = new ArrayList<>();
		}

		setHasStableIds(true);
	}

	public void setFullSpanChecker(final FullSpanChecker fullSpanChecker) {
		mFullSpanChecker = fullSpanChecker;
	}

	private int getItemPositionInternal(final int groupId, final Item<?> item) {

		final ArrayList<Item<?>> group = mItems[groupId];

		for(int i = 0; i < group.size(); i++) {
			if(group.get(i) == item) {
				return getItemPositionInternal(groupId, i);
			}
		}

		throw new RuntimeException("Item not found");
	}

	// "positionInGroup" should include both hidden and visible items
	private int getItemPositionInternal(final int group, final int positionInGroup) {

		int result = 0;

		for(int i = 0; i < group; i++) {
			result += getGroupUnhiddenCount(i);
		}

		for(int i = 0; i < positionInGroup; i++) {
			if(!mItems[group].get(i).mCurrentlyHidden) {
				result++;
			}
		}

		return result;
	}

	private Item<?> getItemInternal(final int desiredPosition) {

		if(desiredPosition < 0) {
			throw new RuntimeException("Item desiredPosition "
					+ desiredPosition
					+ " is too low");
		}

		int currentPosition = 0;

		for(int groupId = 0; groupId < mItems.length; groupId++) {

			final ArrayList<Item<?>> group = mItems[groupId];

			for(int positionInGroup = 0;
				positionInGroup < group.size();
				positionInGroup++) {

				final Item<?> item = group.get(positionInGroup);

				if(!item.mCurrentlyHidden) {

					if(currentPosition == desiredPosition) {
						return item;
					}

					currentPosition++;
				}
			}
		}

		throw new RuntimeException("Item desiredPosition "
				+ desiredPosition
				+ " is too high");
	}

	@NonNull
	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(
			@NonNull final ViewGroup viewGroup,
			final int viewType) {

		final RecyclerView.ViewHolder viewHolder = mViewTypeItemMap.get(viewType)
				.onCreateViewHolder(viewGroup);

		final RecyclerView.LayoutParams layoutParams = new RecyclerView.LayoutParams(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.WRAP_CONTENT);

		if(viewHolder.itemView.getLayoutParams() instanceof ViewGroup.MarginLayoutParams) {

			final ViewGroup.MarginLayoutParams oldLayoutParams
					= (ViewGroup.MarginLayoutParams)viewHolder.itemView.getLayoutParams();

			layoutParams.setMargins(
					oldLayoutParams.leftMargin,
					oldLayoutParams.topMargin,
					oldLayoutParams.rightMargin,
					oldLayoutParams.bottomMargin);
		}

		viewHolder.itemView.setLayoutParams(layoutParams);

		return viewHolder;
	}

	@Override
	public void onBindViewHolder(
			@NonNull final RecyclerView.ViewHolder viewHolder,
			final int position) {
		getItemInternal(position).onBindViewHolderInner(viewHolder);

		if(mFullSpanChecker != null) {
			applyFullSpan(viewHolder.itemView, position);
		}
	}

	@Override
	public void onViewAttachedToWindow(
			@NonNull final RecyclerView.ViewHolder viewHolder) {
		super.onViewAttachedToWindow(viewHolder);

		if(mFullSpanChecker == null) {
			return;
		}

		final int position = viewHolder.getLayoutPosition();

		if(position != RecyclerView.NO_POSITION) {
			applyFullSpan(viewHolder.itemView, position);
		}
	}

	// In staggered (masonry) grid layouts items are confined to a single column;
	// chrome items (headers, loading spinners, load-more buttons, errors) must
	// span the full width so the masonry flow isn't broken. In list mode (or any
	// other layout manager) this is a no-op, because the layout params never are
	// StaggeredGridLayoutManager.LayoutParams.
	private void applyFullSpan(
			@NonNull final View itemView,
			final int position) {

		final ViewGroup.LayoutParams layoutParams = itemView.getLayoutParams();

		if(layoutParams instanceof StaggeredGridLayoutManager.LayoutParams) {
			((StaggeredGridLayoutManager.LayoutParams)layoutParams)
					.setFullSpan(mFullSpanChecker.isFullSpan(position));
		}
	}

	@Override
	public int getItemViewType(final int position) {

		final Item<?> item = getItemInternal(position);
		final Class<?> viewTypeClass = item.getViewType();

		Integer typeId = mItemViewTypeMap.get(viewTypeClass);

		if(typeId == null) {
			typeId = mItemViewTypeMap.size();
			mItemViewTypeMap.put(viewTypeClass, typeId);
			mViewTypeItemMap.put(typeId, item);
		}

		return typeId;
	}

	private int getGroupUnhiddenCount(final int groupId) {

		final ArrayList<Item<?>> group = mItems[groupId];

		int result = 0;

		for(int i = 0; i < group.size(); i++) {
			if(!group.get(i).mCurrentlyHidden) {
				result++;
			}
		}

		return result;
	}

	@Override
	public long getItemId(final int position) {
		return getItemInternal(position).mUniqueId;
	}

	@Override
	public int getItemCount() {

		int count = 0;

		for(int i = 0; i < mItems.length; i++) {
			count += getGroupUnhiddenCount(i);
		}

		return count;
	}

	public Item<?> getItemAtPosition(final int position) {
		return getItemInternal(position);
	}

	public int getGroupIdAtPosition(final int position) {

		int currentPosition = 0;

		for(int groupId = 0; groupId < mItems.length; groupId++) {

			for(int positionInGroup = 0;
				positionInGroup < mItems[groupId].size();
				positionInGroup++) {

				final Item<?> item = mItems[groupId].get(positionInGroup);

				if(!item.mCurrentlyHidden) {

					if(currentPosition == position) {
						return groupId;
					}

					currentPosition++;
				}
			}
		}

		throw new RuntimeException("Item position "
				+ position
				+ " is too high");
	}

	public void appendToGroup(final int group, final Item<?> item) {

		final int position = getItemPositionInternal(group + 1, 0);

		mItems[group].add(item);

		if(!item.mCurrentlyHidden) {
			notifyItemInserted(position);
		}
	}

	public void appendToGroup(final int group, final Collection<Item<?>> items) {

		final int position = getItemPositionInternal(group + 1, 0);

		mItems[group].addAll(items);

		for(final Item<?> item : items) {
			item.mCurrentlyHidden = false;
		}

		notifyItemRangeInserted(position, items.size());
	}

	public void removeAllFromGroup(final int groupId) {

		final ArrayList<Item<?>> group = mItems[groupId];

		for(int i = group.size() - 1; i >= 0; i--) {

			final Item<?> item = group.get(i);
			final int position = getItemPositionInternal(groupId, i);

			group.remove(i);

			if(!item.mCurrentlyHidden) {
				notifyItemRemoved(position);
			}
		}
	}

	public void removeFromGroup(final int groupId, final Item<?> item) {

		final ArrayList<Item<?>> group = mItems[groupId];

		for(int i = 0; i < group.size(); i++) {
			if(group.get(i) == item) {

				final int position = getItemPositionInternal(groupId, i);

				group.remove(i);

				if(!item.mCurrentlyHidden) {
					notifyItemRemoved(position);
				}

				return;
			}
		}

		throw new RuntimeException("Item not found");
	}

	public void updateHiddenStatus() {

		int position = 0;

		for(int groupId = 0; groupId < mItems.length; groupId++) {

			final ArrayList<Item<?>> group = mItems[groupId];

			for(int positionInGroup = 0;
				positionInGroup < group.size();
				positionInGroup++) {

				final Item<?> item = group.get(positionInGroup);

				final boolean wasHidden = item.mCurrentlyHidden;
				final boolean isHidden = item.isHidden();
				item.mCurrentlyHidden = isHidden;

				if(isHidden && !wasHidden) {
					notifyItemRemoved(position);

				} else if(!isHidden && wasHidden) {
					notifyItemInserted(position);
				}

				if(!isHidden) {
					position++;
				}
			}
		}
	}

	public void notifyItemChanged(final int groupId, final Item<?> item) {
		final int position = getItemPositionInternal(groupId, item);
		notifyItemChanged(position);
	}
}
