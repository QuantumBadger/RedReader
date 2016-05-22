package org.quantumbadger.redreader.adapters;

import android.support.v7.widget.RecyclerView;
import android.view.ViewGroup;

/**
 * Created by veyndan on 18/04/2016.
 */
public abstract class HeaderRecyclerAdapter<VH extends RecyclerView.ViewHolder> extends RecyclerView.Adapter<VH> {

	private static final int TYPE_HEADER = 0;
	private static final int TYPE_CONTENT = 1;

	protected static final int HEADER_SIZE = 1;

	@Override
	public VH onCreateViewHolder(ViewGroup parent, int viewType) {
		switch (viewType) {
			case TYPE_HEADER:
				return onCreateHeaderItemViewHolder(parent);
			case TYPE_CONTENT:
				return onCreateContentItemViewHolder(parent);
			default:
				throw new IllegalStateException();
		}
	}

	protected abstract VH onCreateHeaderItemViewHolder(ViewGroup parent);

	protected abstract VH onCreateContentItemViewHolder(ViewGroup parent);

	@Override
	public void onBindViewHolder(VH holder, int position) {
		if (position == 0) {
			onBindHeaderItemViewHolder(holder, position);
		} else {
			onBindContentItemViewHolder(holder, position - HEADER_SIZE);
		}
	}

	protected abstract void onBindHeaderItemViewHolder(VH holder, int position);

	protected abstract void onBindContentItemViewHolder(VH holder, int position);

	@Override
	public int getItemCount() {
		return getContentItemCount() + HEADER_SIZE;
	}

	@Override
	public int getItemViewType(int position) {
		return position == 0 ? TYPE_HEADER : TYPE_CONTENT;
	}

	protected abstract int getContentItemCount();
}
