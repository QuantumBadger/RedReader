package org.quantumbadger.redreader.adapters

import android.database.DataSetObserver
import android.view.View
import android.view.ViewGroup
import android.widget.Filter
import android.widget.Filterable
import android.widget.ListAdapter

class NoFilterAdapter (
	private val adapter: ListAdapter,
	private val allValues: List<String>,
) : ListAdapter, Filterable {

	override fun registerDataSetObserver(observer: DataSetObserver?) {
		adapter.registerDataSetObserver(observer)
	}

	override fun unregisterDataSetObserver(observer: DataSetObserver?) {
		adapter.unregisterDataSetObserver(observer)
	}

	override fun getCount() = adapter.count

	override fun getItem(position: Int) = adapter.getItem(position)

	override fun getItemId(position: Int) = adapter.getItemId(position)

	override fun hasStableIds() = adapter.hasStableIds()

	override fun getView(position: Int, convertView: View?, parent: ViewGroup?) =
		adapter.getView(position, convertView, parent)

	override fun getItemViewType(position: Int) = adapter.getItemViewType(position)

	override fun getViewTypeCount() = adapter.viewTypeCount

	override fun isEmpty() = adapter.isEmpty

	override fun areAllItemsEnabled() = adapter.areAllItemsEnabled()

	override fun isEnabled(position: Int) = adapter.isEnabled(position)

	// The important bit
	override fun getFilter() = object : Filter() {

		override fun performFiltering(constraint: CharSequence?) = FilterResults().apply {
			values = ArrayList(allValues);
			count = allValues.size
		}

		override fun publishResults(constraint: CharSequence?, results: FilterResults?) {
			// Nothing to do here
		}
	}
}
