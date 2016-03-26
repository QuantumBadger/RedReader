package org.quantumbadger.redreader.common.collections;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class WeakReferenceListHashMapManager<K, V> {

	private final HashMap<K, WeakReferenceListManager<V>> mData = new HashMap<>();

	private byte mCleanupCounter = 0;

	public void add(final K key, final V value) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list == null) {
			list = new WeakReferenceListManager<>();
			mData.put(key, list);
		}

		list.add(value);

		// Perform cleanup once for each 256 values which are added
		if(++mCleanupCounter == 0) {
			clean();
		}
	}

	public void remove(final K key, final V value) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.remove(value);
		}
	}

	public void map(
			final K key,
			final WeakReferenceListManager.Operator<V> operator) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.map(operator);
		}
	}

	public <A> void map(
			final K key,
			final WeakReferenceListManager.ArgOperator<V, A> operator,
			final A arg) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.map(operator, arg);
		}
	}

	public void clean() {

		final Iterator<Map.Entry<K, WeakReferenceListManager<V>>> iterator = mData.entrySet().iterator();

		while(iterator.hasNext()) {

			final Map.Entry<K, WeakReferenceListManager<V>> entry = iterator.next();

			final WeakReferenceListManager<V> list = entry.getValue();
			list.clean();

			if(list.isEmpty()) {
				iterator.remove();
			}
		}
	}
}
