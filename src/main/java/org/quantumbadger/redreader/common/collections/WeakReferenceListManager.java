package org.quantumbadger.redreader.common.collections;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

import static java.util.Collections.synchronizedMap;

public final class WeakReferenceListManager<E> {
	private final Map<WeakReference<E>, Boolean> weakReferenceList = synchronizedMap(new WeakHashMap<WeakReference<E>, Boolean>());

	public void add(final E object) {
		weakReferenceList.put(new WeakReference<E>(object), true);
	}

	public <A> void map(final ArgOperator<E, A> operator, final A arg) {
		for (WeakReference<E> key : weakReferenceList.keySet()) {
			final E object = key.get();
			if (object != null) {
				operator.operate(object, arg);
			}
		}
	}

	public static interface Operator<E> {
		public void operate(E object);
	}

	public static interface ArgOperator<E, A> {
		public void operate(E object, A arg);
	}
}
