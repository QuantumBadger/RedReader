package org.quantumbadger.redreader.common.collections;

import java.lang.ref.WeakReference;
import java.util.Iterator;
import java.util.LinkedList;

public final class WeakReferenceListManager<E> {

	// TODO avoid linked list here -- a new object is created for every Link<>
	private final LinkedList<WeakReference<E>> data = new LinkedList<WeakReference<E>>();

	public synchronized void add(final E object) {
		data.add(new WeakReference<E>(object));
	}

	public synchronized void map(final Operator<E> operator) {

		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			final E object = iterator.next().get();

			if(object == null) {
				iterator.remove();
			} else {
				operator.operate(object);
			}
		}

	}

	public synchronized void remove(final E object) {
		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			if(iterator.next().get() == object) iterator.remove();
		}
	}

	public static interface Operator<E> {
		public void operate(E object);
	}
}
