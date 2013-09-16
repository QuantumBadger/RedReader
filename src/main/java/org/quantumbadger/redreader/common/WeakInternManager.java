package org.quantumbadger.redreader.common;

import java.lang.ref.WeakReference;
import java.util.HashMap;

public class WeakInternManager<E> {

	private final HashMap<String, WeakReference<E>> interned = new HashMap<String, WeakReference<E>>();
	private final Interner<E> interner;

	public final Interner<E> DO_NOTHING_INTERNER = new Interner<E>() {
		public E doIntern(String id, E obj) {
			return obj;
		}
	};

	public WeakInternManager(Interner<E> interner) {
		this.interner = interner;
	}

	public final synchronized E intern(final String id, final E obj) {

		final WeakReference<E> existingRef = interned.get(id);
		if(existingRef != null) {
			final E existing = existingRef.get();
			if(existing != null) return existing;
		}

		final E result = interner.doIntern(id, obj);
		interned.put(id, new WeakReference<E>(result));
		return result;
	}

	public static interface Interner<E> {
		public E doIntern(String id, E obj);
	}
}
