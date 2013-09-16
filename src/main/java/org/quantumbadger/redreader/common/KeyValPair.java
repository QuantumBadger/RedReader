package org.quantumbadger.redreader.common;

public class KeyValPair<K, V> {
	public final K key;
	public final V value;

	public KeyValPair(K key, V value) {
		this.key = key;
		this.value = value;
	}
}
