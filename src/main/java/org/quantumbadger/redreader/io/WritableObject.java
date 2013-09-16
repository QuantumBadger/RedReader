package org.quantumbadger.redreader.io;

public interface WritableObject<K> {
	public K getKey();
	public long getTimestamp();
}
