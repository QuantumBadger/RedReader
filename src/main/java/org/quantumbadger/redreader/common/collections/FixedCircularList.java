package org.quantumbadger.redreader.common.collections;

public final class FixedCircularList<E> {

	private final E[] data;
	private int pos;
	public final int size;

	public FixedCircularList(final int size) {
		//noinspection unchecked
		data = (E[]) new Object[size];
		this.size = size;
	}

	public E getRelative(final int relativeIndex) {
		return data[mod(pos + relativeIndex, data.length)];
	}

	public void moveRelative(final int relativeIndex) {
		pos = mod(pos + relativeIndex, data.length);
	}

	private static int mod(final int a, final int b) {
		return (a % b + b) % b;
	}

	public void setRelative(final int relativeIndex, final E object) {
		data[mod(pos + relativeIndex, data.length)] = object;
	}
}
