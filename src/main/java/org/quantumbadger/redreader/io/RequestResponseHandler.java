package org.quantumbadger.redreader.io;

public interface RequestResponseHandler<E, F> {
	void onRequestFailed(F failureReason);

	void onRequestSuccess(E result, long timeCached);
}
