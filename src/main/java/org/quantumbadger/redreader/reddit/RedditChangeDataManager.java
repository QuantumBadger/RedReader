package org.quantumbadger.redreader.reddit;

import org.quantumbadger.redreader.common.collections.UniqueSynchronizedQueue;

import java.util.HashMap;

public final class RedditChangeDataManager {

	private final HashMap<String, TimestampedInt> changes = new HashMap<String, TimestampedInt>();
	private final UniqueSynchronizedQueue<String> dirtyChanges = new UniqueSynchronizedQueue<String>();

	private String getChangeId(String thing_id, String property_id) {
		return thing_id + ":" + property_id;
	}

	private TimestampedInt updateAndGetProperty(String change_id, int value, long timestamp) {
		final TimestampedInt currentValue = changes.get(change_id);
		if(currentValue.lastUpdate >= timestamp) return currentValue;
		final TimestampedInt newValue = new TimestampedInt(timestamp, value);
		changes.put(change_id, newValue);
		return newValue;
	}

	private void loadFromDb() {

	}

	private synchronized void writeToDisk() {

	}


	private class TimestampedInt {
		public final long lastUpdate;
		public final int value;

		private TimestampedInt(long lastUpdate, int value) {
			this.lastUpdate = lastUpdate;
			this.value = value;
		}
	}
}
