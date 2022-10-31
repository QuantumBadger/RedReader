/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.saiditnet.redreader.reddit.prepared;

import android.content.Context;
import android.preference.PreferenceManager;
import android.util.Log;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.collections.WeakReferenceListHashMapManager;
import org.saiditnet.redreader.common.collections.WeakReferenceListManager;
import org.saiditnet.redreader.io.ExtendedDataInputStream;
import org.saiditnet.redreader.io.ExtendedDataOutputStream;
import org.saiditnet.redreader.io.RedditChangeDataIO;
import org.saiditnet.redreader.reddit.things.RedditComment;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.things.RedditThingWithIdAndType;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

public final class RedditChangeDataManager {

	private static final String TAG = "RedditChangeDataManager";

	private static final int MAX_ENTRY_COUNT = 10000;

	private static final HashMap<RedditAccount, RedditChangeDataManager> INSTANCE_MAP
			= new HashMap<>();

	public static RedditChangeDataManager getInstance(final RedditAccount user) {

		synchronized(INSTANCE_MAP) {

			RedditChangeDataManager result = INSTANCE_MAP.get(user);

			if(result == null) {
				result = new RedditChangeDataManager();
				INSTANCE_MAP.put(user, result);
			}

			return result;
		}
	}

	private static HashMap<RedditAccount, HashMap<String, Entry>> snapshotAllUsers() {

		final HashMap<RedditAccount, HashMap<String, Entry>> result = new HashMap<>();

		synchronized(INSTANCE_MAP) {
			for(final RedditAccount account : INSTANCE_MAP.keySet()) {
				result.put(account, getInstance(account).snapshot());
			}
		}

		return result;
	}

	public static void writeAllUsers(final ExtendedDataOutputStream dos) throws IOException {

		Log.i(TAG, "Taking snapshot...");

		final HashMap<RedditAccount, HashMap<String, Entry>> data = snapshotAllUsers();

		Log.i(TAG, "Writing to stream...");

		final Set<Map.Entry<RedditAccount, HashMap<String, Entry>>> userDataSet = data.entrySet();

		dos.writeInt(userDataSet.size());

		for(final Map.Entry<RedditAccount, HashMap<String, Entry>> userData : userDataSet) {

			final String username = userData.getKey().getCanonicalUsername();
			dos.writeUTF(username);

			final Set<Map.Entry<String, Entry>> entrySet = userData.getValue().entrySet();

			dos.writeInt(entrySet.size());

			for(final Map.Entry<String, Entry> entry : entrySet) {
				dos.writeUTF(entry.getKey());
				entry.getValue().writeTo(dos);
			}

			Log.i(TAG, String.format(Locale.US, "Wrote %d entries for user '%s'", entrySet.size(), username));
		}

		Log.i(TAG, "All entries written to stream.");
	}

	public static void readAllUsers(
			final ExtendedDataInputStream dis,
			final Context context) throws IOException {

		Log.i(TAG, "Reading from stream...");

		final int userCount = dis.readInt();

		Log.i(TAG, userCount + " users to read.");

		for(int i = 0; i < userCount; i++) {

			final String username = dis.readUTF();
			final int entryCount = dis.readInt();

			Log.i(TAG, String.format(Locale.US, "Reading %d entries for user '%s'", entryCount, username));

			final HashMap<String, Entry> entries = new HashMap<>(entryCount);

			for(int j = 0; j < entryCount; j++) {
				final String thingId = dis.readUTF();
				final Entry entry = new Entry(dis);
				entries.put(thingId, entry);
			}

			Log.i(TAG, "Getting account...");

			final RedditAccount account = RedditAccountManager.getInstance(context).getAccount(username);

			if(account == null) {
				Log.i(TAG, String.format(Locale.US, "Skipping user '%s' as the account no longer exists", username));

			} else {
				getInstance(account).insertAll(entries);
				Log.i(TAG, String.format(Locale.US, "Finished inserting entries for user '%s'", username));
			}
		}

		Log.i(TAG, "All entries read from stream.");
	}

	public static void pruneAllUsers(final Context context) {

		Log.i(TAG, "Pruning for all users...");

		final Set<RedditAccount> users;

		synchronized(INSTANCE_MAP) {
			users = new HashSet<>(INSTANCE_MAP.keySet());
		}

		for(final RedditAccount user : users) {

			final RedditChangeDataManager managerForUser = getInstance(user);
			managerForUser.prune(context);
		}

		Log.i(TAG, "Pruning complete.");
	}

	public interface Listener {
		void onRedditDataChange(final String thingIdAndType);
	}

	private static final class Entry {

		private final long mTimestamp;

		private final boolean mIsUpvoted;
		private final boolean mIsDownvoted;
		private final boolean mIsRead;
		private final boolean mIsSaved;
		private final Boolean mIsHidden; // For posts, this means "hidden". For comments, this means "collapsed".

		static final Entry CLEAR_ENTRY = new Entry();

		private Entry() {
			mTimestamp = Long.MIN_VALUE;
			mIsUpvoted = false;
			mIsDownvoted = false;
			mIsRead = false;
			mIsSaved = false;
			mIsHidden = null;
		}

		private Entry(
				final long timestamp,
				final boolean isUpvoted,
				final boolean isDownvoted,
				final boolean isRead,
				final boolean isSaved,
				final Boolean isHidden) {

			mTimestamp = timestamp;
			mIsUpvoted = isUpvoted;
			mIsDownvoted = isDownvoted;
			mIsRead = isRead;
			mIsSaved = isSaved;
			mIsHidden = isHidden;
		}

		private Entry(final ExtendedDataInputStream dis) throws IOException {
			mTimestamp = dis.readLong();
			mIsUpvoted = dis.readBoolean();
			mIsDownvoted = dis.readBoolean();
			mIsRead = dis.readBoolean();
			mIsSaved = dis.readBoolean();
			mIsHidden = dis.readNullableBoolean();
		}

		private void writeTo(final ExtendedDataOutputStream dos) throws IOException {
			dos.writeLong(mTimestamp);
			dos.writeBoolean(mIsUpvoted);
			dos.writeBoolean(mIsDownvoted);
			dos.writeBoolean(mIsRead);
			dos.writeBoolean(mIsSaved);
			dos.writeNullableBoolean(mIsHidden);
		}

		boolean isClear() {
			return !mIsUpvoted && !mIsDownvoted && !mIsRead && !mIsSaved && mIsHidden == null;
		}

		public boolean isUpvoted() {
			return mIsUpvoted;
		}

		public boolean isSaved() {
			return mIsSaved;
		}

		public boolean isRead() {
			return mIsRead;
		}

		public Boolean isHidden() {
			return mIsHidden;
		}

		public boolean isDownvoted() {
			return mIsDownvoted;
		}

		Entry update(
				final long timestamp,
				final RedditComment comment) {

			if(timestamp < mTimestamp) {
				return this;
			}

			return new Entry(
					timestamp,
					Boolean.TRUE.equals(comment.likes),
					Boolean.TRUE.equals(comment.dislikes),
					false,
					Boolean.TRUE.equals(comment.saved),
					mIsHidden); // Use existing value for "collapsed"
		}

		Entry update(
				final long timestamp,
				final RedditPost post) {

			if(timestamp < mTimestamp) {
				return this;
			}

			return new Entry(
					timestamp,
					Boolean.TRUE.equals(post.likes),
					Boolean.TRUE.equals(post.dislikes),
					post.clicked || mIsRead,
					post.saved,
					post.hidden ? true : null);
		}

		Entry markUpvoted(final long timestamp) {

			return new Entry(
					timestamp,
					true,
					mIsDownvoted,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markDownvoted(final long timestamp) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					true,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		// Entry markUnvoted(final long timestamp) {

		// 	return new Entry(
		// 			timestamp,
		// 			false,
		// 			false,
		// 			mIsRead,
		// 			mIsSaved,
		// 			mIsHidden);
		// }

		Entry markUnUpvoted(final long timestamp) {

			return new Entry(
					timestamp,
					false,
					mIsDownvoted,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markUnDownvoted(final long timestamp) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					false,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markRead(final long timestamp) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					mIsDownvoted,
					true,
					mIsSaved,
					mIsHidden);
		}

		Entry markSaved(final long timestamp, final boolean isSaved) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					mIsDownvoted,
					mIsRead,
					isSaved,
					mIsHidden);
		}

		Entry markHidden(final long timestamp, final Boolean isHidden) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					mIsDownvoted,
					mIsRead,
					mIsSaved,
					isHidden);
		}
	}

	private static final class ListenerNotifyOperator
			implements WeakReferenceListManager.ArgOperator<Listener, String> {

		public static final ListenerNotifyOperator INSTANCE = new ListenerNotifyOperator();

		private ListenerNotifyOperator() {}

		@Override
		public void operate(final Listener listener, final String arg) {
			listener.onRedditDataChange(arg);
		}
	}

	private final HashMap<String, Entry> mEntries = new HashMap<>();
	private final Object mLock = new Object();

	private final WeakReferenceListHashMapManager<String, Listener> mListeners = new WeakReferenceListHashMapManager<>();

	public void addListener(
			final RedditThingWithIdAndType thing,
			final Listener listener) {

		mListeners.add(thing.getIdAndType(), listener);
	}

	public void removeListener(
			final RedditThingWithIdAndType thing,
			final Listener listener) {

		mListeners.remove(thing.getIdAndType(), listener);
	}

	private Entry get(final RedditThingWithIdAndType thing) {

		final Entry entry = mEntries.get(thing.getIdAndType());

		if(entry == null) {
			return Entry.CLEAR_ENTRY;
		} else {
			return entry;
		}
	}

	private void set(
			final RedditThingWithIdAndType thing,
			final Entry existingValue,
			final Entry newValue) {

		if(newValue.isClear()) {
			if(!existingValue.isClear()) {
				mEntries.remove(thing.getIdAndType());
				RedditChangeDataIO.notifyUpdateStatic();
			}

		} else {
			mEntries.put(thing.getIdAndType(), newValue);
			RedditChangeDataIO.notifyUpdateStatic();
		}

		mListeners.map(thing.getIdAndType(), ListenerNotifyOperator.INSTANCE, thing.getIdAndType());
	}

	private void insertAll(final HashMap<String, Entry> entries) {

		synchronized(mLock) {

			for(final Map.Entry<String, Entry> entry : entries.entrySet()) {

				final Entry newEntry = entry.getValue();
				final Entry existingEntry = mEntries.get(entry.getKey());

				if(existingEntry == null
						|| existingEntry.mTimestamp < newEntry.mTimestamp) {

					mEntries.put(entry.getKey(), newEntry);
				}
			}
		}

		for(final String idAndType : entries.keySet()) {
			mListeners.map(idAndType, ListenerNotifyOperator.INSTANCE, idAndType);
		}
	}

	public void update(final long timestamp, final RedditComment comment) {

		synchronized(mLock) {
			final Entry existingEntry = get(comment);
			final Entry updatedEntry = existingEntry.update(timestamp, comment);
			set(comment, existingEntry, updatedEntry);
		}
	}

	public void update(final long timestamp, final RedditPost post) {

		synchronized(mLock) {
			final Entry existingEntry = get(post);
			final Entry updatedEntry = existingEntry.update(timestamp, post);
			set(post, existingEntry, updatedEntry);
		}
	}

	public void markUpvoted(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUpvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markDownvoted(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markDownvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	// public void markUnvoted(final long timestamp, final RedditThingWithIdAndType thing) {

	// 	synchronized(mLock) {
	// 		final Entry existingEntry = get(thing);
	// 		final Entry updatedEntry = existingEntry.markUnvoted(timestamp);
	// 		set(thing, existingEntry, updatedEntry);
	// 	}
	// }

	public void markUnUpvoted(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUnUpvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markUnDownvoted(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUnDownvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markSaved(final long timestamp, final RedditThingWithIdAndType thing, final boolean saved) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markSaved(timestamp, saved);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markHidden(final long timestamp, final RedditThingWithIdAndType thing, final Boolean hidden) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markHidden(timestamp, hidden);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markRead(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markRead(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public boolean isUpvoted(final RedditThingWithIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isUpvoted();
		}
	}

	public boolean isDownvoted(final RedditThingWithIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isDownvoted();
		}
	}

	public boolean isRead(final RedditThingWithIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isRead();
		}
	}

	public boolean isSaved(final RedditThingWithIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isSaved();
		}
	}

	public Boolean isHidden(final RedditThingWithIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isHidden();
		}
	}

	private HashMap<String, Entry> snapshot() {
		synchronized(mLock) {
			return new HashMap<>(mEntries);
		}
	}

	private void prune(final Context context) {

		final long now = System.currentTimeMillis();
		final long timestampBoundary = now - PrefsUtility.pref_cache_maxage_entry(
				context, PreferenceManager.getDefaultSharedPreferences(context));

		synchronized(mLock) {
			final Iterator<Map.Entry<String, Entry>> iterator = mEntries.entrySet().iterator();
			final SortedMap<Long, String> byTimestamp = new TreeMap<Long, String>();

			while(iterator.hasNext()) {

				final Map.Entry<String, Entry> entry = iterator.next();
				final long timestamp = entry.getValue().mTimestamp;
				byTimestamp.put(timestamp, entry.getKey());

				if(timestamp < timestampBoundary) {

					Log.i(TAG, String.format(
							"Pruning '%s' (%d hours old)",
							entry.getKey(),
							(now - timestamp) / (60L * 60L * 1000L)));

					iterator.remove();
				}
			}

			// Limit total number of entries to limit our memory usage. This is meant as a
			// safeguard, as the time-based pruning above should have removed enough already.
			final Iterator<Map.Entry<Long, String>> iter2 = byTimestamp.entrySet().iterator();
			while(iter2.hasNext()) {
				if(mEntries.size() <= MAX_ENTRY_COUNT) {
					break;
				}

				final Map.Entry<Long, String> entry = iter2.next();

				Log.i(TAG, String.format(
						"Evicting '%s' (%d hours old)",
						entry.getValue(),
						(now - entry.getKey()) / (60L * 60L * 1000L)));

				mEntries.remove(entry.getValue());
			}
		}
	}
}
