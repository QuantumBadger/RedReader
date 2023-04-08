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

package org.quantumbadger.redreader.reddit.prepared;

import android.content.Context;
import android.util.Log;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.collections.WeakReferenceListHashMapManager;
import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimeStringsDebug;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.io.ExtendedDataInputStream;
import org.quantumbadger.redreader.io.ExtendedDataOutputStream;
import org.quantumbadger.redreader.io.RedditChangeDataIO;
import org.quantumbadger.redreader.reddit.kthings.RedditComment;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;

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

	private static final int MAX_ENTRY_COUNT = 10_000;

	private static final HashMap<RedditAccount, RedditChangeDataManager> INSTANCE_MAP
			= new HashMap<>();

	@NonNull
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

	private static HashMap<RedditAccount, HashMap<RedditIdAndType, Entry>> snapshotAllUsers() {

		final HashMap<RedditAccount, HashMap<RedditIdAndType, Entry>> result = new HashMap<>();

		synchronized(INSTANCE_MAP) {
			for(final RedditAccount account : INSTANCE_MAP.keySet()) {
				result.put(account, getInstance(account).snapshot());
			}
		}

		return result;
	}

	public static void writeAllUsers(final ExtendedDataOutputStream dos) throws IOException {

		Log.i(TAG, "Taking snapshot...");

		final HashMap<RedditAccount, HashMap<RedditIdAndType, Entry>> data = snapshotAllUsers();

		Log.i(TAG, "Writing to stream...");

		final Set<Map.Entry<RedditAccount, HashMap<RedditIdAndType, Entry>>> userDataSet =
				data.entrySet();

		dos.writeInt(userDataSet.size());

		for(final Map.Entry<RedditAccount, HashMap<RedditIdAndType, Entry>> userData
				: userDataSet) {

			final String username = userData.getKey().getCanonicalUsername();
			dos.writeUTF(username);

			final Set<Map.Entry<RedditIdAndType, Entry>> entrySet = userData.getValue().entrySet();

			dos.writeInt(entrySet.size());

			for(final Map.Entry<RedditIdAndType, Entry> entry : entrySet) {
				dos.writeUTF(entry.getKey().getValue());
				entry.getValue().writeTo(dos);
			}

			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(
						TAG,
						String.format(
								Locale.US,
								"Wrote %d entries for user '%s'",
								entrySet.size(),
								username));
			}
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

			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(
						TAG,
						String.format(
								Locale.US,
								"Reading %d entries for user '%s'",
								entryCount,
								username));
			}

			final HashMap<RedditIdAndType, Entry> entries = new HashMap<>(entryCount);

			for(int j = 0; j < entryCount; j++) {
				final RedditIdAndType thingId = new RedditIdAndType(dis.readUTF());
				final Entry entry = new Entry(dis);
				entries.put(thingId, entry);
			}

			Log.i(TAG, "Getting account...");

			final RedditAccount account =
					RedditAccountManager.getInstance(context).getAccount(username);

			if(account == null) {
				if(General.isSensitiveDebugLoggingEnabled()) {
					Log.i(
							TAG,
							String.format(
									Locale.US,
									"Skipping user '%s' as the account no longer exists",
									username));
				}

			} else {
				getInstance(account).insertAll(entries);
				if(General.isSensitiveDebugLoggingEnabled()) {
					Log.i(
							TAG,
							String.format(
									Locale.US,
									"Finished inserting entries for user '%s'",
									username));
				}
			}
		}

		Log.i(TAG, "All entries read from stream.");
	}

	public static void pruneAllUsersDefaultMaxAge() {
		pruneAllUsersWhereOlderThan(PrefsUtility.pref_cache_maxage_entry());
	}

	public static void pruneAllUsersWhereOlderThan(final TimeDuration maxAge) {

		Log.i(TAG, "Pruning for all users...");

		final Set<RedditAccount> users;

		synchronized(INSTANCE_MAP) {
			users = new HashSet<>(INSTANCE_MAP.keySet());
		}

		for(final RedditAccount user : users) {

			final RedditChangeDataManager managerForUser = getInstance(user);
			managerForUser.prune(maxAge);
		}

		Log.i(TAG, "Pruning complete.");
	}

	public interface Listener {
		void onRedditDataChange(final RedditIdAndType thingIdAndType);
	}

	private static final class Entry {

		private final TimestampUTC mTimestamp;

		private final boolean mIsUpvoted;
		private final boolean mIsDownvoted;
		private final boolean mIsRead;
		private final boolean mIsSaved;
		private final Boolean mIsHidden;
		// For posts, this means "hidden". For comments, this means "collapsed".

		static final Entry CLEAR_ENTRY = new Entry();

		private Entry() {
			mTimestamp = TimestampUTC.ZERO;
			mIsUpvoted = false;
			mIsDownvoted = false;
			mIsRead = false;
			mIsSaved = false;
			mIsHidden = null;
		}

		private Entry(
				final TimestampUTC timestamp,
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
			mTimestamp = TimestampUTC.fromUtcMs(dis.readLong());
			mIsUpvoted = dis.readBoolean();
			mIsDownvoted = dis.readBoolean();
			mIsRead = dis.readBoolean();
			mIsSaved = dis.readBoolean();
			mIsHidden = dis.readNullableBoolean();
		}

		private void writeTo(final ExtendedDataOutputStream dos) throws IOException {
			dos.writeLong(mTimestamp.toUtcMs());
			dos.writeBoolean(mIsUpvoted);
			dos.writeBoolean(mIsDownvoted);
			dos.writeBoolean(mIsRead);
			dos.writeBoolean(mIsSaved);
			dos.writeNullableBoolean(mIsHidden);
		}

		boolean isClear() {
			return !mIsUpvoted
					&& !mIsDownvoted
					&& !mIsRead
					&& !mIsSaved
					&& mIsHidden == null;
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
				final TimestampUTC timestamp,
				final RedditComment comment) {

			if(timestamp.isLessThan(mTimestamp)) {
				return this;
			}

			return new Entry(
					timestamp,
					Boolean.TRUE.equals(comment.getLikes()),
					Boolean.FALSE.equals(comment.getLikes()),
					false,
					comment.getSaved(),
					mIsHidden); // Use existing value for "collapsed"
		}

		Entry update(
				final TimestampUTC timestamp,
				final RedditPost post) {

			if(timestamp.isLessThan(mTimestamp)) {
				return this;
			}

			return new Entry(
					timestamp,
					Boolean.TRUE.equals(post.getLikes()),
					Boolean.FALSE.equals(post.getLikes()),
					post.getClicked() || mIsRead,
					post.getSaved(),
					post.getHidden() ? true : null);
		}

		Entry markUpvoted(final TimestampUTC timestamp) {

			return new Entry(
					timestamp,
					true,
					false,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markDownvoted(final TimestampUTC timestamp) {

			return new Entry(
					timestamp,
					false,
					true,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markUnvoted(final TimestampUTC timestamp) {

			return new Entry(
					timestamp,
					false,
					false,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markRead(final TimestampUTC timestamp) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					mIsDownvoted,
					true,
					mIsSaved,
					mIsHidden);
		}

		Entry markSaved(final TimestampUTC timestamp, final boolean isSaved) {

			return new Entry(
					timestamp,
					mIsUpvoted,
					mIsDownvoted,
					mIsRead,
					isSaved,
					mIsHidden);
		}

		Entry markHidden(final TimestampUTC timestamp, final Boolean isHidden) {

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
			implements WeakReferenceListManager.ArgOperator<Listener, RedditIdAndType> {

		public static final ListenerNotifyOperator INSTANCE =
				new ListenerNotifyOperator();

		@Override
		public void operate(final Listener listener, final RedditIdAndType arg) {
			listener.onRedditDataChange(arg);
		}
	}

	private final HashMap<RedditIdAndType, Entry> mEntries = new HashMap<>();
	private final Object mLock = new Object();

	private final WeakReferenceListHashMapManager<RedditIdAndType, Listener> mListeners =
			new WeakReferenceListHashMapManager<>();

	public void addListener(
			final RedditIdAndType thing,
			final Listener listener) {

		mListeners.add(thing, listener);
	}

	public void removeListener(
			final RedditIdAndType thing,
			final Listener listener) {

		mListeners.remove(thing, listener);
	}

	private Entry get(final RedditIdAndType thing) {

		final Entry entry = mEntries.get(thing);

		if(entry == null) {
			return Entry.CLEAR_ENTRY;
		} else {
			return entry;
		}
	}

	private void set(
			final RedditIdAndType thing,
			final Entry existingValue,
			final Entry newValue) {

		if(newValue.isClear()) {
			if(!existingValue.isClear()) {
				mEntries.remove(thing);
				RedditChangeDataIO.notifyUpdateStatic();
			}

		} else {
			mEntries.put(thing, newValue);
			RedditChangeDataIO.notifyUpdateStatic();
		}

		AndroidCommon.UI_THREAD_HANDLER.post(() -> mListeners.map(
				thing,
				ListenerNotifyOperator.INSTANCE,
				thing));
	}

	private void insertAll(final HashMap<RedditIdAndType, Entry> entries) {

		synchronized(mLock) {

			for(final Map.Entry<RedditIdAndType, Entry> entry : entries.entrySet()) {

				final Entry newEntry = entry.getValue();
				final Entry existingEntry = mEntries.get(entry.getKey());

				if(existingEntry == null
						|| existingEntry.mTimestamp.isLessThan(newEntry.mTimestamp)) {

					mEntries.put(entry.getKey(), newEntry);
				}
			}
		}

		for(final RedditIdAndType idAndType : entries.keySet()) {
			mListeners.map(idAndType, ListenerNotifyOperator.INSTANCE, idAndType);
		}
	}

	public void update(final TimestampUTC timestamp, final RedditComment comment) {

		synchronized(mLock) {
			final Entry existingEntry = get(comment.getIdAndType());
			final Entry updatedEntry = existingEntry.update(timestamp, comment);
			set(comment.getIdAndType(), existingEntry, updatedEntry);
		}
	}

	public void update(final TimestampUTC timestamp, final RedditPost post) {

		synchronized(mLock) {
			final Entry existingEntry = get(post.getIdAndType());
			final Entry updatedEntry = existingEntry.update(timestamp, post);
			set(post.getIdAndType(), existingEntry, updatedEntry);
		}
	}

	public void markUpvoted(final TimestampUTC timestamp, final RedditIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUpvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markDownvoted(
			final TimestampUTC timestamp,
			final RedditIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markDownvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markUnvoted(final TimestampUTC timestamp, final RedditIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUnvoted(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markSaved(
			final TimestampUTC timestamp,
			final RedditIdAndType thing,
			final boolean saved) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markSaved(timestamp, saved);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markHidden(
			final TimestampUTC timestamp,
			final RedditIdAndType thing,
			final Boolean hidden) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markHidden(timestamp, hidden);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public void markRead(final TimestampUTC timestamp, final RedditIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markRead(timestamp);
			set(thing, existingEntry, updatedEntry);
		}
	}

	public boolean isUpvoted(final RedditIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isUpvoted();
		}
	}

	public boolean isDownvoted(final RedditIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isDownvoted();
		}
	}

	public boolean isRead(final RedditIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isRead();
		}
	}

	public boolean isSaved(final RedditIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isSaved();
		}
	}

	public Boolean isHidden(final RedditIdAndType thing) {
		synchronized(mLock) {
			return get(thing).isHidden();
		}
	}

	private HashMap<RedditIdAndType, Entry> snapshot() {
		synchronized(mLock) {
			return new HashMap<>(mEntries);
		}
	}

	private void prune(final TimeDuration maxAge) {

		final TimestampUTC now = TimestampUTC.now();
		final TimestampUTC timestampBoundary = now.subtract(maxAge);

		synchronized(mLock) {
			final Iterator<Map.Entry<RedditIdAndType, Entry>> iterator =
					mEntries.entrySet().iterator();
			final SortedMap<TimestampUTC, RedditIdAndType> byTimestamp = new TreeMap<>();

			while(iterator.hasNext()) {

				final Map.Entry<RedditIdAndType, Entry> entry = iterator.next();
				final TimestampUTC timestamp = entry.getValue().mTimestamp;
				byTimestamp.put(timestamp, entry.getKey());

				if(timestamp.isLessThan(timestampBoundary)) {

					Log.i(TAG, String.format(
							"Pruning '%s' (%s old)",
							entry.getKey(),
							now.elapsedPeriodSince(timestamp).format(
									TimeStringsDebug.INSTANCE,
									2
							)));

					iterator.remove();
				}
			}

			// Limit total number of entries to limit our memory usage. This is meant as a
			// safeguard, as the time-based pruning above should have removed enough already.
			final Iterator<Map.Entry<TimestampUTC, RedditIdAndType>> iter2 =
					byTimestamp.entrySet().iterator();
			while(iter2.hasNext()) {
				if(mEntries.size() <= MAX_ENTRY_COUNT) {
					break;
				}

				final Map.Entry<TimestampUTC, RedditIdAndType> entry = iter2.next();

				Log.i(TAG, String.format(
						"Evicting '%s' (%s old)",
						entry.getValue(),
						now.elapsedPeriodSince(entry.getKey()).format(
								TimeStringsDebug.INSTANCE,
								2
						)));

				mEntries.remove(entry.getValue());
			}
		}
	}
}
