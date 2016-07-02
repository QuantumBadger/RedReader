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

import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.collections.WeakReferenceListHashMapManager;
import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType;

import java.util.HashMap;

public final class RedditChangeDataManagerVolatile {

	private static final HashMap<RedditAccount, RedditChangeDataManagerVolatile> INSTANCE_MAP
			= new HashMap<>();

	public static synchronized RedditChangeDataManagerVolatile getInstance(final RedditAccount user) {

		RedditChangeDataManagerVolatile result = INSTANCE_MAP.get(user);

		if(result == null) {
			result = new RedditChangeDataManagerVolatile();
			INSTANCE_MAP.put(user, result);
		}

		return result;
	}

	public static synchronized HashMap<RedditAccount, HashMap<String, Entry>> snapshotAllUsers() {

		final HashMap<RedditAccount, HashMap<String, Entry>> result = new HashMap<>();

		for(final RedditAccount account : INSTANCE_MAP.keySet()) {
			result.put(account, getInstance(account).snapshot());
		}

		return result;
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
					Boolean.FALSE.equals(comment.likes),
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
					Boolean.FALSE.equals(post.likes),
					post.clicked || mIsRead,
					post.saved,
					post.hidden ? true : null);
		}

		Entry markUpvoted(final long timestamp) {

			return new Entry(
					timestamp,
					true,
					false,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markDownvoted(final long timestamp) {

			return new Entry(
					timestamp,
					false,
					true,
					mIsRead,
					mIsSaved,
					mIsHidden);
		}

		Entry markUnvoted(final long timestamp) {

			return new Entry(
					timestamp,
					false,
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
			}

		} else {
			mEntries.put(thing.getIdAndType(), newValue);
		}

		mListeners.map(thing.getIdAndType(), ListenerNotifyOperator.INSTANCE, thing.getIdAndType());
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

	public void markUnvoted(final long timestamp, final RedditThingWithIdAndType thing) {

		synchronized(mLock) {
			final Entry existingEntry = get(thing);
			final Entry updatedEntry = existingEntry.markUnvoted(timestamp);
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

	public HashMap<String, Entry> snapshot() {
		synchronized(mLock) {
			return new HashMap<>(mEntries);
		}
	}
}
