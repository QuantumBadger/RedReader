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

package org.quantumbadger.redreader.common;

import android.annotation.SuppressLint;
import android.content.SharedPreferences;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SharedPrefsWrapper {

	@NonNull private static final String TAG = "SharedPrefsWrapper";

	public class Editor {

		@NonNull private final SharedPreferences.Editor mEditor;

		@SuppressLint("CommitPrefEdits")
		private Editor() {
			mEditor = mPrefs.edit();
		}

		public Editor putString(
				@NonNull final String key,
				@Nullable final String value) {

			mEditor.putString(key, value);
			return this;
		}

		public Editor putInt(
				@NonNull final String key,
				final int value) {

			mEditor.putInt(key, value);
			return this;
		}

		public Editor putLong(
				@NonNull final String key,
				final long value) {

			mEditor.putLong(key, value);
			return this;
		}

		public Editor putBoolean(
				@NonNull final String key,
				final boolean value) {

			mEditor.putBoolean(key, value);
			return this;
		}

		public Editor putStringSet(
				@NonNull final String key,
				@Nullable final Set<String> value) {

			mEditor.putStringSet(key, value);
			return this;
		}

		public void apply() {
			// Take read lock as we aren't doing an atomic restore
			try(Locker ignored = new Locker(mRestoreLock.readLock())) {
				mEditor.apply();
			}
		}
	}

	public interface OnSharedPreferenceChangeListener {

		void onSharedPreferenceChanged(
				@NonNull SharedPrefsWrapper sharedPreferences,
				@NonNull String key);
	}

	@NonNull private final SharedPreferences mPrefs;

	@NonNull private final ReadWriteLock mRestoreLock = new ReentrantReadWriteLock();

	@NonNull private final HashMap<
			OnSharedPreferenceChangeListener,
			SharedPreferences.OnSharedPreferenceChangeListener> mListenerWrappers
					= new HashMap<>();

	SharedPrefsWrapper(@NonNull final SharedPreferences prefs) {
		mPrefs = prefs;
	}

	public void registerOnSharedPreferenceChangeListener(
			final OnSharedPreferenceChangeListener listener) {

		final SharedPreferences.OnSharedPreferenceChangeListener spListener
				= (sharedPreferences, key)
						-> listener.onSharedPreferenceChanged(this, key);

		mPrefs.registerOnSharedPreferenceChangeListener(spListener);

		mListenerWrappers.put(listener, spListener);
	}

	public void unregisterOnSharedPreferenceChangeListener(
			final OnSharedPreferenceChangeListener listener) {

		final SharedPreferences.OnSharedPreferenceChangeListener spListener
				= mListenerWrappers.remove(listener);

		if(spListener != null) {
		mPrefs.unregisterOnSharedPreferenceChangeListener(spListener);
		}
	}

	public boolean contains(@NonNull final String key) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.contains(key);
		}
	}

	@NonNull
	public Map<String, ?> getAllClone() {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return new HashMap<>(mPrefs.getAll());
		}
	}

	@Nullable
	public String getString(
			@NonNull final String key,
			@Nullable final String defValue) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.getString(key, defValue);
		}
	}

	public int getInt(
			@NonNull final String key,
			final int defValue) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.getInt(key, defValue);
		}
	}

	public long getLong(
			@NonNull final String key,
			final long defValue) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.getLong(key, defValue);
		}
	}

	@Nullable
	public Set<String> getStringSet(
			@NonNull final String key,
			@Nullable final Set<String> defValues) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.getStringSet(key, defValues);
		}
	}

	public boolean getBoolean(
			@NonNull final String key,
			final boolean defValue) {

		try(Locker ignored = new Locker(mRestoreLock.readLock())) {
			return mPrefs.getBoolean(key, defValue);
		}
	}

	@NonNull
	public Editor edit() {
		return new Editor();
	}

	void performActionWithWriteLock(@NonNull final Consumer<SharedPreferences> action) {

		Log.i(TAG, "Acquiring write lock");

		try(Locker ignored = new Locker(mRestoreLock.writeLock())) {

			Log.i(TAG, "Write lock acquired, performing action...");

			action.consume(mPrefs);
		}
	}

}
