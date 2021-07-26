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

import android.content.SharedPreferences;
import android.util.Log;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.BuildConfig;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.receivers.NewMessageChecker;
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class PrefsBackup {

	@NonNull private static final String TAG = "PrefsBackup";

	@NonNull private static final byte[] MAGIC_HEADER
			= "RedReader preferences backup\r\n".getBytes(General.CHARSET_UTF8);

	@NonNull private static final String FIELD_TYPE = "type";
	@NonNull private static final String FIELD_FILE_VERSION = "file_version";
	@NonNull private static final String FIELD_VERSION_CODE = "version_code";
	@NonNull private static final String FIELD_VERSION_NAME = "version_name";
	@NonNull private static final String FIELD_IS_ALPHA = "is_alpha";
	@NonNull private static final String FIELD_TIMESTAMP_UTC = "timestamp_utc";
	@NonNull private static final String FIELD_PREFS = "prefs";

	@NonNull private static final String FILE_TYPE = "redreader_prefs_backup";
	private static final int FILE_VERSION = 1;

	@NonNull private static final HashSet<String> IGNORED_PREFS = new HashSet<>();

	static {
		IGNORED_PREFS.add(AnnouncementDownloader.PREF_KEY_LAST_READ_ID);
		IGNORED_PREFS.add(AnnouncementDownloader.PREF_KEY_PAYLOAD_STORAGE_HEX);
		IGNORED_PREFS.add(NewMessageChecker.PREFS_SAVED_MESSAGE_ID);
		IGNORED_PREFS.add(NewMessageChecker.PREFS_SAVED_MESSAGE_TIMESTAMP);
		IGNORED_PREFS.add(FeatureFlagHandler.PREF_LAST_VERSION);
		IGNORED_PREFS.add(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN);
	}

	public interface BackupDestination {
		@NonNull OutputStream openOutputStream() throws IOException;
	}

	public interface BackupSource {
		@NonNull InputStream openInputStream() throws IOException;
	}

	private PrefsBackup() {}

	public static void backup(
			@NonNull final BaseActivity activity,
			@NonNull final BackupDestination destination,
			@NonNull final Runnable onSuccess) {

		final SharedPrefsWrapper prefs = General.getSharedPrefs(activity);

		new Thread(() -> {

			final HashMap<String, ?> prefMap = new HashMap<>(prefs.getAllClone());

			for(final String ignoredPref : IGNORED_PREFS) {
				prefMap.remove(ignoredPref);
			}

			final HashMap<String, Object> map = new HashMap<>();

			map.put(FIELD_TYPE, FILE_TYPE);
			map.put(FIELD_FILE_VERSION, FILE_VERSION);
			map.put(FIELD_VERSION_CODE, BuildConfig.VERSION_CODE);
			map.put(FIELD_VERSION_NAME, BuildConfig.VERSION_NAME);
			map.put(FIELD_IS_ALPHA, General.isAlpha());
			map.put(FIELD_TIMESTAMP_UTC, RRTime.utcCurrentTimeMillis());
			map.put(FIELD_PREFS, prefMap);

			final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

			try {
				final DataOutputStream dos = new DataOutputStream(bytes);
				dos.write(MAGIC_HEADER);
				SerializeUtils.serialize(dos, map);
				dos.flush();

			} catch(final SerializeUtils.UnhandledTypeException | IOException e) {
				BugReportActivity.handleGlobalError(activity, e);
				return;
			}

			try(OutputStream outputStream = destination.openOutputStream()) {

				outputStream.write(bytes.toByteArray());
				outputStream.flush();

			} catch(final IOException e) {

				General.showResultDialog(
						activity,
						new RRError(
								activity.getString(R.string.error_unexpected_storage_title),
								activity.getString(R.string.error_unexpected_storage_message),
								true,
								e));

				return;
			}

			onSuccess.run();

		}).start();
	}

	private static class MapReader {

		@NonNull private final Map<?, ?> mMap;

		private MapReader(@NonNull final Map<?, ?> map) {
			mMap = map;
		}

		@NonNull
		public Object getRequired(@NonNull final Object key) throws IOException {

			final Object result = mMap.get(key);

			if(result == null) {
				throw new IOException("Missing field: '" + key + "'");
			}

			return result;
		}

		@NonNull
		public String getRequiredString(@NonNull final Object key) throws IOException {

			final Object result = getRequired(key);

			if(!(result instanceof String)) {
				throw new IOException("Expecting string for key '"
						+ key
						+ "', got "
						+ result.getClass().getCanonicalName());
			}

			return (String)result;
		}

		@NonNull
		public Map<?, ?> getRequiredMap(@NonNull final Object key) throws IOException {

			final Object result = getRequired(key);

			if(!(result instanceof Map)) {
				throw new IOException("Expecting map for key '"
						+ key
						+ "', got "
						+ result.getClass().getCanonicalName());
			}

			return (Map<?, ?>)result;
		}

		public int getRequiredInt(@NonNull final Object key) throws IOException {

			final Object result = getRequired(key);

			if(!(result instanceof Integer)) {
				throw new IOException("Expecting integer for key '"
						+ key
						+ "', got "
						+ result.getClass().getCanonicalName());
			}

			return (Integer)result;
		}

		public long getRequiredLong(@NonNull final Object key) throws IOException {

			final Object result = getRequired(key);

			if(!(result instanceof Long)) {
				throw new IOException("Expecting long for key '"
						+ key
						+ "', got "
						+ result.getClass().getCanonicalName());
			}

			return (Long)result;
		}

		public boolean getRequiredBoolean(@NonNull final Object key) throws IOException {

			final Object result = getRequired(key);

			if(!(result instanceof Boolean)) {
				throw new IOException("Expecting boolean for key '"
						+ key
						+ "', got "
						+ result.getClass().getCanonicalName());
			}

			return (Boolean)result;
		}
	}

	public static void restore(
			@NonNull final BaseActivity activity,
			@NonNull final BackupSource source,
			@NonNull final Runnable onSuccess) {

		new Thread(() -> {

			try(DataInputStream dis = new DataInputStream(new BufferedInputStream(
					source.openInputStream()))) {

				final byte[] magicHeader = new byte[MAGIC_HEADER.length];
				dis.readFully(magicHeader);

				if(!Arrays.equals(MAGIC_HEADER, magicHeader)) {

					DialogUtils.showDialog(
							activity,
							R.string.restore_preferences_error_invalid_file_title,
							R.string.restore_preferences_error_invalid_file_contents_message);

					return;
				}

				final MapReader root;

				{
					final Object rootObj = SerializeUtils.deserialize(dis);

					if(rootObj == null) {
						throw new IOException("Expecting Map, got null");
					}

					if(!(rootObj instanceof Map)) {
						throw new IOException("Expecting Map, got "
								+ rootObj.getClass().getCanonicalName());
					}

					//noinspection unchecked
					root = new MapReader((Map<Object, Object>)rootObj);
				}

				final String type = root.getRequiredString(FIELD_TYPE);
				final int fileVersion = root.getRequiredInt(FIELD_FILE_VERSION);
				final int versionCode = root.getRequiredInt(FIELD_VERSION_CODE);
				final String versionName = root.getRequiredString(FIELD_VERSION_NAME);
				final boolean isAlpha = root.getRequiredBoolean(FIELD_IS_ALPHA);
				final long timestampUtc = root.getRequiredLong(FIELD_TIMESTAMP_UTC);
				final Map<?, ?> restorePrefs = root.getRequiredMap(FIELD_PREFS);

				Log.i(TAG, "Backup loaded: type="
						+ type
						+ ", fileVersion="
						+ fileVersion
						+ ", versionCode="
						+ versionCode
						+ ", versionName="
						+ versionName
						+ ", isAlpha="
						+ isAlpha
						+ ", timestampUtc="
						+ timestampUtc);

				if(!type.equals(FILE_TYPE)) {

					DialogUtils.showDialog(
							activity,
							R.string.restore_preferences_error_invalid_file_title,
							R.string.restore_preferences_error_invalid_file_contents_message);

					return;
				}

				if(fileVersion > FILE_VERSION) {

					DialogUtils.showDialog(
							activity,
							R.string.restore_preferences_error_invalid_file_title,
							R.string.restore_preferences_error_invalid_file_version_message);

					return;
				}

				final Runnable doRestore = () -> {

					Log.i(TAG, "Restoring " + restorePrefs.size() + " value(s)");

					General.getSharedPrefs(activity).performActionWithWriteLock(sharedPrefs -> {

						final HashSet<String> keysToRemove
								= new HashSet<>(sharedPrefs.getAll().keySet());

						for(final String ignoredPref : IGNORED_PREFS) {
							keysToRemove.remove(ignoredPref);
						}

						Log.i(TAG, "Existing preference count: " + restorePrefs.size());

						final SharedPreferences.Editor editor = sharedPrefs.edit();

						for(final Map.Entry<?, ?> entry : restorePrefs.entrySet()) {

							if(!(entry.getKey() instanceof String)) {

								Log.e(TAG, "Skipping entry of type "
										+ entry.getKey().getClass().getCanonicalName()
										+ " ("
										+ entry.getKey().toString()
										+ ")");

								continue;
							}

							final String key = (String)entry.getKey();
							final Object value = entry.getValue();

							if(IGNORED_PREFS.contains(key)) {
								Log.i(TAG, "Ignoring pref '" + key + "'");
								continue;
							}

							Log.i(TAG, "Restoring '" + key + "'");

							keysToRemove.remove(key);

							if(value instanceof String) {
								editor.putString(key, (String)value);

							} else if(value instanceof Integer) {
								editor.putInt(key, (Integer)value);

							} else if(value instanceof Set) {
								//noinspection unchecked
								editor.putStringSet(key, (Set<String>)value);

							} else if(value instanceof Boolean) {
								editor.putBoolean(key, (Boolean)value);

							} else if(value instanceof Long) {
								editor.putLong(key, (Long)value);

							} else if(value instanceof Float) {
								editor.putFloat(key, (Float)value);

							} else {
								throw new RuntimeException("Unexpected type: "
										+ value.getClass().getCanonicalName());
							}
						}

						Log.i(TAG, "Removing " + keysToRemove.size() + " old values");

						for(final String key : keysToRemove) {
							Log.i(TAG, "Removing '" + key + "'");
							editor.remove(key);
						}

						Log.i(TAG, "All restored, committing...");

						editor.apply();

						Log.i(TAG, "Handling feature flag upgrades...");

						FeatureFlagHandler.handleUpgrade(activity);

						Log.i(TAG, "Restore complete");
					});
					onSuccess.run();
				};

				if(versionCode > BuildConfig.VERSION_CODE) {

					DialogUtils.showDialogPositiveNegative(
							activity,
							activity.getString(
									R.string.restore_preferences_error_version_warning_title),
							activity.getString(
									R.string.restore_preferences_error_version_warning_message),
							R.string.button_continue_anyway,
							R.string.button_cancel,
							doRestore,
							() -> {});

				} else {
					doRestore.run();
				}

			} catch(final IOException | SerializeUtils.UnhandledTypeException e) {

				General.showResultDialog(activity, new RRError(
						activity.getString(
								R.string.restore_preferences_error_invalid_file_title),
						activity.getString(
								R.string.restore_preferences_error_invalid_file_contents_message),
						true,
						e));
			}

		}).start();
	}
}
