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

package org.quantumbadger.redreader.cache;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.ProviderInfo;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.ParcelFileDescriptor;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.StringUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Locale;

public class CacheContentProvider extends ContentProvider {

	private static final String TAG = "CacheContentProvider";

	@NonNull
	private static final String[] COLUMNS = {OpenableColumns.DISPLAY_NAME, OpenableColumns.SIZE};

	private CacheManager mCacheManager;

	@NonNull
	private static String generateFilename(
			final long cacheId,
			@NonNull final String mimetype,
			@NonNull final String defaultExtension) {

		final String extension
				= FileUtils.getExtensionForMimetype(mimetype).orElse(defaultExtension);

		return String.format(
				Locale.US,
				"redreader_dl_%d.%s",
				cacheId,
				extension);
	}

	@NonNull
	private static Optional<Long> getCacheIdFromFilename(@NonNull final String filename) {

		final String[] filenameSplitDot = filename.split("\\.");

		if(filenameSplitDot.length != 2) {
			Log.e(TAG, "Expecting one dot in filename: " + filename);
			return Optional.empty();
		}

		final Optional<String> prefixRemoved
				= StringUtils.removePrefix(filenameSplitDot[0], "redreader_dl_");

		if(!prefixRemoved.isPresent()) {
			Log.e(TAG, "Expecting redreader_dl_ prefix in filename: " + filename);
			return Optional.empty();
		}

		try {
			return Optional.of(Long.valueOf(prefixRemoved.get()));

		} catch(final NumberFormatException e) {
			Log.e(TAG, "Invalid number in filename: " + filename, e);
			return Optional.empty();
		}
	}

	public static Uri getUriForFile(
			final long cacheId,
			@NonNull final String mimetype,
			@NonNull final String defaultExtension) {

		return new Uri.Builder()
				.scheme("content")
				.authority("org.quantumbadger.redreader.cacheprovider")
				.encodedPath(generateFilename(cacheId, mimetype, defaultExtension))
				.build();
	}

	@NonNull
	private Optional<CacheManager.ReadableCacheFile> getReadableCacheFile(@NonNull final Uri uri) {

		final String filename = General.filenameFromString(uri.toString());

		final Optional<Long> cacheId = getCacheIdFromFilename(filename);

		if(!cacheId.isPresent()) {
			return Optional.empty();
		}

		return Optional.of(mCacheManager.getExistingCacheFileById(
				cacheId.get(),
				CacheCompressionType.NONE)); // No compression is used for images
	}

	@NonNull
	private Optional<File> getFile(@NonNull final Uri uri) {

		final Optional<CacheManager.ReadableCacheFile> readableCacheFile
				= getReadableCacheFile(uri);

		if(!readableCacheFile.isPresent()) {
			return Optional.empty();
		}

		return readableCacheFile.get().getFile();
	}

	@Nullable
	@Override
	public ParcelFileDescriptor openFile(@NonNull final Uri uri, @NonNull final String mode)
			throws FileNotFoundException {

		return ParcelFileDescriptor.open(
				getFile(uri).orThrow(() -> new FileNotFoundException(uri.toString())),
				ParcelFileDescriptor.MODE_READ_ONLY);
	}

	@Override
	public boolean onCreate() {
		mCacheManager = CacheManager.getInstance(getContext());
		return true;
	}

	@Override
	public void attachInfo(
			@NonNull final Context context,
			@NonNull final ProviderInfo info) {
		super.attachInfo(context, info);

		// Sanity check our security
		if (info.exported) {
			throw new SecurityException("Provider must not be exported");
		}

		if (!info.grantUriPermissions) {
			throw new SecurityException("Provider must grant uri permissions");
		}
	}

	@Nullable
	@Override
	public Cursor query(
			@NonNull final Uri uri,
			@Nullable final String[] projection,
			@Nullable final String selection,
			@Nullable final String[] selectionArgs,
			@Nullable final String sortOrder) {

		final Optional<CacheManager.ReadableCacheFile> readableCacheFile
				= getReadableCacheFile(uri);

		if(!readableCacheFile.isPresent()) {
			Log.e(TAG, "Couldn't get readable cache file: " + uri);
			return new MatrixCursor(COLUMNS, 0);
		}

		final Optional<File> file = readableCacheFile.get().getFile();

		if(!file.isPresent()) {
			Log.e(TAG, "Couldn't get underlying file: " + uri);
			return new MatrixCursor(COLUMNS, 0);
		}

		final Optional<String> mimetype = readableCacheFile.get().lookupMimetype();

		if(!mimetype.isPresent()) {
			Log.e(TAG, "Couldn't get mimetype: " + uri);
			return new MatrixCursor(COLUMNS, 0);
		}

		final ArrayList<String> cols = new ArrayList<>();
		final ArrayList<Object> values = new ArrayList<>();

		for(final String col : projection == null ? COLUMNS : projection) {

			if(OpenableColumns.DISPLAY_NAME.equals(col)) {
				cols.add(OpenableColumns.DISPLAY_NAME);
				values.add(generateFilename(
						readableCacheFile.get().getId(),
						mimetype.get(),
						"jpg"));

			} else if(OpenableColumns.SIZE.equals(col)) {
				cols.add(OpenableColumns.SIZE);
				values.add(file.get().length());

			} else if(MediaStore.MediaColumns.MIME_TYPE.equals(col)) {
				cols.add(MediaStore.MediaColumns.MIME_TYPE);
				values.add(mimetype.get());
			}
		}

		final MatrixCursor cursor = new MatrixCursor(cols.toArray(new String[0]), 1);
		cursor.addRow(values);
		return cursor;
	}

	@Nullable
	@Override
	public String getType(@NonNull final Uri uri) {

		final Optional<CacheManager.ReadableCacheFile> readableCacheFile
				= getReadableCacheFile(uri);

		if(!readableCacheFile.isPresent()) {
			Log.e(TAG, "Couldn't get readable cache file: " + uri);
			return null;
		}

		return readableCacheFile.get().lookupMimetype().orElseNull();
	}

	@Nullable
	@Override
	public Uri insert(
			@NonNull final Uri uri,
			@Nullable final ContentValues values) {
		throw new UnsupportedOperationException("No external inserts");
	}

	@Override
	public int delete(
			@NonNull final Uri uri,
			@Nullable final String selection,
			@Nullable final String[] selectionArgs) {
		return 0;
	}

	@Override
	public int update(
			@NonNull final Uri uri,
			@Nullable final ContentValues values,
			@Nullable final String selection,
			@Nullable final String[] selectionArgs) {
		throw new UnsupportedOperationException("No external updates");
	}
}
