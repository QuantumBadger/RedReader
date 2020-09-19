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

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.StatFs;
import android.preference.PreferenceManager;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.LegacySaveImageCallback;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

public class FileUtils {

	public static void moveFile(final File src, final File dst) throws IOException {

		if(!src.renameTo(dst)) {

			copyFile(src, dst);

			if(!src.delete()) {
				src.deleteOnExit();
			}
		}
	}

	public static void copyFile(final File src, final File dst) throws IOException {

		try(FileInputStream fis = new FileInputStream(src)) {
			copyFile(fis, dst);
		}
	}

	public static void copyFile(final InputStream fis, final File dst) throws IOException {
		try(FileOutputStream fos = new FileOutputStream(dst)) {
			General.copyStream(fis, fos);
			fos.flush();
		}
	}

	public static boolean isCacheDiskFull(final Context context) {
		final long space = getFreeSpaceAvailable(PrefsUtility.pref_cache_location(
				context,
				PreferenceManager.getDefaultSharedPreferences(context)));
		return space < 128 * 1024 * 1024;
	}

	/// Get the number of free bytes that are available on the external storage.
	@SuppressWarnings("deprecation")
	public static long getFreeSpaceAvailable(final String path) {
		final StatFs stat = new StatFs(path);
		final long availableBlocks;
		final long blockSize;
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			availableBlocks = stat.getAvailableBlocksLong();
			blockSize = stat.getBlockSizeLong();
		} else {
			availableBlocks = stat.getAvailableBlocks();
			blockSize = stat.getBlockSize();
		}
		return availableBlocks * blockSize;
	}

	public static void saveImageAtUri(
			@NonNull final BaseActivity activity,
			@NonNull final String uri) {

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

			downloadImageToSave(activity, uri, (info, cacheFile, mimetype) -> {

				final String filename = General.filenameFromString(info.urlOriginal);

				final Intent intent = new Intent(Intent.ACTION_CREATE_DOCUMENT)
						.setType(mimetype)
						.putExtra(Intent.EXTRA_TITLE, filename)
						.addCategory(Intent.CATEGORY_OPENABLE);

				activity.startActivityForResult(
						intent,
						42);
			});

		} else {
			activity.requestPermissionWithCallback(
					Manifest.permission.WRITE_EXTERNAL_STORAGE,
					new LegacySaveImageCallback(activity, uri));
		}
	}

	public interface DownloadImageToSaveSuccessCallback {
		void onSuccess(
				@NonNull ImageInfo info,
				CacheManager.ReadableCacheFile cacheFile,
				String mimetype);
	}

	public static void downloadImageToSave(
			@NonNull final BaseActivity activity,
			@NonNull final String uri,
			@NonNull final DownloadImageToSaveSuccessCallback callback) {

		final RedditAccount account
				= RedditAccountManager.getInstance(activity).getDefaultAccount();

		LinkHandler.getImageInfo(
				activity,
				uri,
				Constants.Priority.IMAGE_VIEW,
				0,
				new GetImageInfoListener() {

					@Override
					public void onFailure(
							final @CacheRequest.RequestFailureType int type,
							final Throwable t,
							final Integer status,
							final String readableMessage) {
						final RRError error = General.getGeneralErrorForFailure(
								activity,
								type,
								t,
								status,
								uri);
						General.showResultDialog(activity, error);
					}

					@Override
					public void onSuccess(final ImageInfo info) {

						CacheManager.getInstance(activity).makeRequest(new CacheRequest(
								General.uriFromString(info.urlOriginal),
								account,
								null,
								Constants.Priority.IMAGE_VIEW,
								0,
								DownloadStrategyIfNotCached.INSTANCE,
								Constants.FileType.IMAGE,
								CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
								false,
								false,
								activity) {

							@Override
							protected void onCallbackException(final Throwable t) {
								BugReportActivity.handleGlobalError(context, t);
							}

							@Override
							protected void onDownloadNecessary() {
								General.quickToast(
										context,
										R.string.download_downloading);
							}

							@Override
							protected void onDownloadStarted() {
							}

							@Override
							protected void onFailure(
									@CacheRequest.RequestFailureType final int type,
									final Throwable t,
									final Integer status,
									final String readableMessage) {

								General.showResultDialog(
										activity,
										General.getGeneralErrorForFailure(
												context,
												type,
												t,
												status,
												url.toString()));
							}

							@Override
							protected void onProgress(
									final boolean authorizationInProgress,
									final long bytesRead,
									final long totalBytes) {
							}

							@Override
							protected void onSuccess(
									final CacheManager.ReadableCacheFile cacheFile,
									final long timestamp,
									final UUID session,
									final boolean fromCache,
									final String mimetype) {

								callback.onSuccess(info, cacheFile, mimetype);
							}
						});

					}

					@Override
					public void onNotAnImage() {
						General.quickToast(activity, R.string.selected_link_is_not_image);
					}
				});
	}
}
