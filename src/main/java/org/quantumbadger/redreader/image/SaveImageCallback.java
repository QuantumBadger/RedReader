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

package org.quantumbadger.redreader.image;

import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.RRError;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

/**
 * Created by Marco on 01.03.2017.
 */
public class SaveImageCallback implements BaseActivity.PermissionCallback {
	private final AppCompatActivity activity;
	private final String uri;
	public SaveImageCallback(final AppCompatActivity activity, final String uri){
		this.activity = activity;
		this.uri = uri;
	}

	@Override
	public void onPermissionGranted() {

		final RedditAccount anon = RedditAccountManager.getAnon();

		LinkHandler.getImageInfo(activity, uri, Constants.Priority.IMAGE_VIEW, 0, new GetImageInfoListener() {

			@Override
			public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				final RRError error = General.getGeneralErrorForFailure(activity, type, t, status, uri);
				General.showResultDialog(activity, error);
			}

			@Override
			public void onSuccess(final ImageInfo info) {

				CacheManager.getInstance(activity).makeRequest(new CacheRequest(
						General.uriFromString(info.urlOriginal),
						anon,
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
					protected void onCallbackException(Throwable t) {
						BugReportActivity.handleGlobalError(context, t);
					}

					@Override
					protected void onDownloadNecessary() {
						General.quickToast(context, R.string.download_downloading);
					}

					@Override
					protected void onDownloadStarted() {
					}

					@Override
					protected void onFailure(
							@CacheRequest.RequestFailureType int type,
							Throwable t,
							Integer status,
							String readableMessage) {

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
						General.showResultDialog(activity, error);
					}

					@Override
					protected void onProgress(
							boolean authorizationInProgress,
							long bytesRead,
							long totalBytes) {
					}

					@Override
					protected void onSuccess(
							CacheManager.ReadableCacheFile cacheFile,
							long timestamp,
							UUID session,
							boolean fromCache,
							String mimetype) {

						String filename = General.filenameFromString(info.urlOriginal);
						File dst = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), filename);

						if(dst.exists()) {
							int count = 0;

							while(dst.exists()) {
								count++;
								dst = new File(
										Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES),
										count + "_" + filename.substring(1));
							}
						}

						try {
							final InputStream cacheFileInputStream = cacheFile.getInputStream();

							if(cacheFileInputStream == null) {
								notifyFailure(CacheRequest.REQUEST_FAILURE_CACHE_MISS, null, null, "Could not find cached image");
								return;
							}

							General.copyFile(cacheFileInputStream, dst);

						} catch(IOException e) {
							notifyFailure(CacheRequest.REQUEST_FAILURE_STORAGE, e, null, "Could not copy file");
							return;
						}

						activity.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
								Uri.parse("file://" + dst.getAbsolutePath()))
						);

						General.quickToast(context, context.getString(R.string.action_save_image_success) + " " + dst.getAbsolutePath());
					}
				});

			}

			@Override
			public void onNotAnImage() {
				General.quickToast(activity, R.string.selected_link_is_not_image);
			}
		});
	}

	@Override
	public void onPermissionDenied() {
		General.quickToast(activity, R.string.save_image_permission_denied);
	}
}
