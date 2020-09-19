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
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.preference.PreferenceManager;
import android.util.Log;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.FileProvider;
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
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.ShareOrderDialog;

import java.io.File;
import java.util.UUID;

// TODO remove this class, the permission isn't needed any more
public class ShareImageCallback implements BaseActivity.PermissionCallback {

	private static final String TAG = "ShareImageCallback";

	private final AppCompatActivity activity;
	private final String uri;

	public ShareImageCallback(final AppCompatActivity activity, final String uri) {
		this.activity = activity;
		this.uri = uri;
	}


	@Override
	public void onPermissionGranted() {

		final RedditAccount anon = RedditAccountManager.getAnon();

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

								final RRError error = General.getGeneralErrorForFailure(
										context,
										type,
										t,
										status,
										url.toString());
								General.showResultDialog(activity, error);
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

								final String filename
										= General.filenameFromString(info.urlOriginal);

								final File file = cacheFile.getFile();

								if(file == null) {
									notifyFailure(
											CacheRequest.REQUEST_FAILURE_STORAGE,
											new RuntimeException("Cache file is null"),
											null,
											"Cache file is null");
									return;
								}

								final Uri fileUri = FileProvider.getUriForFile(
										context,
										"org.quantumbadger.redreader.provider",
										file);

								final Intent shareIntent = new Intent()
										.setAction(Intent.ACTION_SEND)
										.putExtra(Intent.EXTRA_TITLE, filename)
										.putExtra(Intent.EXTRA_STREAM, fileUri)
										.setType(mimetype)
										.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);

								if(Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {

									// Due to bugs in the API before Android Lollipop, we have to
									// grant permission for every single app on the system to read
									// this file!

									for(final ResolveInfo resolveInfo
											: activity.getPackageManager().queryIntentActivities(
													shareIntent,
													PackageManager.MATCH_DEFAULT_ONLY)) {

										Log.i(TAG, "Granting permission to "
												+ resolveInfo.activityInfo.packageName
												+ " to read "
												+ fileUri);

										activity.grantUriPermission(
												resolveInfo.activityInfo.packageName,
												fileUri,
												Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
									}
								}

								if(PrefsUtility.pref_behaviour_sharing_dialog(
										activity,
										PreferenceManager.getDefaultSharedPreferences(
												activity))) {
									ShareOrderDialog.newInstance(shareIntent)
											.show(
													activity.getSupportFragmentManager(),
													null);
								} else {
									activity.startActivity(Intent.createChooser(
											shareIntent,
											activity.getString(R.string.action_share)));
								}


								activity.sendBroadcast(new Intent(
										Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
										fileUri)
								);
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
