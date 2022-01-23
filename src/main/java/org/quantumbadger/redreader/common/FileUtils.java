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
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.os.StatFs;
import android.provider.MediaStore;
import android.util.Log;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheCompressionType;
import org.quantumbadger.redreader.cache.CacheContentProvider;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.fragments.ShareOrderDialog;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.LegacySaveImageCallback;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;

public class FileUtils {

	private static final String TAG = "FileUtils";

	private static final HashMap<String, String> MIMETYPE_TO_EXTENSION = new HashMap<>();

	static {
		MIMETYPE_TO_EXTENSION.put("audio/3gpp2", "3g2");
		MIMETYPE_TO_EXTENSION.put("video/3gpp2", "3g2");
		MIMETYPE_TO_EXTENSION.put("audio/3gpp", "3gp");
		MIMETYPE_TO_EXTENSION.put("video/3gpp", "3gp");
		MIMETYPE_TO_EXTENSION.put("application/x-7z-compressed", "7z");
		MIMETYPE_TO_EXTENSION.put("audio/aac", "aac");
		MIMETYPE_TO_EXTENSION.put("application/x-abiword", "abw");
		MIMETYPE_TO_EXTENSION.put("application/x-freearc", "arc");
		MIMETYPE_TO_EXTENSION.put("video/x-msvideo", "avi");
		MIMETYPE_TO_EXTENSION.put("application/vnd.amazon.ebook", "azw");
		MIMETYPE_TO_EXTENSION.put("application/octet-stream", "bin");
		MIMETYPE_TO_EXTENSION.put("image/bmp", "bmp");
		MIMETYPE_TO_EXTENSION.put("application/x-bzip2", "bz2");
		MIMETYPE_TO_EXTENSION.put("application/x-bzip", "bz");
		MIMETYPE_TO_EXTENSION.put("application/x-csh", "csh");
		MIMETYPE_TO_EXTENSION.put("text/css", "css");
		MIMETYPE_TO_EXTENSION.put("text/csv", "csv");
		MIMETYPE_TO_EXTENSION.put("application/msword", "doc");
		MIMETYPE_TO_EXTENSION.put(
				"application/vnd.openxmlformats-officedocument.wordprocessingml.document",
				"docx");
		MIMETYPE_TO_EXTENSION.put("application/vnd.ms-fontobject", "eot");
		MIMETYPE_TO_EXTENSION.put("application/epub+zip", "epub");
		MIMETYPE_TO_EXTENSION.put("image/gif", "gif");
		MIMETYPE_TO_EXTENSION.put("application/gzip", "gz");
		MIMETYPE_TO_EXTENSION.put("video/h263", "h263");
		MIMETYPE_TO_EXTENSION.put("video/h264", "h264");
		MIMETYPE_TO_EXTENSION.put("video/h265", "h265");
		MIMETYPE_TO_EXTENSION.put("image/heic ", "heic");
		MIMETYPE_TO_EXTENSION.put("image/heic-sequence ", "heic");
		MIMETYPE_TO_EXTENSION.put("image/heif ", "heif");
		MIMETYPE_TO_EXTENSION.put("image/heif-sequence", "heif");
		MIMETYPE_TO_EXTENSION.put("text/html", "html");
		MIMETYPE_TO_EXTENSION.put("image/vnd.microsoft.icon", "ico");
		MIMETYPE_TO_EXTENSION.put("text/calendar", "ics");
		MIMETYPE_TO_EXTENSION.put("application/java-archive", "jar");
		MIMETYPE_TO_EXTENSION.put("image/jpeg", "jpg");
		MIMETYPE_TO_EXTENSION.put("application/json", "json");
		MIMETYPE_TO_EXTENSION.put("application/ld+json", "jsonld");
		MIMETYPE_TO_EXTENSION.put("text/javascript", "js");
		MIMETYPE_TO_EXTENSION.put("audio/midi audio/x-midi", "mid");
		MIMETYPE_TO_EXTENSION.put("audio/mpeg", "mp3");
		MIMETYPE_TO_EXTENSION.put("video/mp4", "mp4");
		MIMETYPE_TO_EXTENSION.put("application/dash+xml", "mpd");
		MIMETYPE_TO_EXTENSION.put("video/mpeg", "mpeg");
		MIMETYPE_TO_EXTENSION.put("application/vnd.apple.installer+xml", "mpkg");
		MIMETYPE_TO_EXTENSION.put("video/mpv", "mpv");
		MIMETYPE_TO_EXTENSION.put("application/vnd.oasis.opendocument.presentation", "odp");
		MIMETYPE_TO_EXTENSION.put("application/vnd.oasis.opendocument.spreadsheet", "ods");
		MIMETYPE_TO_EXTENSION.put("application/vnd.oasis.opendocument.text", "odt");
		MIMETYPE_TO_EXTENSION.put("audio/ogg", "oga");
		MIMETYPE_TO_EXTENSION.put("video/ogg", "ogv");
		MIMETYPE_TO_EXTENSION.put("application/ogg", "ogx");
		MIMETYPE_TO_EXTENSION.put("audio/opus", "opus");
		MIMETYPE_TO_EXTENSION.put("font/otf", "otf");
		MIMETYPE_TO_EXTENSION.put("application/pdf", "pdf");
		MIMETYPE_TO_EXTENSION.put("application/x-httpd-php", "php");
		MIMETYPE_TO_EXTENSION.put("image/png", "png");
		MIMETYPE_TO_EXTENSION.put("application/vnd.ms-powerpoint", "ppt");
		MIMETYPE_TO_EXTENSION.put(
				"application/vnd.openxmlformats-officedocument.presentationml.presentation",
				"pptx");
		MIMETYPE_TO_EXTENSION.put("application/vnd.rar", "rar");
		MIMETYPE_TO_EXTENSION.put("application/rtf", "rtf");
		MIMETYPE_TO_EXTENSION.put("application/x-sh", "sh");
		MIMETYPE_TO_EXTENSION.put("image/svg+xml", "svg");
		MIMETYPE_TO_EXTENSION.put("application/x-shockwave-flash", "swf");
		MIMETYPE_TO_EXTENSION.put("application/x-tar", "tar");
		MIMETYPE_TO_EXTENSION.put("image/tiff", "tiff");
		MIMETYPE_TO_EXTENSION.put("video/mp2t", "ts");
		MIMETYPE_TO_EXTENSION.put("font/ttf", "ttf");
		MIMETYPE_TO_EXTENSION.put("text/plain", "txt");
		MIMETYPE_TO_EXTENSION.put("application/vnd.visio", "vsd");
		MIMETYPE_TO_EXTENSION.put("audio/wav", "wav");
		MIMETYPE_TO_EXTENSION.put("audio/webm", "weba");
		MIMETYPE_TO_EXTENSION.put("video/webm", "webm");
		MIMETYPE_TO_EXTENSION.put("image/webp", "webp");
		MIMETYPE_TO_EXTENSION.put("font/woff2", "woff2");
		MIMETYPE_TO_EXTENSION.put("font/woff", "woff");
		MIMETYPE_TO_EXTENSION.put("application/xhtml+xml", "xhtml");
		MIMETYPE_TO_EXTENSION.put("application/vnd.ms-excel", "xls");
		MIMETYPE_TO_EXTENSION.put(
				"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
				"xlsx");
		MIMETYPE_TO_EXTENSION.put("application/xml", "xml");
		MIMETYPE_TO_EXTENSION.put("text/xml", "xml");
		MIMETYPE_TO_EXTENSION.put("application/vnd.mozilla.xul+xml", "xul");
		MIMETYPE_TO_EXTENSION.put("application/zip", "zip");
	}

	@NonNull
	public static Optional<String> getExtensionForMimetype(@NonNull final String mimetype) {

		final String splitType;

		if(mimetype.contains(";")) {
			splitType = mimetype.split(";")[0];
		} else {
			splitType = mimetype;
		}

		return Optional.ofNullable(MIMETYPE_TO_EXTENSION.get(
				StringUtils.asciiLowercase(splitType)));
	}

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
		final long space = getFreeSpaceAvailable(PrefsUtility.pref_cache_location(context));
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

	public static void shareImageAtUri(
			@NonNull final BaseActivity activity,
			@NonNull final String uri) {

		downloadImageToSave(activity, uri, (info, cacheFile, mimetype) -> {

			final Uri externalUri = CacheContentProvider.getUriForFile(
					cacheFile.getId(),
					mimetype,
					getExtensionFromPath(info.urlOriginal).orElse("jpg"));

			Log.i(TAG, "Sharing image with external uri: " + externalUri);

			final Intent shareIntent = new Intent()
					.setAction(Intent.ACTION_SEND)
					.putExtra(Intent.EXTRA_STREAM, externalUri)
					.setType(mimetype)
					.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);

			// Workaround for third party share apps
			shareIntent.setClipData(ClipData.newRawUri(null, externalUri));

			if(Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {

				// Due to bugs in the API before Android Lollipop, we have to
				// grant permission for every single app on the system to read
				// this file!

				for(final ResolveInfo resolveInfo
						: activity.getPackageManager().queryIntentActivities(
								shareIntent,
								PackageManager.MATCH_DEFAULT_ONLY)) {

					Log.i(TAG, "Legacy OS: granting permission to "
							+ resolveInfo.activityInfo.packageName
							+ " to read "
							+ externalUri);

					activity.grantUriPermission(
							resolveInfo.activityInfo.packageName,
							externalUri,
							Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
				}
			}

			if(PrefsUtility.pref_behaviour_sharing_dialog()) {
				ShareOrderDialog.newInstance(shareIntent)
						.show(activity.getSupportFragmentManager(), null);

			} else {
				activity.startActivity(Intent.createChooser(
						shareIntent,
						activity.getString(R.string.action_share)));
			}
		});
	}

	private interface FileDataSource {
		void writeTo(@NonNull OutputStream outputStream) throws IOException;
	}

	private static class CacheFileDataSource implements FileDataSource {

		@NonNull private final CacheManager.ReadableCacheFile mCacheFile;

		private CacheFileDataSource(
				@NonNull final CacheManager.ReadableCacheFile cacheFile) {

			mCacheFile = cacheFile;
		}

		@Override
		public void writeTo(@NonNull final OutputStream outputStream) throws IOException {

			try(InputStream inputStream = mCacheFile.getInputStream()) {
				General.copyStream(inputStream, outputStream);
				outputStream.flush();
			}
		}
	}

	@RequiresApi(Build.VERSION_CODES.Q)
	private static void mediaStoreDownloadsInsertFile(
			@NonNull final BaseActivity activity,
			@NonNull final String name,
			@Nullable final String mimetype,
			final long fileSize,
			@NonNull final FileDataSource source,
			@NonNull final Runnable onSuccess) {

		final Uri downloads = MediaStore.Downloads.getContentUri(MediaStore.VOLUME_EXTERNAL);

		Log.i(TAG, "Got downloads URI: " + downloads.toString());

		final ContentValues fileMetadata = new ContentValues();
		fileMetadata.put(MediaStore.Downloads.DISPLAY_NAME, name);
		fileMetadata.put(MediaStore.Downloads.SIZE, fileSize);

		if(mimetype != null) {
			fileMetadata.put(MediaStore.Downloads.MIME_TYPE, mimetype);
		}

		fileMetadata.put(MediaStore.Downloads.IS_PENDING, true);

		final ContentResolver resolver = activity.getContentResolver();

		final Uri fileUri = resolver.insert(downloads, fileMetadata);

		Log.i(TAG, "Got file URI: " + fileUri.toString());

		new Thread(() -> {

			try(OutputStream os = resolver.openOutputStream(fileUri)) {
				source.writeTo(os);
				os.flush();

			} catch(final IOException e) {

				showUnexpectedStorageErrorDialog(
						activity,
						e,
						fileUri.toString());

				resolver.delete(fileUri, null, null);

				return;
			}

			fileMetadata.put(MediaStore.Downloads.IS_PENDING, false);
			resolver.update(fileUri, fileMetadata, null, null);

			onSuccess.run();

		}).start();
	}

	@RequiresApi(19)
	private static void createSAFDocumentWithIntent(
			@NonNull final BaseActivity activity,
			@NonNull final String filename,
			@Nullable final String mimetype,
			@NonNull final FileDataSource source,
			@NonNull final Runnable onSuccess) {

		final Intent intent = new Intent(Intent.ACTION_CREATE_DOCUMENT)
				.setType(mimetype)
				.putExtra(Intent.EXTRA_TITLE, filename)
				.addCategory(Intent.CATEGORY_OPENABLE);

		try {
			activity.startActivityForResultWithCallback(
					intent,
					(resultCode, data) -> {

						if(data == null || data.getData() == null) {
							return;
						}

						new Thread(() -> {

							try(OutputStream outputStream = activity.getContentResolver()
									.openOutputStream(data.getData())) {

								source.writeTo(outputStream);

								onSuccess.run();

							} catch(final IOException e) {
								showUnexpectedStorageErrorDialog(
										activity,
										e,
										data.getData().toString());
							}

						}).start();
					});

		} catch(final ActivityNotFoundException e) {
			DialogUtils.showDialog(
					activity,
					R.string.error_no_file_manager_title,
					R.string.error_no_file_manager_message);
		}
	}

	public static void saveImageAtUri(
			@NonNull final BaseActivity activity,
			@NonNull final String uri) {

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

			final PrefsUtility.SaveLocation saveLocation
					= PrefsUtility.pref_behaviour_save_location();

			switch(saveLocation) {

				case PROMPT_EVERY_TIME: {

					Log.i(TAG, "Android version Lollipop or higher, showing SAF prompt");

					downloadImageToSave(activity, uri, (info, cacheFile, mimetype) -> {

						final String filename = General.filenameFromString(info.urlOriginal);

						createSAFDocumentWithIntent(
								activity,
								filename,
								mimetype,
								new CacheFileDataSource(cacheFile),
								() -> General.quickToast(
										activity,
										R.string.action_save_image_success_no_path));
					});

					break;
				}

				case SYSTEM_DEFAULT: {

					Log.i(TAG, "Android version Lollipop or higher, saving to Downloads");

					if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {

						Log.i(TAG, "Android version Q or higher, saving with MediaStore");

						downloadImageToSave(activity, uri, (info, cacheFile, mimetype) -> {

							final String filename = General.filenameFromString(info.urlOriginal);

							mediaStoreDownloadsInsertFile(
									activity,
									filename,
									mimetype,
									cacheFile.getFile().map(File::length).orElse(0L),
									new CacheFileDataSource(cacheFile),
									() -> General.quickToast(
											activity,
											R.string.action_save_image_success_no_path));
						});

					} else {
						Log.i(TAG, "Android version below Q, saving with legacy method");

						activity.requestPermissionWithCallback(
								Manifest.permission.WRITE_EXTERNAL_STORAGE,
								new LegacySaveImageCallback(activity, uri));
					}

					break;
				}

				default: {
					BugReportActivity.handleGlobalError(activity, new RuntimeException(
							"Missing handler for preference value " + saveLocation));
				}
			}

		} else {

			Log.i(TAG, "Android version before Lollipop, using legacy save method");

			activity.requestPermissionWithCallback(
					Manifest.permission.WRITE_EXTERNAL_STORAGE,
					new LegacySaveImageCallback(activity, uri));
		}
	}

	private static void showUnexpectedStorageErrorDialog(
			@NonNull final BaseActivity activity,
			@NonNull final Throwable throwable,
			@NonNull final String uri) {

		General.showResultDialog(activity, new RRError(
				activity.getString(R.string.error_unexpected_storage_title),
				activity.getString(R.string.error_unexpected_storage_message),
				true,
				throwable,
				null,
				uri,
				null));
	}

	public interface DownloadImageToSaveSuccessCallback {
		void onSuccess(
				@NonNull ImageInfo info,
				CacheManager.ReadableCacheFile cacheFile,
				String mimetype);
	}

	@RequiresApi(api = Build.VERSION_CODES.JELLY_BEAN_MR2)
	private static void internalDownloadImageToSaveAudio(
			@NonNull final BaseActivity activity,
			@NonNull final ImageInfo info,
			@NonNull final CacheManager.ReadableCacheFile video,
			@NonNull final DownloadImageToSaveSuccessCallback callback) {

		final CacheManager cacheManager = CacheManager.getInstance(activity);

		cacheManager.makeRequest(new CacheRequest(
				General.uriFromString(info.urlAudioStream),
				RedditAccountManager.getAnon(),
				null,
				new Priority(Constants.Priority.IMAGE_VIEW),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				activity,
				new CacheRequestCallbacks() {
					@Override
					public void onDownloadNecessary() {
					}

					@Override
					public void onFailure(
							@CacheRequest.RequestFailureType final int type,
							final Throwable t,
							final Integer status,
							final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						General.showResultDialog(
								activity,
								General.getGeneralErrorForFailure(
										activity,
										type,
										t,
										status,
										info.urlAudioStream,
										body));
					}

					@Override
					public void onCacheFileWritten(
							@NonNull final CacheManager.ReadableCacheFile cacheFile,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							final String mimetype) {

						try {
							final CacheManager.WritableCacheFile output
									= cacheManager.openNewCacheFile(
											Objects.requireNonNull(General.uriFromString(
													"redreader://muxedmedia/"
															+ UUID.randomUUID()
															+ ".mp4")),
											RedditAccountManager.getAnon(),
											Constants.FileType.IMAGE,
											session,
											"video/mp4",
											CacheCompressionType.NONE);

							final File file = output.writeExternally();

							MediaUtils.muxFiles(
									file,
									new File[] {
											cacheFile.getFile().orThrow(() -> new RuntimeException(
													"Audio file not found")),
											video.getFile().orThrow(() -> new RuntimeException(
													"Video file not found"))
									},
									() -> {

										try {
											output.onWriteFinished();

											callback.onSuccess(
													info,
													output.getReadableCacheFile(),
													"video/mp4");

										} catch(final Exception e) {
											General.showResultDialog(
													activity,
													General.getGeneralErrorForFailure(
															activity,
															CacheRequest.REQUEST_FAILURE_STORAGE,
															e,
															null,
															info.urlOriginal,
															Optional.empty()));
										}
									},

									e -> General.showResultDialog(
											activity,
											new RRError(
													activity.getResources().getString(
															R.string.error_title_muxing_failed),
													activity.getResources().getString(
															R.string.error_message_muxing_failed),
													true,
													e,
													null,
													info.urlOriginal,
													null)));

						} catch(final Exception e) {
							General.showResultDialog(
									activity,
									General.getGeneralErrorForFailure(
											activity,
											CacheRequest.REQUEST_FAILURE_STORAGE,
											e,
											null,
											info.urlOriginal,
											Optional.empty()));
						}
					}
				}));
	}

	public static void downloadImageToSave(
			@NonNull final BaseActivity activity,
			@NonNull final String uri,
			@NonNull final DownloadImageToSaveSuccessCallback callback) {

		LinkHandler.getImageInfo(
				activity,
				uri,
				new Priority(Constants.Priority.IMAGE_VIEW),
				new GetImageInfoListener() {

					@Override
					public void onFailure(
							final @CacheRequest.RequestFailureType int type,
							final Throwable t,
							final Integer status,
							final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {
						final RRError error = General.getGeneralErrorForFailure(
								activity,
								type,
								t,
								status,
								uri,
								body);
						General.showResultDialog(activity, error);
					}

					@Override
					public void onSuccess(final ImageInfo info) {

						CacheManager.getInstance(activity).makeRequest(new CacheRequest(
								General.uriFromString(info.urlOriginal),
								RedditAccountManager.getAnon(),
								null,
								new Priority(Constants.Priority.IMAGE_VIEW),
								DownloadStrategyIfNotCached.INSTANCE,
								Constants.FileType.IMAGE,
								CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
								activity,
								new CacheRequestCallbacks() {
									@Override
									public void onDownloadNecessary() {
										General.quickToast(
												activity,
												R.string.download_downloading,
												Toast.LENGTH_SHORT);
									}

									@Override
									public void onFailure(
											@CacheRequest.RequestFailureType final int type,
											final Throwable t,
											final Integer status,
											final String readableMessage,
											@NonNull final Optional<FailedRequestBody> body) {

										General.showResultDialog(
												activity,
												General.getGeneralErrorForFailure(
														activity,
														type,
														t,
														status,
														info.urlOriginal,
														body));
									}

									@Override
									public void onCacheFileWritten(
											@NonNull final CacheManager.ReadableCacheFile cacheFile,
											final long timestamp,
											@NonNull final UUID session,
											final boolean fromCache,
											final String mimetype) {

										if(info.urlAudioStream != null
												&& Build.VERSION.SDK_INT >= 18) {
											Log.i(TAG, "Also downloading audio stream...");

											internalDownloadImageToSaveAudio(
													activity,
													info,
													cacheFile,
													callback);

										} else {
											callback.onSuccess(info, cacheFile, mimetype);
										}
									}
								}));

					}

					@Override
					public void onNotAnImage() {
						General.quickToast(activity, R.string.selected_link_is_not_image);
					}
				});
	}

	@NonNull
	public static Optional<String> getExtensionFromPath(@NonNull final String path) {

		final String[] pathSegments = path.split("/");

		if(pathSegments.length == 0) {
			return Optional.empty();
		}

		final String[] dotSegments = pathSegments[pathSegments.length - 1].split("\\.");

		if(dotSegments.length < 2) {
			return Optional.empty();
		}

		if(dotSegments.length == 2 && dotSegments[0].isEmpty()) {
			return Optional.empty();
		}

		return Optional.of(dotSegments[dotSegments.length - 1]);
	}

	@NonNull
	public static File buildPath(
			@NonNull final File base,
			@NonNull final String... components) {

		File result = base;

		for(final String component : components) {
			result = new File(result, component);
		}

		return result;
	}

	@NonNull private static final Object sMkdirsLock = new Object();

	public static void mkdirs(@NonNull final File file) throws IOException {

		synchronized(sMkdirsLock) {

			if(file.isDirectory()) {
				return;
			}

			if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {

				try {
					Files.createDirectories(file.toPath());
				} catch(final Exception e) {
					throw new IOException(
							"Failed to create dirs " + file.getAbsolutePath(),
							e);
				}

			} else {
				if(!file.mkdirs()) {
					throw new IOException("Failed to create dirs " + file.getAbsolutePath());
				}
			}
		}
	}
}
