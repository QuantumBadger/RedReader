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

package org.quantumbadger.redreader.activities;

import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.body.HTTPRequestBodyMultipart;
import org.quantumbadger.redreader.http.body.multipart.PartFormDataBinary;
import org.quantumbadger.redreader.image.ThumbnailScaler;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.views.LoadingSpinnerView;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

public class ImgurUploadActivity extends BaseActivity {

	private TextView mTextView;

	private ImageView mThumbnailView;

	private byte[] mImageData;

	private Button mUploadButton;

	private View mLoadingOverlay;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.upload_to_imgur);

		final FrameLayout outerLayout = new FrameLayout(this);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);

		mTextView = new TextView(this);
		mTextView.setText(R.string.no_file_selected);
		layout.addView(mTextView);

		General.setAllMarginsDp(this, mTextView, 10);

		mUploadButton = new Button(this);
		mUploadButton.setText(R.string.button_upload);
		mUploadButton.setEnabled(false);
		layout.addView(mUploadButton);
		updateUploadButtonVisibility();

		final Button browseButton = new Button(this);
		browseButton.setText(R.string.button_browse);
		layout.addView(browseButton);

		mThumbnailView = new ImageView(this);
		layout.addView(mThumbnailView);
		General.setAllMarginsDp(this, mThumbnailView, 20);

		browseButton.setOnClickListener(v -> {
			final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
			intent.setType("image/*");
			startActivityForResultWithCallback(intent, (resultCode, data) -> {

				if(data == null || data.getData() == null) {
					return;
				}

				if(resultCode != RESULT_OK) {
					return;
				}

				onImageSelected(data.getData());
			});
		});

		mUploadButton.setOnClickListener(v -> {
			if(mImageData != null) {
				uploadImage();
			} else {
				General.quickToast(this, R.string.no_file_selected);
			}
		});

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);
		outerLayout.addView(sv);

		{
			mLoadingOverlay = new LoadingSpinnerView(this);
			outerLayout.addView(mLoadingOverlay);
			mLoadingOverlay.setBackgroundColor(Color.argb(220, 50, 50, 50));

			General.setLayoutMatchParent(mLoadingOverlay);

			mLoadingOverlay.setOnClickListener(v -> {
				// Do nothing
			});

			mLoadingOverlay.setVisibility(View.GONE);
		}

		setBaseActivityListing(outerLayout);

		General.setAllMarginsDp(this, layout, 20);
	}

	private void showLoadingOverlay() {
		mLoadingOverlay.setVisibility(View.VISIBLE);
	}

	private void hideLoadingOverlay() {
		mLoadingOverlay.setVisibility(View.GONE);
	}

	private void updateUploadButtonVisibility() {
		mUploadButton.setVisibility(
				mImageData != null
						? View.VISIBLE
						: View.GONE);
	}

	private void onImageSelected(final Uri uri) {

		showLoadingOverlay();

		new Thread("Image selected thread") {
			@Override
			public void run() {

				try {
					final ParcelFileDescriptor file
							= getContentResolver().openFileDescriptor(uri, "r");
					final long statSize = file.getStatSize();

					if(statSize >= 10 * 1000 * 1000) { // Use base 10 just to be safe...
						General.showResultDialog(
								ImgurUploadActivity.this,
								new RRError(
										getString(R.string.error_file_too_big_title),
										getString(
												R.string.error_file_too_big_message,
												statSize / 1024 + "kB",
												"10MB"),
										false));
						return;
					}

					final int thumbnailSizePx
							= General.dpToPixels(ImgurUploadActivity.this, 200);

					final Bitmap rawBitmap
							= BitmapFactory.decodeFileDescriptor(file.getFileDescriptor());
					final Bitmap thumbnailBitmap = ThumbnailScaler.scaleNoCrop(
							rawBitmap,
							thumbnailSizePx);
					rawBitmap.recycle();

					final ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();

					try(InputStream inputStream = getContentResolver().openInputStream(uri)) {
						General.copyStream(inputStream, byteOutput);
						byteOutput.flush();

					} catch(final IOException e) {
						throw new RuntimeException(e);
					}

					final byte[] imageData = byteOutput.toByteArray();

					AndroidCommon.UI_THREAD_HANDLER.post(() -> {
						mImageData = imageData;
						mUploadButton.setEnabled(true);
						mThumbnailView.setImageBitmap(thumbnailBitmap);
						mTextView.setText(getString(
								R.string.image_selected_summary,
								rawBitmap.getWidth(),
								rawBitmap.getHeight(),
								statSize / 1024 + "kB"));
						hideLoadingOverlay();
						updateUploadButtonVisibility();
					});

				} catch(final Exception e) {
					General.showResultDialog(
							ImgurUploadActivity.this,
							new RRError(
									getString(R.string.error_file_open_failed_title),
									getString(R.string.error_file_open_failed_message),
									true,
									e));

					AndroidCommon.UI_THREAD_HANDLER.post(() -> {
						mImageData = null;
						mUploadButton.setEnabled(false);
						mThumbnailView.setImageBitmap(null);
						mTextView.setText(R.string.no_file_selected);
						hideLoadingOverlay();
						updateUploadButtonVisibility();
					});
				}
			}
		}.start();
	}

	private void uploadImage() {

		showLoadingOverlay();

		CacheManager.getInstance(this).makeRequest(new CacheRequest(
				General.uriFromString("https://api.imgur.com/3/image"),
				RedditAccountManager.getInstance(this).getDefaultAccount(),
				null,
				new Priority(Constants.Priority.API_ACTION),
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.NOCACHE,
				CacheRequest.DOWNLOAD_QUEUE_IMGUR_API,
				new HTTPRequestBodyMultipart()
						.addPart(new PartFormDataBinary("image", mImageData)),
				this,
				new CacheRequestJSONParser(this, new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						final Uri imageUri;

						try {
							final JsonObject root = result.asObject();

							if(root == null) {
								throw new RuntimeException("Response root object is null");
							}

							final Boolean success = root.getBoolean("success");

							if(!Boolean.TRUE.equals(success)) {
								onFailure(
										CacheRequest.REQUEST_FAILURE_UPLOAD_FAIL_IMGUR,
										null,
										null,
										null,
										Optional.of(new FailedRequestBody(result)));
								return;
							}

							final String id = root.getObject("data").getString("id");
							imageUri = Uri.parse("https://imgur.com/" + id);

						} catch(final Throwable t) {
							onFailure(
									CacheRequest.REQUEST_FAILURE_PARSE_IMGUR,
									t,
									null,
									t.toString(),
									Optional.of(new FailedRequestBody(result)));
							return;
						}

						AndroidCommon.runOnUiThread(() -> {

							final Intent resultIntent = new Intent();
							resultIntent.setData(imageUri);
							setResult(0, resultIntent);
							finish();
						});
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						General.showResultDialog(
								ImgurUploadActivity.this,
								General.getGeneralErrorForFailure(
										ImgurUploadActivity.this,
										type,
										t,
										httpStatus,
										"https://api.imgur.com/3/image",
										body));

						AndroidCommon.runOnUiThread(ImgurUploadActivity.this::hideLoadingOverlay);
					}
				})));
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}
}
