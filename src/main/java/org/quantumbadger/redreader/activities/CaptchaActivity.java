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
import android.os.Bundle;
import android.text.InputType;
import android.view.Gravity;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

public class CaptchaActivity extends BaseActivity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		getSupportActionBar().setTitle(R.string.post_captcha_title);

		final LoadingView loadingView = new LoadingView(this, R.string.download_waiting, true, true);
		setBaseActivityContentView(loadingView);

		final RedditAccount selectedAccount = RedditAccountManager.getInstance(this).getAccount(getIntent().getStringExtra("username"));

		final CacheManager cm = CacheManager.getInstance(this);

		RedditAPI.newCaptcha(cm, new APIResponseHandler.NewCaptchaResponseHandler(this) {
			@Override
			protected void onSuccess(final String captchaId) {

				final URI captchaUrl = General.uriFromString("https://reddit.com/captcha/" + captchaId);

				cm.makeRequest(new CacheRequest(captchaUrl, RedditAccountManager.getAnon(), null, Constants.Priority.CAPTCHA,
						0, CacheRequest.DOWNLOAD_FORCE, Constants.FileType.CAPTCHA,
						CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE, false, true, CaptchaActivity.this) {
					@Override
					protected void onCallbackException(Throwable t) {
						BugReportActivity.handleGlobalError(CaptchaActivity.this, t);
					}

					@Override
					protected void onDownloadNecessary() {}

					@Override
					protected void onDownloadStarted() {
						loadingView.setIndeterminate(R.string.download_downloading);
					}

					@Override
					protected void onFailure(@RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
						final RRError error = General.getGeneralErrorForFailure(CaptchaActivity.this, type, t, status, url.toString());
						General.showResultDialog(CaptchaActivity.this, error);
					}

					@Override
					protected void onProgress(final boolean authorizationInProgress, long bytesRead, long totalBytes) {
						if(authorizationInProgress) {
							loadingView.setIndeterminate(R.string.download_authorizing);
						} else {
							loadingView.setProgress(R.string.download_downloading, (float)((double)bytesRead / (double)totalBytes));
						}
					}

					@Override
					protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, String mimetype) {

						final Bitmap image;
						try {
							image = BitmapFactory.decodeStream(cacheFile.getInputStream());
						} catch(IOException e) {
							BugReportActivity.handleGlobalError(CaptchaActivity.this, e);
							return;
						}

						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {

								final LinearLayout ll = new LinearLayout(CaptchaActivity.this);
								ll.setOrientation(LinearLayout.VERTICAL);

								final ImageView captchaImg = new ImageView(CaptchaActivity.this);
								ll.addView(captchaImg);
								final LinearLayout.LayoutParams layoutParams = (LinearLayout.LayoutParams) captchaImg.getLayoutParams();
								layoutParams.setMargins(20, 20, 20, 20);
								layoutParams.height = General.dpToPixels(context, 100);
								captchaImg.setScaleType(ImageView.ScaleType.FIT_CENTER);


								final EditText captchaText = new EditText(CaptchaActivity.this);
								ll.addView(captchaText);
								((LinearLayout.LayoutParams) captchaText.getLayoutParams()).setMargins(20, 0, 20, 20);
								captchaText.setInputType(android.text.InputType.TYPE_CLASS_TEXT | android.text.InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD | InputType.TYPE_TEXT_FLAG_CAP_CHARACTERS);

								captchaImg.setImageBitmap(image);

								final Button submitButton = new Button(CaptchaActivity.this);
								submitButton.setText(R.string.post_captcha_submit_button);
								ll.addView(submitButton);
								((LinearLayout.LayoutParams) submitButton.getLayoutParams()).setMargins(20, 0, 20, 20);
								((LinearLayout.LayoutParams) submitButton.getLayoutParams()).gravity = Gravity.RIGHT;
								((LinearLayout.LayoutParams) submitButton.getLayoutParams()).width = LinearLayout.LayoutParams.WRAP_CONTENT;

								submitButton.setOnClickListener(new View.OnClickListener() {
									@Override
									public void onClick(View v) {
										final Intent result = new Intent();
										result.putExtra("captchaId", captchaId);
										result.putExtra("captchaText", captchaText.getText().toString());
										setResult(RESULT_OK, result);
										finish();
									}
								});

								final ScrollView sv = new ScrollView(CaptchaActivity.this);
								sv.addView(ll);
								setBaseActivityContentView(sv);
							}
						});

					}
				});
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(CaptchaActivity.this, t);
			}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				final RRError error = General.getGeneralErrorForFailure(CaptchaActivity.this, type, t, status, null);
				General.showResultDialog(CaptchaActivity.this, error);
			}

			@Override
			protected void onFailure(APIFailureType type) {
				final RRError error = General.getGeneralErrorForFailure(CaptchaActivity.this, type);
				General.showResultDialog(CaptchaActivity.this, error);
			}
		}, selectedAccount, this);
	}
}
