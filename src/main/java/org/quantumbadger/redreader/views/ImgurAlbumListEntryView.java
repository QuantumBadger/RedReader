package org.quantumbadger.redreader.views;

import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.util.TypedValue;
import android.view.Gravity;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.image.ImgurAPI;

import java.io.IOException;
import java.util.UUID;

public class ImgurAlbumListEntryView extends LinearLayout {

	private final ImageView mThumbnail;
	private final TextView mTitle;
	private final TextView mSubtitle;

	private ImgurAPI.ImageInfo mImageInfo;

	public ImgurAlbumListEntryView(final Context context) {

		super(context);

		setOrientation(HORIZONTAL);

		mThumbnail = new ImageView(context);
		mTitle = new TextView(context);
		mSubtitle = new TextView(context);

		final LinearLayout textLayout = new LinearLayout(context);
		textLayout.setOrientation(VERTICAL);

		textLayout.addView(mTitle);
		textLayout.addView(mSubtitle);

		addView(mThumbnail);
		addView(textLayout);

		textLayout.setGravity(Gravity.CENTER_VERTICAL);
		setGravity(Gravity.CENTER_VERTICAL);

		final int thumbnailSizePx = General.dpToPixels(context, 64);
		mThumbnail.getLayoutParams().width = thumbnailSizePx;
		mThumbnail.getLayoutParams().height = thumbnailSizePx;

		final int paddingSidesPx = General.dpToPixels(context, 15.0f);
		final int paddingTopBottomPx = General.dpToPixels(context, 10.0f);
		final int paddingSeparationPx = General.dpToPixels(context, 3.0f);

		textLayout.setPadding(paddingSidesPx, paddingTopBottomPx, paddingSidesPx, paddingTopBottomPx);
		mSubtitle.setPadding(0, paddingSeparationPx, 0, 0);

		mTitle.setTextSize(TypedValue.COMPLEX_UNIT_SP, 18);
		mSubtitle.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14);
		mSubtitle.setTextColor(Color.rgb(0x90, 0x90, 0x90));
	}

	public void reset(final int listPosition, final ImgurAPI.ImageInfo info) {

		mImageInfo = info;

		if(info.title == null || info.title.trim().isEmpty()) {
			mTitle.setText("Image " + listPosition);
		} else {
			mTitle.setText(listPosition + ". " + info.title.trim());
		}

		String subtitle = "";

		if(info.type != null) {
			subtitle += info.type;
		}

		if(info.width != null && info.height != null) {

			if(!subtitle.isEmpty()) subtitle += ", ";

			subtitle += info.width + "x" + info.height;
		}

		if(info.size != null) {

			if(!subtitle.isEmpty()) subtitle += ", ";

			long size = info.size;

			if(size < 512 * 1024) {
				subtitle += String.format("%.1f kB", (float)size / 1024);
			} else {
				subtitle += String.format("%.1f MB", (float)size / (1024 * 1024));
			}
		}

		if(subtitle.isEmpty()) {
			mSubtitle.setVisibility(GONE);
		} else {
			mSubtitle.setVisibility(VISIBLE);
		}

		mSubtitle.setText(subtitle);

		mThumbnail.setImageBitmap(null);

		// TODO check thumbnail preference
		CacheManager.getInstance(getContext()).makeRequest(new CacheRequest(
				General.uriFromString(info.urlBigSquare),
				RedditAccountManager.getAnon(),
				null,
				Constants.Priority.THUMBNAIL,
				listPosition,
				CacheRequest.DownloadType.IF_NECESSARY,
				Constants.FileType.THUMBNAIL,
				false,
				false,
				false,
				getContext()
		) {
			@Override
			protected void onCallbackException(final Throwable t) {
				Log.e("ImgurAlbumListEntryView", "Error in album thumbnail fetch callback", t);
			}

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
				Log.e("ImgurAlbumListEntryView", "Failed to fetch thumbnail " + url.toString());
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {

				if(mImageInfo != info) return;

				// TODO post message rather than runnable
				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {
						try {
							mThumbnail.setImageURI(cacheFile.getUri());
						} catch(IOException e) {
							throw new RuntimeException(e);
						}
					}
				});
			}
		});
	}


}
