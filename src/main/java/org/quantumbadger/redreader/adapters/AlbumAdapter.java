package org.quantumbadger.redreader.adapters;

import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.viewholders.VH2TextIcon;

import java.io.IOException;
import java.util.UUID;

public class AlbumAdapter extends RecyclerView.Adapter<VH2TextIcon> {

	private final AppCompatActivity activity;
	private final ImgurAPI.AlbumInfo albumInfo;

	public AlbumAdapter(final AppCompatActivity activity, final ImgurAPI.AlbumInfo albumInfo) {
		this.activity = activity;
		this.albumInfo = albumInfo;
	}

	@Override
	public VH2TextIcon onCreateViewHolder(ViewGroup parent, int viewType) {
		View v = LayoutInflater.from(parent.getContext())
			.inflate(R.layout.list_item_2_text_icon, parent, false);
		return new VH2TextIcon(v);
	}

	@Override
	public void onBindViewHolder(final VH2TextIcon vh, final int position) {
		final ImageInfo imageInfo = albumInfo.images.get(position);

		if(imageInfo.title == null || imageInfo.title.trim().isEmpty()) {
			vh.text.setText("Image " + (position + 1));
		} else {
			vh.text.setText((position + 1) + ". " + imageInfo.title.trim());
		}

		String subtitle = "";

		if(imageInfo.type != null) {
			subtitle += imageInfo.type;
		}

		if(imageInfo.width != null && imageInfo.height != null) {
			if(!subtitle.isEmpty()) subtitle += ", ";
			subtitle += imageInfo.width + "x" + imageInfo.height;
		}

		if(imageInfo.size != null) {
			if(!subtitle.isEmpty()) subtitle += ", ";

			long size = imageInfo.size;
			if(size < 512 * 1024) {
				subtitle += String.format("%.1f kB", (float)size / 1024);
			} else {
				subtitle += String.format("%.1f MB", (float)size / (1024 * 1024));
			}
		}

		vh.text2.setVisibility(subtitle.isEmpty() ? View.GONE : View.VISIBLE);

		vh.text2.setText(subtitle);

		vh.icon.setImageBitmap(null);

		final boolean isConnectionWifi = General.isConnectionWifi(activity);

		final PrefsUtility.AppearanceThumbnailsShow thumbnailsPref = PrefsUtility.appearance_thumbnails_show(
			activity,
			PreferenceManager.getDefaultSharedPreferences(activity));

		final boolean downloadThumbnails = thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.ALWAYS
			|| (thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.WIFIONLY && isConnectionWifi);

		if(!downloadThumbnails || imageInfo.urlBigSquare == null) {
			vh.icon.setVisibility(View.GONE);

		} else {
			vh.text2.setVisibility(View.VISIBLE);

			CacheManager.getInstance(activity).makeRequest(new CacheRequest(
				General.uriFromString(imageInfo.urlBigSquare),
				RedditAccountManager.getAnon(),
				null,
				Constants.Priority.THUMBNAIL,
				position,
				CacheRequest.DOWNLOAD_IF_NECESSARY,
				Constants.FileType.THUMBNAIL,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				false,
				false,
				activity
			) {
				@Override
				protected void onCallbackException(final Throwable t) {
					Log.e("AlbumAdapter", "Error in album thumbnail fetch callback", t);
				}

				@Override
				protected void onDownloadNecessary() {}

				@Override
				protected void onDownloadStarted() {}

				@Override
				protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
					Log.e("AlbumAdapter", "Failed to fetch thumbnail " + url.toString());
				}

				@Override
				protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

				@Override
				protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
					// TODO post message rather than runnable
					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {
							try {
								vh.icon.setImageURI(cacheFile.getUri());
							} catch(IOException e) {
								throw new RuntimeException(e);
							}
						}
					});
				}
			});
		}

		vh.itemView.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				LinkHandler.onLinkClicked(activity, imageInfo.urlOriginal, false, null,
					albumInfo, vh.getAdapterPosition());
			}
		});
	}

	@Override
	public int getItemCount() {
		return albumInfo.images.size();
	}
}
