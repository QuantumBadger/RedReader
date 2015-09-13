package org.quantumbadger.redreader.views;

import android.content.Context;
import android.graphics.Color;
import android.util.TypedValue;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.image.ImgurAPI;

public class ImgurAlbumListEntryView extends LinearLayout {

	private final TextView mTitle;
	private final TextView mSubtitle;

	private ImgurAPI.ImageInfo mImageInfo;

	public ImgurAlbumListEntryView(final Context context) {

		super(context);

		setOrientation(VERTICAL);

		mTitle = new TextView(context);
		mSubtitle = new TextView(context);

		addView(mTitle);
		addView(mSubtitle);

		final int paddingSidesPx = General.dpToPixels(context, 10.0f);
		final int paddingTopBottomPx = General.dpToPixels(context, 10.0f);
		final int paddingSeparationPx = General.dpToPixels(context, 3.0f);

		setPadding(paddingSidesPx, paddingTopBottomPx, paddingSidesPx, paddingTopBottomPx);
		mSubtitle.setPadding(0, paddingSeparationPx, 0, 0);

		mTitle.setTextSize(TypedValue.COMPLEX_UNIT_SP, 18);
		mSubtitle.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14);
		mSubtitle.setTextColor(Color.rgb(0x90, 0x90, 0x90));
	}

	public void reset(final int listPosition, final ImgurAPI.ImageInfo info) {

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
	}


}
