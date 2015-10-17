package com.konneh.scroll.adapters;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import com.konneh.scroll.image.ImgurAPI;
import com.konneh.scroll.views.ImgurAlbumListEntryView;

public class AlbumAdapter extends BaseAdapter {

	private final ImgurAPI.AlbumInfo mAlbumInfo;

	public AlbumAdapter(final ImgurAPI.AlbumInfo albumInfo) {
		mAlbumInfo = albumInfo;
	}

	@Override
	public int getCount() {
		return mAlbumInfo.images.size();
	}

	@Override
	public ImgurAPI.ImageInfo getItem(final int position) {
		return mAlbumInfo.images.get(position);
	}

	@Override
	public long getItemId(final int position) {
		return position;
	}

	@Override
	public View getView(final int position, View convertView, final ViewGroup parent) {

		if(convertView == null) {
			convertView = new ImgurAlbumListEntryView(parent.getContext());
		}

		((ImgurAlbumListEntryView)convertView).reset(position, getItem(position));

		return convertView;
	}
}
