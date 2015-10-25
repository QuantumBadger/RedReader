package org.quantumbadger.redreader.adapters;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.views.ImgurAlbumListEntryView;

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
	public ImageInfo getItem(final int position) {
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
