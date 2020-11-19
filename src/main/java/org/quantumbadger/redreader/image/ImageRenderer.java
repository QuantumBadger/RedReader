package org.quantumbadger.redreader.image;

import android.graphics.Bitmap;
import android.graphics.Rect;
import android.graphics.drawable.BitmapDrawable;
import android.net.Uri;
import android.os.Process;
import android.provider.MediaStore;
import android.util.Log;
import android.widget.ImageView;

import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;

import java.io.IOException;

public class ImageRenderer {

	private static final float MAX_WIDTH = 800.0f;
	private static final float MAX_HEIGHT = 800.0f;

	private Thread thread;
	private boolean stopped;
	private long mImageStartRender;
	public ImageRenderer(
			Uri fileUri,
			ImageView inlineImageView,
			RedditPreparedPost post,
			BaseActivity mActivity) {

		mImageStartRender = System.currentTimeMillis();
		float dpScale = mActivity.getResources().getDisplayMetrics().density;

		if (fileUri == null){
			return;
		}

		thread = new Thread("Image rendering thread"){
			@Override
			public void run() {

				Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND);

				if (stopped){
					return;
				}

				Rect windowSize = new Rect ();
				mActivity.getWindow().getDecorView().getWindowVisibleDisplayFrame(windowSize);

				final int availableHeight = Math.min((int)(MAX_HEIGHT * dpScale), (int)windowSize.height());
				final int availableWidth = Math.min((int)(MAX_WIDTH * dpScale), (int)windowSize.width());


				final Bitmap rawBitmap;
				try {
					rawBitmap = MediaStore.Images.Media.getBitmap(mActivity.getContentResolver(), fileUri);
				} catch (IOException e) {
					e.printStackTrace();
					return;
				}

				if (rawBitmap == null){
					return;
				}

				final Bitmap scaledBitmap =
						ThumbnailScaler.scaleToFit(
								rawBitmap,
								availableWidth,
								availableHeight);

				final BitmapDrawable result = new BitmapDrawable(mActivity.getResources(), scaledBitmap);
				if (stopped){
					return;
				}
				mActivity.runOnUiThread(()->{
					if (stopped){
						return;
					}
					if (inlineImageView != null) {
						inlineImageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
						inlineImageView.setImageDrawable(result);

						int height = result.getIntrinsicHeight();
						inlineImageView.setMinimumHeight(height);

						if (post != null) {
							post.lastImageHeight = height;
						}

						long totalMilis = System.currentTimeMillis() - mImageStartRender;
						Log.d("ImageRenderer", "Took " + totalMilis + "ms to render");

					}
				});
			}
		};

		thread.start();
	}

	public void stop() {
		stopped = true;
	}
}
