package org.quantumbadger.redreader.views;

import android.app.Activity;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.View;

import org.quantumbadger.redreader.R;

/**
 * Created by Mario Kosmiskas on 4/28/17.
 */

public class PreviewView extends android.support.v7.widget.AppCompatImageView {

	public PreviewView(Context context) {
		super(context, null);
	}

	public PreviewView(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public PreviewView(Context context, AttributeSet attrs, int defStyleAttr) {
		super(context, attrs, defStyleAttr);
	}

	/**
	 * Cap the height of the preview image to fit on the screen
	 *
	 * @param widthMeasureSpec
	 * @param heightMeasureSpec
	 */
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
		Drawable d = getDrawable();

		super.onMeasure(widthMeasureSpec, heightMeasureSpec);

		// Container for posts
		// TODO: Find a better way to locate this view, this is too brittle
		View allPostsContainer = (View)this.getParent().getParent().getParent();

		// Get the height of the post description
		int postHeight = 0;
		if (getContext() instanceof Activity) {
			Activity activity = (Activity)getContext();
			View postView = activity.findViewById(R.id.reddit_post_layout);
			postHeight = postView.getHeight();
		}

		// Calculate the desired preview height maintaining the aspect ratio
		int parentWidth = MeasureSpec.getSize(widthMeasureSpec);
		int newH = (int)((float)parentWidth * d.getIntrinsicHeight() / d.getIntrinsicWidth());

		int maxHeight = allPostsContainer.getHeight() - postHeight;
		this.setMeasuredDimension(parentWidth, Math.min(newH, maxHeight));
	}

}
