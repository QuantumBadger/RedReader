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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.Log;
import android.view.*;
import android.view.animation.Animation;
import android.view.animation.TranslateAnimation;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;

public class RedditCommentView extends LinearLayout{

	private RedditPreparedComment comment;

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final IndentView indent;

	private final int bodyCol;
	private final float fontScale;

	private final boolean showLinkButtons;

	private CommentListingFragment fragment;

	private float initx, inity;

	public RedditCommentView(final Context context, final CommentListingFragment fragment, final int headerCol, final int bodyCol) {

		super(context);
		this.bodyCol = bodyCol;
		setOrientation(HORIZONTAL);

		this.fragment = fragment;

		LinearLayout main = new LinearLayout(context){
			@Override
			public boolean onInterceptTouchEvent(MotionEvent m){
				//Steal children's touch events to give main the whole touch area
				return true;
			}
		};
		main.setOrientation(VERTICAL);

		fontScale = PrefsUtility.appearance_fontscale_comments(context, PreferenceManager.getDefaultSharedPreferences(context));

		header = new TextView(context);
		header.setTextSize(11.0f * fontScale);
		header.setTextColor(headerCol);
		main.addView(header);

		bodyHolder = new FrameLayout(context);
		bodyHolder.setPadding(0, General.dpToPixels(context, 2), 0, 0);
		main.addView(bodyHolder);
		bodyHolder.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final int paddingPixelsVertical = General.dpToPixels(context, 8.0f);
		final int paddingPixelsHorizontal = General.dpToPixels(context, 12.0f);
		main.setPadding(paddingPixelsHorizontal, paddingPixelsVertical, paddingPixelsHorizontal, paddingPixelsVertical);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		indent = new IndentView(context);
		addView(indent);
		indent.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		addView(main);
		main.getLayoutParams().width = LinearLayout.LayoutParams.MATCH_PARENT;

		showLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, PreferenceManager.getDefaultSharedPreferences(context));

		this.setOnTouchListener(new SwipeTouchListener(context) {
			@Override
			public boolean onTouch(View v, MotionEvent m) {
				switch (m.getAction()) {
					case MotionEvent.ACTION_DOWN:
						requestDisallowInterceptTouchEvent(true);
						initx = m.getX();
						inity = m.getY();
						setBackgroundColor(Color.BLUE);
						break;
					case MotionEvent.ACTION_UP:
						setBackgroundColor(Color.WHITE);
						break;
					case MotionEvent.ACTION_CANCEL:
						requestDisallowInterceptTouchEvent(false);
						setBackgroundColor(Color.WHITE);
						break;
					case MotionEvent.ACTION_MOVE:
						float disx = m.getX() - initx;
						float disy = m.getX() - inity;
						initx += disx;
						inity += disy;
						//Give parent control back( for scroll ) if moving vertically
						if(Math.abs(disy)>Math.abs(disx) && Math.abs(disy) > 75){
							requestDisallowInterceptTouchEvent(false);
						}
						break;
				}

				Log.d("RedditCommentView", "Touch:");
				return super.onTouch(v, m);
			}

			@Override
			public void longPress() {
				Log.d("RedditCommentView", "Long press");
				//Only set context menu in fragment when called upon
				fragment.registerForContextMenu(RedditCommentView.this);
				fragment.openContextMenu(RedditCommentView.this);
				fragment.unregisterForContextMenu(RedditCommentView.this);
			}

			@Override
			public void swipeLeft() {
				comment.bind(RedditCommentView.this);
				TranslateAnimation anim = new TranslateAnimation(0, -getWidth(), 0,  0);
				anim.setDuration(750);
				anim.setAnimationListener(new Animation.AnimationListener() {
					@Override
					public void onAnimationStart(Animation animation) {
						Log.d("RedditCommentView","animation start");
					}

					@Override
					public void onAnimationRepeat(Animation animation) {
						// TODO Auto-generated method stub

					}

					@Override
					public void onAnimationEnd(Animation animation) {
						Log.d("RedditCommentView","animation end");

					}
				});
				startAnimation(anim);
				if(comment.isDownvoted()) {
					Log.d ("RedditCommentView", "Already Downvoted");
					comment.action(fragment.getSupportActivity(), RedditAPI.RedditAction.UNVOTE);
				} else {
					Log.d ("RedditCommentView", "Need to Downvote");
					comment.action(fragment.getSupportActivity(), RedditAPI.RedditAction.DOWNVOTE);
				}

				updateAppearance();
			}

			@Override
			public void swipeRight() {
				Log.d("RedditCommentView", "Swiped right");

				TranslateAnimation anim = new TranslateAnimation(0, getWidth(), 0, 0);
				anim.setDuration(750);
				anim.setAnimationListener(new Animation.AnimationListener() {
					@Override
					public void onAnimationStart(Animation animation) {
						Log.d("RedditCommentView","animation start");
					}

					@Override
					public void onAnimationRepeat(Animation animation) {
						// TODO Auto-generated method stub

					}

					@Override
					public void onAnimationEnd(Animation animation) {
						Log.d("RedditCommentView","animation end");

					}
				});
				startAnimation(anim);
					Log.d("RedditCommentView", "Must UNVOTE");
					comment.action(fragment.getSupportActivity(), RedditAPI.RedditAction.UNVOTE);
				}else{
					Log.d("RedditCommentView", "Must UPVOTE");
					comment.action(fragment.getSupportActivity(), RedditAPI.RedditAction.UPVOTE);
				}

				updateAppearance();
			}
		});
	}

	public void reset(final Activity activity, final RedditPreparedComment comment) {

		if(this.comment != null) this.comment.unbind(this);

		this.comment = comment;
		comment.bind(this);

		indent.setIndentation(comment.indentation);

		if(!comment.isCollapsed()) {
			header.setText(comment.header);
		} else {
			header.setText("[ + ]  " + comment.header);
		}

		final boolean hideLinkButtons = comment.src.author.equalsIgnoreCase("autowikibot");

		bodyHolder.removeAllViews();
		final ViewGroup commentBody = comment.getBody(activity, 13.0f * fontScale, bodyCol, showLinkButtons && !hideLinkButtons);

		bodyHolder.addView(commentBody);
		commentBody.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		((MarginLayoutParams)commentBody.getLayoutParams()).topMargin = General.dpToPixels(activity, 1);

		updateVisibility(activity);
	}

	private void updateVisibility(final Context context) {

		if(comment.isCollapsed()) {

			bodyHolder.setVisibility(GONE);

			if(comment.replyCount() == 1) {
				header.setText(String.format("[ + ] %s (1 %s)", comment.header, context.getString(R.string.subtitle_reply)));
			} else {
				header.setText(String.format("[ + ] %s (%d %s)", comment.header, comment.replyCount(), context.getString(R.string.subtitle_replies)));
			}

		} else {
			bodyHolder.setVisibility(VISIBLE);
			header.setText(comment.header);
		}
	}

	public boolean handleVisibilityToggle() {
		comment.toggleVisibility();
		updateVisibility(getContext());
		return comment.isCollapsed();
	}

	public RedditPreparedComment getComment() {
		return comment;
	}

	public void updateAppearance() {
		header.setText(comment.header);
	}

	/**
	 * Draws the left margin for comments based on the
	 * RedditPreparedComment#indentation number
	 *
	 * @author Gabriel Castro &lt;dev@GabrielCastro.ca&gt;
	 */
	private static class IndentView extends View {

		private final Paint mPaint = new Paint();
		private int mIndent;

		private final int mPixelsPerIndent;
		private final int mPixelsPerLine;
		private final float mHalfALine;

		private final boolean mPrefDrawLines;

		public IndentView(Context context) {
			super(context);

			mPixelsPerIndent = General.dpToPixels(context, 10.0f);
			mPixelsPerLine = General.dpToPixels(context, 2);
			mHalfALine = mPixelsPerLine / 2;

			this.setBackgroundColor(Color.argb(20, 128, 128, 128));
			mPaint.setColor(Color.argb(75, 128, 128, 128));
			mPaint.setStrokeWidth(mPixelsPerLine);

			mPrefDrawLines = PrefsUtility.pref_appearance_indentlines(context, PreferenceManager.getDefaultSharedPreferences(context));
		}

		@Override
		protected void onDraw(final Canvas canvas) {

			super.onDraw(canvas);

			final int height = getMeasuredHeight();

			if(mPrefDrawLines) {
				final float[] lines = new float[mIndent * 4];
				float x;
				// i keeps track of indentation, and
				// l is to populate the float[] with line co-ordinates
				for (int i = 0, l = 0; i < mIndent; ++l) {
					x = (mPixelsPerIndent * ++i) - mHalfALine;
					lines[l]   = x;      // start-x
					lines[++l] = 0;      // start-y
					lines[++l] = x;      // stop-x
					lines[++l] = height; // stop-y
				}
				canvas.drawLines(lines, mPaint);

			} else {
				final float rightLine = getWidth() - mHalfALine;
				canvas.drawLine(rightLine, 0, rightLine, getHeight(), mPaint);
			}
		}

		/**
		 * Sets the indentation for the View
		 * @param indent comment indentation number
		 */
		public void setIndentation(int indent) {
			this.getLayoutParams().width = (mPixelsPerIndent * indent);
			this.mIndent = indent;
			this.invalidate();
		}
	}

	private class SwipeTouchListener implements View.OnTouchListener {
		private static final int SWIPE_DISTANCE = 75;
		private static final int SWIPE_VELOCITY = 60;

		private final GestureDetector gD;

		@Override
		public boolean onTouch(View v, MotionEvent m){
			return gD.onTouchEvent(m);
		}

		public SwipeTouchListener(Context context){
			gD = new GestureDetector(context, new GestureListener());
		}

		public void swipeRight(){
			System.out.println("Comment swipe right");
		}

		public void swipeLeft(){
			System.out.println("Comment swipe left");
		}

		public void longPress(){

		}

		private final class GestureListener extends GestureDetector.SimpleOnGestureListener{

			@Override
			public void onLongPress(MotionEvent m){
				longPress();
				super.onLongPress(m);
			}

			@Override
			public boolean onDown(MotionEvent m1){
				return true;
			}

			@Override
			public boolean onFling(MotionEvent m1, MotionEvent m2, float vx, float vy) {
				Log.d("RedditCommentView", "Fling");
				try {
					float disx = m2.getX() - m1.getX();
					float disy = m2.getY() - m1.getY();
					float absdisx = Math.abs(disx);
					float absdisy = Math.abs(disy);
					if (absdisx > SWIPE_DISTANCE && absdisx > absdisy && Math.abs(vx) > SWIPE_VELOCITY) {
						if (disx > 0) {
							swipeRight();
						} else {
							swipeLeft();
						}
					} else {
						return false;
					}
					return true;
				} catch (Exception e){
					Log.d("RedditCommentView",e.getMessage());
				}
				return false;
			}
		}
	}


}
