/*
* Copyright 2012 Laurence Dawson
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

/*
 * (Slightly) altered version of Laurence Dawson's ActiveTextView
 *
 * Original at: https://github.com/laurencedawson/activetextview/blob/master/library/src/com/laurencedawson/activetextview/ActiveTextView.java
 */

package com.laurencedawson.activetextview;

import android.annotation.SuppressLint;
import android.support.v7.app.AppCompatActivity;
import android.text.Layout;
import android.text.Selection;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.SpannedString;
import android.text.method.LinkMovementMethod;
import android.text.style.ClickableSpan;
import android.text.style.StyleSpan;
import android.text.style.URLSpan;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewParent;
import android.widget.Button;
import android.widget.TextView;

import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.views.RedditCommentView;
import org.quantumbadger.redreader.views.RedditInboxItemView;

public class ActiveTextView extends TextView {

	public ActiveTextView(final AppCompatActivity activity) {
		super(activity);
		setup(activity);
	}

	private boolean mLinkSet, mDisplayMinLongPress;
	private String mUrl;
	private SpannableStringBuilder mSpannable;
	private OnLinkClickedListener mListener;
	private OnLongPressedLinkListener mLongPressedLinkListener;
	private Object attachment;

	public void setAttachment(final Object attachment) {
		this.attachment = attachment;
	}

	private void setup(final AppCompatActivity activity) {

		mSpannable = new SpannableStringBuilder();

		// Set the movement method

		setMovementMethod(new LinkMovementMethod() {
			@Override
			public boolean onTouchEvent(TextView widget, Spannable buffer, MotionEvent event) {

				int action = event.getAction();

				if(action == MotionEvent.ACTION_UP || action == MotionEvent.ACTION_DOWN) {

					int x = (int) event.getX();
					int y = (int) event.getY();

					x -= widget.getTotalPaddingLeft();
					y -= widget.getTotalPaddingTop();

					x += widget.getScrollX();
					y += widget.getScrollY();

					Layout layout = widget.getLayout();
					int line = layout.getLineForVertical(y);
					int off = layout.getOffsetForHorizontal(line, x);
					float maxLineRight = layout.getLineWidth(line);

					// Stops the space after a link being clicked
					if(x <= maxLineRight) {
						ClickableSpan[] link = buffer.getSpans(off, off, ClickableSpan.class);

						if(link.length != 0) {
							if(action == MotionEvent.ACTION_UP) {
								// If a link click listener is set, call that
								// Otherwise just open the link
								if(mLinkSet) {
									if(mListener != null)
										mListener.onClickUrl(mUrl);
									else {
										if(mUrl != null) {
											LinkHandler.onLinkClicked(activity, mUrl, false);
										}
									}
								}
								Selection.removeSelection(buffer);
								return true;

							} else {
								Selection.setSelection(buffer, buffer.getSpanStart(link[0]), buffer.getSpanEnd(link[0]));
								URLSpan s = (URLSpan) link[0];
								mUrl = s.getURL();
								mLinkSet = true;
								return true;
							}

						} else {
							Selection.removeSelection(buffer);
						}
					}
				}

				return false;
			}
		});

		setLongClickable(true);
		setFocusable(false);
		setClickable(false);

		// If a long press is detected, cancel the potential opening of a link
		setOnLongClickListener(new OnLongClickListener() {

			@SuppressLint("NewApi")
			public boolean onLongClick(View v) {

				cancelLink();

				final ViewParent redditView = findRedditView();

				if(redditView != null) {

					if(redditView instanceof RedditCommentView) {
						((RedditCommentView)redditView).notifyLongClick();

					} else {
						((RedditInboxItemView)redditView).handleInboxLongClick(activity);
					}
				}

				return true;
			}
		});

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {

				if(!isLinkPending()) {
					if(mListener != null) mListener.onClickText(attachment);

					final ViewParent redditView = findRedditView();

					if(redditView != null) {

						if(redditView instanceof RedditCommentView) {
							((RedditCommentView)redditView).notifyClick();

						} else {
							((RedditInboxItemView)redditView).handleInboxClick(activity);
						}
					}
				}

				cancelLink();
			}
		});
	}

	private ViewParent findRedditView() {

		ViewParent redditView = getParent();

		while(redditView != null
				&& !(redditView instanceof RedditCommentView || redditView instanceof RedditInboxItemView)) {
			redditView = redditView.getParent();
		}

		return redditView;
	}

	private boolean isLinkPending() {
		return mLinkSet;
	}

	private void cancelLink() {
		mLinkSet = false;
	}

	// When a link is clicked, stop the view from drawing the touched drawable
	@Override
	public int[] onCreateDrawableState(int extraSpace) {
		int[] states;
		if(mLinkSet) {
			states = Button.EMPTY_STATE_SET;
			return states;
		} else
			return super.onCreateDrawableState(extraSpace);
	}

	// Implemented to stop the following bug with TextViews in Jelly Bean
// http://code.google.com/p/android/issues/detail?id=34872
	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
		try {
			super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		} catch(IndexOutOfBoundsException e) {
			if(getText() instanceof SpannedString) {
				SpannedString s = (SpannedString) getText();
				mSpannable.clear();
				mSpannable.append(s);
				StyleSpan[] a = s.getSpans(0, s.length(), StyleSpan.class);
				if(a.length > 0) {
					mSpannable.removeSpan(a[0]);
					setText(mSpannable);
					onMeasure(widthMeasureSpec, heightMeasureSpec);
				} else
					super.onMeasure(widthMeasureSpec, heightMeasureSpec);
			} else if(getText() instanceof SpannableString) {
				SpannableString s = (SpannableString) getText();
				mSpannable.clear();
				mSpannable.append(s);
				StyleSpan[] a = s.getSpans(0, s.length(), StyleSpan.class);
				if(a.length > 0) {
					mSpannable.removeSpan(a[0]);
					setText(mSpannable);
					onMeasure(widthMeasureSpec, heightMeasureSpec);
				} else
					super.onMeasure(widthMeasureSpec, heightMeasureSpec);
			}
		}
	}

	// Implemented to stop the following bug with TextViews in Jelly Bean
// http://code.google.com/p/android/issues/detail?id=34872
	@Override
	public void setText(CharSequence text, BufferType type) {
		try {
			super.setText(text, type);
		} catch(IndexOutOfBoundsException e) {
			if(text instanceof SpannedString) {
				SpannedString s = (SpannedString) text;
				mSpannable.clear();
				mSpannable.append(text);
				StyleSpan[] a = s.getSpans(0, s.length(), StyleSpan.class);
				if(a.length > 0) {
					mSpannable.removeSpan(a[0]);
					super.setText(mSpannable, type);
				} else
					setText(text.toString());
			} else if(text instanceof SpannableString) {
				SpannableString s = (SpannableString) text;
				mSpannable.clear();
				mSpannable.append(text);
				StyleSpan[] a = s.getSpans(0, s.length(), StyleSpan.class);
				if(a.length > 0) {
					mSpannable.removeSpan(a[0]);
					super.setText(mSpannable, type);
				} else
					setText(text.toString());
			}
		}
	}

	// Called when a link in long clicked
	public interface OnLinkClickedListener {
		void onClickUrl(String url);

		void onClickText(Object attachment);
	}

	// Called when a link in long clicked
	public interface OnLongPressedLinkListener {
		void onLongPressed();
	}
}
