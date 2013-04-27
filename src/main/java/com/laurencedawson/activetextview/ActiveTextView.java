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

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.text.*;
import android.text.method.LinkMovementMethod;
import android.text.style.ClickableSpan;
import android.text.style.StyleSpan;
import android.text.style.URLSpan;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class ActiveTextView extends TextView {

	public ActiveTextView(final Context context) {
		super(context);
		setup();
	}

	public ActiveTextView(final Context context, AttributeSet attrs) {
		super(context, attrs);
		setup();
	}

	public ActiveTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setup();
	}

	private boolean mLinkSet, mDisplayMinLongPress;
	private String mUrl;
	private SpannableStringBuilder mSpannable;
	private ActiveTextView.OnLinkClickedListener mListener;
	private ActiveTextView.OnLongPressedLinkListener mLongPressedLinkListener;
	private Object attachment;

	public void setAttachment(final Object attachment) {
		this.attachment = attachment;
	}

	private void setup() {

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
											Intent i = new Intent(Intent.ACTION_VIEW);
											i.setData(Uri.parse(mUrl));
											getContext().startActivity(i);
										}
									}
								}
								Selection.removeSelection(buffer);
								return true;
							} else if(action == MotionEvent.ACTION_DOWN) {
								Selection.setSelection(buffer, buffer.getSpanStart(link[0]), buffer.getSpanEnd(link[0]));
								URLSpan s = (URLSpan) link[0];
								mUrl = s.getURL();
								mLinkSet = true;
								return true;
							}
							return false;
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

			public boolean onLongClick(View v) {

				if(mLongPressedLinkListener != null) {
					if(isLinkPending()) {
// Create the dialog
						AlertDialog.Builder builder = new AlertDialog.Builder(getContext());
						builder.setItems(

								mDisplayMinLongPress ?
										new String[]{"Open in browser", "Copy link address", "Share link"} // TODO strings
										: new String[]{"Open in browser", "Copy link address", "Share link", "Long press parent"},

								new DialogInterface.OnClickListener() {
									@SuppressWarnings("deprecation")
									public void onClick(DialogInterface dialog, int item) {
										if(item == 0) {
											Intent i = new Intent(Intent.ACTION_VIEW);
											i.setData(Uri.parse(mUrl));
											getContext().startActivity(i);
										} else if(item == 1) {
											if(android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.HONEYCOMB) {
												android.text.ClipboardManager clipboard = (android.text.ClipboardManager) getContext().getSystemService(Context.CLIPBOARD_SERVICE);
												clipboard.setText(mUrl);
											} else {
												android.content.ClipboardManager clipboard = (android.content.ClipboardManager) getContext().getSystemService(Context.CLIPBOARD_SERVICE);
												android.content.ClipData clip = android.content.ClipData.newPlainText("Link", mUrl);
												clipboard.setPrimaryClip(clip);
											}
										} else if(item == 2) {
											Intent share = new Intent(android.content.Intent.ACTION_SEND);
											share.putExtra(android.content.Intent.EXTRA_SUBJECT, mUrl);
											share.putExtra(android.content.Intent.EXTRA_TEXT, mUrl);
											share.setType("text/plain");
											getContext().startActivity(Intent.createChooser(share, "Share"));
										} else if(item == 3) {
											mLongPressedLinkListener.onLongPressed();
										}
									}
								});

						final AlertDialog alert = builder.create();
						alert.setTitle(mUrl);
						alert.setCanceledOnTouchOutside(true);
						alert.show();
					} else
						mLongPressedLinkListener.onLongPressed();

					cancelLink();
					return true;
				} else {
					cancelLink();
					return false;
				}
			}
		});

// Provide an easier interface for the parent view
		setOnClickListener(new OnClickListener() {
			public void onClick(View v) {
				if(!isLinkPending() && mListener != null)
					mListener.onClickText(attachment);
				cancelLink();
			}
		});
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
		public void onClickUrl(String url);
		public void onClickText(Object attachment);
	}

	// Called when a link in long clicked
	public interface OnLongPressedLinkListener {
		void onLongPressed();
	}

	/**
	 * Set a link click listener, this is called when a user clicks on a link
	 *
	 * @param clickListener The click listener to call when a link is clicked
	 */
	public void setLinkClickedListener(ActiveTextView.OnLinkClickedListener clickListener) {
		this.mListener = clickListener;
	}

	/**
	 * Set a long press listener, this is called when a user long presses on a link
	 * a small submenu with a few options is then displayed
	 *
	 * @param longPressedLinkListener Sets the long press listener to call when "Long press parent" is called
	 * @param minDisplay              Enable a smaller submenu when long pressed (removes the option Long press parent)
	 */
	public void setLongPressedLinkListener(ActiveTextView.OnLongPressedLinkListener longPressedLinkListener, boolean minDisplay) {
		this.mLongPressedLinkListener = longPressedLinkListener;
		this.mDisplayMinLongPress = minDisplay;
	}
}
