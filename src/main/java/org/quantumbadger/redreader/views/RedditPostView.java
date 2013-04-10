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
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.text.ClipboardManager;
import android.util.TypedValue;
import android.view.ViewGroup;
import android.widget.ImageView;
import org.apache.http.StatusLine;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.Fragment;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.ListView;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.PostPropertiesDialog;
import org.quantumbadger.redreader.fragments.UserProfileDialog;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.views.list.SwipableListItemView;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.UUID;

public final class RedditPostView extends SwipableListItemView implements RedditPreparedPost.ThumbnailLoadedCallback {

	private final float dpScale;

	private RedditPreparedPost post = null;
	private final TextView title, subtitle;

	private final ImageView thumbnailView, savedIcon, hiddenIcon;

	private final LinearLayout commentsButton;
	private final TextView commentsText;

	private int usageId = 0;

	private final Handler thumbnailHandler;

	private final PostListingFragment fragmentParent;

	private boolean swipeReady = false, leftOverlayShown = false, rightOverlayShown = false;

	private final PrefsUtility.PostFlingAction leftFlingPref, rightFlingPref;
	private ActionDescriptionPair leftFlingAction, rightFlingAction;

	private final TextView leftOverlayText, rightOverlayText;

	private final Drawable rrIconFfLeft, rrIconFfRight, rrIconTick;
	private final int rrPostTitleReadCol, rrPostTitleCol;

	private final int offsetBeginAllowed, offsetActionPerformed;

	public static enum Action {
		UPVOTE, UNVOTE, DOWNVOTE, SAVE, HIDE, UNSAVE, UNHIDE, REPORT, SHARE, REPLY, USER_PROFILE, EXTERNAL, PROPERTIES, COMMENTS, LINK, SHARE_COMMENTS, GOTO_SUBREDDIT, ACTION_MENU, SAVE_IMAGE, COPY
	}

	private final class ActionDescriptionPair {
		public final Action action;
		public final int descriptionRes;

		private ActionDescriptionPair(Action action, int descriptionRes) {
			this.action = action;
			this.descriptionRes = descriptionRes;
		}
	}

	@Override
	protected void onSwipeBegin(int xOffsetPixels) {
		if(offsetBeginAllowed > Math.abs(xOffsetPixels)) {

			leftFlingAction = chooseFlingAction(leftFlingPref);
			rightFlingAction = chooseFlingAction(rightFlingPref);

			rightOverlayText.setText(leftFlingAction.descriptionRes);
			leftOverlayText.setText(rightFlingAction.descriptionRes);

			rightOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconFfLeft, null, null);
			leftOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconFfRight, null, null);

			swipeReady = true;
		}
	}

	private ActionDescriptionPair chooseFlingAction(PrefsUtility.PostFlingAction pref) {

		switch(pref) {

			case UPVOTE:
				if(post.isUpvoted()) {
					return new ActionDescriptionPair(Action.UNVOTE, R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(Action.UPVOTE, R.string.action_upvote);
				}

			case DOWNVOTE:
				if(post.isDownvoted()) {
					return new ActionDescriptionPair(Action.UNVOTE, R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(Action.DOWNVOTE, R.string.action_downvote);
				}

			case SAVE:
				if(post.isSaved()) {
					return new ActionDescriptionPair(Action.UNSAVE, R.string.action_unsave);
				} else {
					return new ActionDescriptionPair(Action.SAVE, R.string.action_save);
				}

			case HIDE:
				if(post.isHidden()) {
					return new ActionDescriptionPair(Action.UNHIDE, R.string.action_unhide);
				} else {
					return new ActionDescriptionPair(Action.HIDE, R.string.action_hide);
				}

			case COMMENTS:
				return new ActionDescriptionPair(Action.COMMENTS, R.string.action_comments_short);

			case LINK:
				return new ActionDescriptionPair(Action.LINK, R.string.action_link_short);

			case BROWSER:
				return new ActionDescriptionPair(Action.EXTERNAL, R.string.action_external_short);

			case ACTION_MENU:
				return new ActionDescriptionPair(Action.ACTION_MENU, R.string.action_actionmenu_short);
		}

		return null;
	}

	@Override
	protected void onSwipePositionChange(int xOffsetPixels) {

		final int absOffset = Math.abs(xOffsetPixels);

		if(swipeReady && absOffset > offsetActionPerformed) {

			if(xOffsetPixels > 0) {
				onActionSelected(post, fragmentParent, rightFlingAction.action);
				leftOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			} else {
				onActionSelected(post, fragmentParent, leftFlingAction.action);
				rightOverlayText.setCompoundDrawablesWithIntrinsicBounds(null, rrIconTick, null, null);
			}

			swipeReady = false;

		} else if(absOffset > 5) {

			if(xOffsetPixels > 0) {

				// Right swipe

				if(!leftOverlayShown) {
					leftOverlayShown = true;
					leftOverlayText.setVisibility(VISIBLE);
				}

				if(rightOverlayShown) {
					rightOverlayShown = false;
					rightOverlayText.setVisibility(GONE);
				}

			} else {

				// Left swipe

				if(!rightOverlayShown) {
					rightOverlayShown = true;
					rightOverlayText.setVisibility(VISIBLE);
				}

				if(leftOverlayShown) {
					leftOverlayShown = false;
					leftOverlayText.setVisibility(GONE);
				}
			}

		} else {

			if(leftOverlayShown) {
				leftOverlayShown = false;
				leftOverlayText.setVisibility(GONE);
			}

			if(rightOverlayShown) {
				rightOverlayShown = false;
				rightOverlayText.setVisibility(GONE);
			}
		}
	}

	public RedditPostView(final Context context, final ListView listParent, final PostListingFragment fragmentParent) {

		super(context, listParent);
		this.fragmentParent = fragmentParent;

		offsetBeginAllowed = General.dpToPixels(context, 50);
		offsetActionPerformed = General.dpToPixels(context, 150);

		thumbnailHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {
				if(usageId != msg.what) return;
				thumbnailView.setImageBitmap((Bitmap)msg.obj);
				invalidate(); // TODO is this necessary?
			}
		};

		dpScale = context.getResources().getDisplayMetrics().density; // TODO xml?

		final float fontScale = PrefsUtility.appearance_fontscale_posts(context, PreferenceManager.getDefaultSharedPreferences(context));

		final FrameLayout mainLayout = (FrameLayout) inflate(context, R.layout.reddit_post, null);
		final LinearLayout visiblePostLayout = (LinearLayout) mainLayout.findViewById(R.id.reddit_post_layout);

		thumbnailView = (ImageView) mainLayout.findViewById(R.id.reddit_post_thumbnail_view);
		savedIcon = (ImageView) mainLayout.findViewById(R.id.reddit_post_saved_icon);
		hiddenIcon = (ImageView) mainLayout.findViewById(R.id.reddit_post_hidden_icon);

		title = (TextView) mainLayout.findViewById(R.id.reddit_post_title);
		subtitle = (TextView) mainLayout.findViewById(R.id.reddit_post_subtitle);
		commentsButton = (LinearLayout) mainLayout.findViewById(R.id.reddit_post_comments_button);
		commentsText = (TextView)commentsButton.findViewById(R.id.reddit_post_comments_text);

		title.setTextSize(TypedValue.COMPLEX_UNIT_PX, title.getTextSize() * fontScale);
		subtitle.setTextSize(TypedValue.COMPLEX_UNIT_PX, subtitle.getTextSize() * fontScale);

		leftOverlayText = (TextView) mainLayout.findViewById(R.id.reddit_post_fling_text_left);
		rightOverlayText = (TextView) mainLayout.findViewById(R.id.reddit_post_fling_text_right);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
		leftFlingPref = PrefsUtility.pref_behaviour_fling_post_left(context, sharedPreferences);
		rightFlingPref = PrefsUtility.pref_behaviour_fling_post_right(context, sharedPreferences);

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrIconFfLeft,
				R.attr.rrIconFfRight,
				R.attr.rrIconTick,
				R.attr.rrPostTitleCol,
				R.attr.rrPostTitleReadCol
		});

		rrIconFfLeft = attr.getDrawable(0);
		rrIconFfRight = attr.getDrawable(1);
		rrIconTick = attr.getDrawable(2);
		rrPostTitleCol = attr.getColor(3, 0);
		rrPostTitleReadCol = attr.getColor(4, 0);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		setSwipableView(visiblePostLayout);
		setVisibleView(mainLayout);
	}

	// Only run in the UI thread
	public void reset(final RedditPreparedPost data) {

		usageId++;

		super.reset();
		if(post != null) post.unbind(this);
		data.bind(this);

		swipeReady = false;
		leftOverlayShown = false;
		rightOverlayShown = false;
		leftOverlayText.setVisibility(GONE);
		rightOverlayText.setVisibility(GONE);

		this.post = data;

		final Bitmap thumbnail = data.getThumbnail(this, usageId);
		thumbnailView.setImageBitmap(thumbnail);

		title.setText(data.title);
		commentsText.setText(String.valueOf(post.commentCount));

		if(data.hasThumbnail) {
			thumbnailView.setVisibility(VISIBLE);
			thumbnailView.setMinimumWidth((int)(64.0f * dpScale)); // TODO remove constant, customise
			thumbnailView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		} else {
			thumbnailView.setMinimumWidth(0);
			thumbnailView.setVisibility(GONE);
		}

		updateAppearance();
	}

	public void updateAppearance() {

		if(post.isRead()) {
			title.setTextColor(rrPostTitleReadCol);
		} else {
			title.setTextColor(rrPostTitleCol);
		}

		subtitle.setText(post.postListDescription);

		if(post.isSaved()) {
			savedIcon.setVisibility(ImageView.VISIBLE);
			hiddenIcon.setVisibility(ImageView.GONE);
		} else {
			savedIcon.setVisibility(ImageView.GONE);
			hiddenIcon.setVisibility(post.isHidden() ? ImageView.VISIBLE : ImageView.GONE);
		}
	}

	public void rrOnClick(final int x, final int y) {

		final int[] buttonLoc = new int[2];
		commentsButton.getLocationOnScreen(buttonLoc);

		if(x >= buttonLoc[0]
				&& x <= buttonLoc[0] + commentsButton.getWidth()
				&& y >= buttonLoc[1]
				&& y <= buttonLoc[1] + commentsButton.getHeight()) {

			fragmentParent.onPostCommentsSelected(post);

		} else {
			fragmentParent.onPostSelected(post);
		}
	}

	private static class RPVMenuItem {
		public final String title;
		public final Action action;

		private RPVMenuItem(Context context, int titleRes, Action action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public void rrOnLongClick() {
		showActionMenu(getContext(), fragmentParent, post);
	}

	private static void showActionMenu(final Context context, final Fragment fragmentParent, final RedditPreparedPost post) {

		final ArrayList<RPVMenuItem> menu = new ArrayList<RPVMenuItem>();

		if(!RedditAccountManager.getInstance(context).getDefaultAccount().isAnonymous()) {

			if(!post.isUpvoted()) {
				menu.add(new RPVMenuItem(context, R.string.action_upvote, Action.UPVOTE));
			} else {
				menu.add(new RPVMenuItem(context, R.string.action_upvote_remove, Action.UNVOTE));
			}

			if(!post.isDownvoted()) {
				menu.add(new RPVMenuItem(context, R.string.action_downvote, Action.DOWNVOTE));
			} else {
				menu.add(new RPVMenuItem(context, R.string.action_downvote_remove, Action.UNVOTE));
			}

			if(!post.isSaved()) {
				menu.add(new RPVMenuItem(context, R.string.action_save, Action.SAVE));
			} else {
				menu.add(new RPVMenuItem(context, R.string.action_unsave, Action.UNSAVE));
			}

			if(!post.isHidden()) {
				menu.add(new RPVMenuItem(context, R.string.action_hide, Action.HIDE));
			} else {
				menu.add(new RPVMenuItem(context, R.string.action_unhide, Action.UNHIDE));
			}

			menu.add(new RPVMenuItem(context, R.string.action_report, Action.REPORT));
		}

		menu.add(new RPVMenuItem(context, R.string.action_external, Action.EXTERNAL));
		if(post.imageUrl != null) menu.add(new RPVMenuItem(context, R.string.action_save_image, Action.SAVE_IMAGE));
		menu.add(new RPVMenuItem(context, R.string.action_gotosubreddit, Action.GOTO_SUBREDDIT));
		menu.add(new RPVMenuItem(context, R.string.action_share, Action.SHARE));
		menu.add(new RPVMenuItem(context, R.string.action_share_comments, Action.SHARE_COMMENTS));
		menu.add(new RPVMenuItem(context, R.string.action_copy, Action.COPY));
		menu.add(new RPVMenuItem(context, R.string.action_user_profile, Action.USER_PROFILE));
		menu.add(new RPVMenuItem(context, R.string.action_properties, Action.PROPERTIES));

		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(context);

		builder.setItems(menuText, new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int which) {
				onActionSelected(post, fragmentParent, menu.get(which).action);
			}
		});

		builder.setNeutralButton(R.string.dialog_cancel, null);

		final AlertDialog alert = builder.create();
		alert.setTitle(R.string.action_menu_post_title);
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	public static void onActionSelected(final RedditPreparedPost post, final Fragment fragmentParent, final Action action) {

		final Activity activity = fragmentParent.getSupportActivity();

		switch(action) {

			case UPVOTE:
				post.action(activity, RedditAPI.RedditAction.UPVOTE);
				break;

			case DOWNVOTE:
				post.action(activity, RedditAPI.RedditAction.DOWNVOTE);
				break;

			case UNVOTE:
				post.action(activity, RedditAPI.RedditAction.UNVOTE);
				break;

			case SAVE:
				post.action(activity, RedditAPI.RedditAction.SAVE);
				break;

			case UNSAVE:
				post.action(activity, RedditAPI.RedditAction.UNSAVE);
				break;

			case HIDE:
				post.action(activity, RedditAPI.RedditAction.HIDE);
				break;

			case UNHIDE:
				post.action(activity, RedditAPI.RedditAction.UNHIDE);
				break;

			case REPORT:

				new AlertDialog.Builder(activity)
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(R.string.action_report,
								new DialogInterface.OnClickListener() {
									public void onClick(final DialogInterface dialog, final int which) {
										post.action(activity, RedditAPI.RedditAction.REPORT);
										// TODO update the view to show the result
										// TODO don't forget, this also hides
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case EXTERNAL: {
				final Intent intent = new Intent(Intent.ACTION_VIEW);
				intent.setData(Uri.parse(post.url));
				activity.startActivity(intent);
				break;
			}

			case SAVE_IMAGE: {

				final RedditAccount anon = RedditAccountManager.getAnon();

				CacheManager.getInstance(activity).makeRequest(new CacheRequest(General.uriFromString(post.imageUrl), anon, null,
						Constants.Priority.IMAGE_VIEW, 0, CacheRequest.DownloadType.IF_NECESSARY,
						Constants.FileType.IMAGE, false, false, false, activity) {

					@Override
					protected void onCallbackException(Throwable t) {
						BugReportActivity.handleGlobalError(context, t);
					}

					@Override
					protected void onDownloadNecessary() {
						General.quickToast(context, R.string.download_downloading);
					}

					@Override
					protected void onDownloadStarted() {}

					@Override
					protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
						final RRError error = General.getGeneralErrorForFailure(context, type, t, status);
						General.showResultDialog(activity, error);
					}

					@Override
					protected void onProgress(long bytesRead, long totalBytes) {}

					@Override
					protected void onSuccess(CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session, boolean fromCache, String mimetype) {

						File dst = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), General.uriFromString(post.imageUrl).getPath());

						if(dst.exists()) {
							int count = 0;

							while(dst.exists()) {
								count++;
								dst = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), count + "_" + General.uriFromString(post.imageUrl).getPath().substring(1));
							}
						}

						try {
							General.copyFile(cacheFile.getInputStream(), dst);
						} catch(IOException e) {
							notifyFailure(RequestFailureType.STORAGE, e, null, "Could not copy file");
							return;
						}

						General.quickToast(context, context.getString(R.string.action_save_image_success) + " " + dst.getName());
					}
				});

				break;
			}

			case SHARE: {

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				mailer.putExtra(Intent.EXTRA_SUBJECT, post.title);
				mailer.putExtra(Intent.EXTRA_TEXT, post.url);
				activity.startActivity(Intent.createChooser(mailer, "Share Post")); // TODO string
				break;
			}

			case SHARE_COMMENTS: {

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				mailer.putExtra(Intent.EXTRA_SUBJECT, "Comments for " + post.title);
				mailer.putExtra(Intent.EXTRA_TEXT, Constants.Reddit.getUri(Constants.Reddit.PATH_COMMENTS + post.idAlone).toString());
				activity.startActivity(Intent.createChooser(mailer, "Share Comments")); // TODO string
				break;
			}

			case COPY: {

				ClipboardManager manager = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
				manager.setText(post.url);
				break;
			}

			case GOTO_SUBREDDIT: {

				final RedditSubreddit subreddit = new RedditSubreddit("/r/" + post.src.subreddit, "/r/" + post.src.subreddit, true);

				final Intent intent = new Intent(activity, PostListingActivity.class);
				intent.putExtra("subreddit", subreddit);
				activity.startActivityForResult(intent, 1);
				break;
			}

			case USER_PROFILE:
				UserProfileDialog.newInstance(post.src.author).show(fragmentParent.getSupportActivity());
				break;

			case PROPERTIES:
				PostPropertiesDialog.newInstance(post.src).show(fragmentParent.getSupportActivity());
				break;

			case COMMENTS:
				((PostListingFragment)fragmentParent).onPostCommentsSelected(post);
				break;

			case LINK:
				((PostListingFragment)fragmentParent).onPostSelected(post);
				break;

			case ACTION_MENU:
				showActionMenu(activity, fragmentParent, post);
				break;
		}
	}

	public void betterThumbnailAvailable(final Bitmap thumbnail, final int callbackUsageId) {
		final Message msg = new Message();
		msg.obj = thumbnail;
		msg.what = callbackUsageId;
		thumbnailHandler.sendMessage(msg);
	}

	public static interface PostSelectionListener {
		public void onPostSelected(RedditPreparedPost post);
		public void onPostCommentsSelected(RedditPreparedPost post);
	}
}
