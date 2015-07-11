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

package org.quantumbadger.redreader.reddit.prepared;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.os.Environment;
import android.preference.PreferenceManager;
import android.text.ClipboardManager;
import android.text.SpannableStringBuilder;
import android.util.Log;
import android.view.View;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.*;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.fragments.PostPropertiesDialog;
import org.quantumbadger.redreader.image.ThumbnailScaler;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParagraphGroup;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParser;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.FlatImageButton;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.bezelmenu.VerticalToolbar;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.*;

public final class RedditPreparedPost {

	public final RedditPost src;

	public final String title;
	public SpannableStringBuilder postListDescription;
	public final String url;

	public final String idAlone, idAndType;

	private int voteDirection;
	private boolean saved, hidden, read, stickied;

	public final boolean hasThumbnail;
	private boolean gotHighResThumb = false;

	// TODO make it possible to turn off in-memory caching when out of memory
	private volatile Bitmap thumbnailCache = null;
	public final String imageUrl, thumbnailUrl;

	private static final Object singleImageDecodeLock = new Object();

	private ThumbnailLoadedCallback thumbnailCallback;
	private int usageId = -1;

	public long lastChange = Long.MIN_VALUE;
	public final int commentCount;

	private final boolean showSubreddit;

	private RedditPostView boundView = null;

	public final MarkdownParagraphGroup parsedSelfText;

	public static enum Action {
		UPVOTE, UNVOTE, DOWNVOTE, SAVE, HIDE, UNSAVE, UNHIDE, REPORT, SHARE, REPLY, USER_PROFILE, EXTERNAL, PROPERTIES, COMMENTS, LINK, COMMENTS_SWITCH, LINK_SWITCH, SHARE_COMMENTS, GOTO_SUBREDDIT, ACTION_MENU, SAVE_IMAGE, COPY, SELFTEXT_LINKS
	}

	// TODO too many parameters
	public RedditPreparedPost(final Context context, final CacheManager cm, final int listId, final RedditPost post,
							  final long timestamp, final boolean showSubreddit, final boolean updateNeeded,
							  final boolean showThumbnails, final boolean precacheImages, final RedditAccount user,
							  final boolean parseSelfText) {

		this.src = post;
		this.showSubreddit = showSubreddit;

		if(post.title == null) {
			title = "[null]";
		} else {
			title = StringEscapeUtils.unescapeHtml4(post.title.replace('\n', ' ')).trim();
		}

		idAlone = post.id;
		idAndType = post.name;
		url = StringEscapeUtils.unescapeHtml4(post.url);
		commentCount = post.num_comments;

		if(post.likes == null) {
			voteDirection = 0;
		} else {
			voteDirection = Boolean.TRUE.equals(post.likes) ? 1 : -1;
		}

		this.saved = post.saved;
		this.hidden = post.hidden;
		this.stickied = post.stickied;

		imageUrl = LinkHandler.getImageUrl(url);
		thumbnailUrl = post.thumbnail;
		hasThumbnail = showThumbnails && (hasThumbnail(post) || imageUrl != null);

		// TODO parameterise
		final int thumbnailWidth = General.dpToPixels(context, 64);

		if(hasThumbnail && hasThumbnail(post)) {
			downloadThumbnail(context, thumbnailWidth, cm, listId, false);
		}

		if(imageUrl != null && precacheImages) {
			downloadThumbnail(context, thumbnailWidth, cm, listId, true);
		}

		// TODO precache comments (respect settings)

		lastChange = timestamp;
		if(voteDirection != 0 || saved || hidden) {
			RedditChangeDataManager.getInstance(context).update("posts", user, this, true);
		} else if(updateNeeded) {
			RedditChangeDataManager.getInstance(context).update("posts", user, this, false);
		}

		rebuildSubtitle(context);

		if(parseSelfText && src.is_self && src.selftext != null && src.selftext.trim().length() > 0) {
			parsedSelfText = MarkdownParser.parse(StringEscapeUtils.unescapeHtml4(post.selftext).toCharArray());
		} else {
			parsedSelfText = null;
		}
	}

	public static void showActionMenu(final Activity activity, final RedditPreparedPost post) {

		final EnumSet<Action> itemPref = PrefsUtility.pref_menus_post_context_items(activity, PreferenceManager.getDefaultSharedPreferences(activity));

		if(itemPref.isEmpty()) return;

		final ArrayList<RPVMenuItem> menu = new ArrayList<RPVMenuItem>();

		if(!RedditAccountManager.getInstance(activity).getDefaultAccount().isAnonymous()) {

			if(itemPref.contains(Action.UPVOTE)) {
				if(!post.isUpvoted()) {
					menu.add(new RPVMenuItem(activity, R.string.action_upvote, Action.UPVOTE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_upvote_remove, Action.UNVOTE));
				}
			}

			if(itemPref.contains(Action.DOWNVOTE)) {
				if(!post.isDownvoted()) {
					menu.add(new RPVMenuItem(activity, R.string.action_downvote, Action.DOWNVOTE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_downvote_remove, Action.UNVOTE));
				}
			}

			if(itemPref.contains(Action.SAVE)) {
				if(!post.isSaved()) {
					menu.add(new RPVMenuItem(activity, R.string.action_save, Action.SAVE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_unsave, Action.UNSAVE));
				}
			}

			if(itemPref.contains(Action.HIDE)) {
				if(!post.isHidden()) {
					menu.add(new RPVMenuItem(activity, R.string.action_hide, Action.HIDE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_unhide, Action.UNHIDE));
				}
			}

			if(itemPref.contains(Action.REPORT)) menu.add(new RPVMenuItem(activity, R.string.action_report, Action.REPORT));
		}

		if(itemPref.contains(Action.EXTERNAL)) menu.add(new RPVMenuItem(activity, R.string.action_external, Action.EXTERNAL));
		if(itemPref.contains(Action.SELFTEXT_LINKS) && post.src.selftext != null && post.src.selftext.length() > 1) menu.add(new RPVMenuItem(activity, R.string.action_selftext_links, Action.SELFTEXT_LINKS));
		if(itemPref.contains(Action.SAVE_IMAGE) && post.imageUrl != null) menu.add(new RPVMenuItem(activity, R.string.action_save_image, Action.SAVE_IMAGE));
		if(itemPref.contains(Action.GOTO_SUBREDDIT)) menu.add(new RPVMenuItem(activity, R.string.action_gotosubreddit, Action.GOTO_SUBREDDIT));
		if(itemPref.contains(Action.SHARE)) menu.add(new RPVMenuItem(activity, R.string.action_share, Action.SHARE));
		if(itemPref.contains(Action.SHARE_COMMENTS)) menu.add(new RPVMenuItem(activity, R.string.action_share_comments, Action.SHARE_COMMENTS));
		if(itemPref.contains(Action.COPY)) menu.add(new RPVMenuItem(activity, R.string.action_copy, Action.COPY));
		if(itemPref.contains(Action.USER_PROFILE)) menu.add(new RPVMenuItem(activity, R.string.action_user_profile, Action.USER_PROFILE));
		if(itemPref.contains(Action.PROPERTIES)) menu.add(new RPVMenuItem(activity, R.string.action_properties, Action.PROPERTIES));

		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		builder.setItems(menuText, new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int which) {
				onActionMenuItemSelected(post, activity, menu.get(which).action);
			}
		});

		//builder.setNeutralButton(R.string.dialog_cancel, null);

		final AlertDialog alert = builder.create();
		alert.setTitle(R.string.action_menu_post_title);
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	public static void onActionMenuItemSelected(final RedditPreparedPost post, final Activity activity, final Action action) {

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
                String url = (activity instanceof WebViewActivity) ? ((WebViewActivity) activity).getCurrentUrl() : post.url;
				intent.setData(Uri.parse(url));
				activity.startActivity(intent);
				break;
			}

			case SELFTEXT_LINKS: {

				final HashSet<String> linksInComment = LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(post.src.selftext));

				if(linksInComment.isEmpty()) {
					General.quickToast(activity, R.string.error_toast_no_urls_in_self);

				} else {

					final String[] linksArr = linksInComment.toArray(new String[linksInComment.size()]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
					builder.setItems(linksArr, new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface dialog, int which) {
							LinkHandler.onLinkClicked(activity, linksArr[which], false, post.src);
							dialog.dismiss();
						}
					});

					final AlertDialog alert = builder.create();
					alert.setTitle(R.string.action_selftext_links);
					alert.setCanceledOnTouchOutside(true);
					alert.show();
				}

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
						final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
						General.showResultDialog(activity, error);
					}

					@Override
					protected void onProgress(boolean authorizationInProgress, long bytesRead, long totalBytes) {}

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
							final InputStream cacheFileInputStream = cacheFile.getInputStream();

							if(cacheFileInputStream == null) {
								notifyFailure(RequestFailureType.CACHE_MISS, null, null, "Could not find cached image");
								return;
							}

							General.copyFile(cacheFileInputStream, dst);

						} catch(IOException e) {
							notifyFailure(RequestFailureType.STORAGE, e, null, "Could not copy file");
							return;
						}

						activity.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
								Uri.parse("file://" + dst.getAbsolutePath()))
						);

						General.quickToast(context, context.getString(R.string.action_save_image_success) + " " + dst.getAbsolutePath());
					}
				});

				break;
			}

			case SHARE: {

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				mailer.putExtra(Intent.EXTRA_SUBJECT, post.title);
				mailer.putExtra(Intent.EXTRA_TEXT, post.url);
				activity.startActivity(Intent.createChooser(mailer, activity.getString(R.string.action_share)));
				break;
			}

			case SHARE_COMMENTS: {

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				mailer.putExtra(Intent.EXTRA_SUBJECT, "Comments for " + post.title);
				mailer.putExtra(Intent.EXTRA_TEXT, Constants.Reddit.getUri(Constants.Reddit.PATH_COMMENTS + post.idAlone).toString());
				activity.startActivity(Intent.createChooser(mailer, activity.getString(R.string.action_share_comments)));
				break;
			}

			case COPY: {

				ClipboardManager manager = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
				manager.setText(post.url);
				break;
			}

			case GOTO_SUBREDDIT: {

				try {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(SubredditPostListURL.getSubreddit(post.src.subreddit).generateJsonUri());
					activity.startActivityForResult(intent, 1);

				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					Toast.makeText(activity, R.string.invalid_subreddit_name, Toast.LENGTH_LONG).show();
				}

				break;
			}

			case USER_PROFILE:
				LinkHandler.onLinkClicked(activity, new UserProfileURL(post.src.author).toString());
				break;

			case PROPERTIES:
				PostPropertiesDialog.newInstance(post.src).show(activity.getFragmentManager(), null);
				break;

			case COMMENTS:
				((RedditPostView.PostSelectionListener)activity).onPostCommentsSelected(post);
				break;

			case LINK:
				((RedditPostView.PostSelectionListener)activity).onPostSelected(post);
				break;

			case COMMENTS_SWITCH:
				if(!(activity instanceof MainActivity)) activity.finish();
				((RedditPostView.PostSelectionListener)activity).onPostCommentsSelected(post);
				break;

			case LINK_SWITCH:
				if(!(activity instanceof MainActivity)) activity.finish();
				((RedditPostView.PostSelectionListener)activity).onPostSelected(post);
				break;

			case ACTION_MENU:
				showActionMenu(activity, post);
				break;

			case REPLY:
				final Intent intent = new Intent(activity, CommentReplyActivity.class);
				intent.putExtra("parentIdAndType", post.idAndType);
				activity.startActivity(intent);
				break;
		}
	}

	private void rebuildSubtitle(Context context) {

		// TODO customise display
		// TODO preference for the X days, X hours thing

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
				R.attr.rrPostSubtitleBoldCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol,
				R.attr.rrFlairBackCol,
				R.attr.rrFlairTextCol
		});

		final int boldCol = appearance.getColor(0, 255),
				rrPostSubtitleUpvoteCol = appearance.getColor(1, 255),
				rrPostSubtitleDownvoteCol = appearance.getColor(2, 255),
				rrFlairBackCol = appearance.getColor(3, 255),
				rrFlairTextCol = appearance.getColor(4, 255);

		final BetterSSB postListDescSb = new BetterSSB();

		final int pointsCol;
		int score = src.score;

		if(Boolean.TRUE.equals(src.likes)) score--;
		if(Boolean.FALSE.equals(src.likes)) score++;

		if(isUpvoted()) {
			pointsCol = rrPostSubtitleUpvoteCol;
			score++;
		} else if(isDownvoted()) {
			pointsCol = rrPostSubtitleDownvoteCol;
			score--;
		} else {
			pointsCol = boldCol;
		}

		if(src.over_18) {
			postListDescSb.append(" NSFW ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.RED, 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		if(src.link_flair_text != null && src.link_flair_text.length() > 0) {
			postListDescSb.append(" " + StringEscapeUtils.unescapeHtml4(src.link_flair_text) + " ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					rrFlairTextCol, rrFlairBackCol, 1f);
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(String.valueOf(score), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, pointsCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_points) + " ", 0);
		postListDescSb.append(RRTime.formatDurationFrom(context, src.created_utc * 1000), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_by) + " ", 0);
		postListDescSb.append(src.author, BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);

		if(showSubreddit) {
			postListDescSb.append(" " + context.getString(R.string.subtitle_to) + " ", 0);
			postListDescSb.append(src.subreddit, BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		}

		postListDescSb.append(" (" + src.domain + ")", 0);

		postListDescription = postListDescSb.get();
	}

	// lol, reddit api
	private static boolean hasThumbnail(final RedditPost post) {
		return post.thumbnail != null
				&& post.thumbnail.length() != 0
				&& !post.thumbnail.equalsIgnoreCase("nsfw")
				&& !post.thumbnail.equalsIgnoreCase("self")
				&& !post.thumbnail.equalsIgnoreCase("default");
	}

	private void downloadThumbnail(final Context context, final int widthPixels, final CacheManager cm, final int listId, final boolean highRes) {

		final String uriStr = highRes ? imageUrl : thumbnailUrl;
		final URI uri = General.uriFromString(uriStr);

		final int priority = highRes ? Constants.Priority.IMAGE_PRECACHE : Constants.Priority.THUMBNAIL;
		final int fileType = highRes ? Constants.FileType.IMAGE : Constants.FileType.THUMBNAIL;

		final RedditAccount anon = RedditAccountManager.getAnon();

		cm.makeRequest(new CacheRequest(uri, anon, null, priority, listId, CacheRequest.DownloadType.IF_NECESSARY, fileType, false, false, false, context) {

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onCallbackException(final Throwable t) {
				// TODO handle -- internal error
				throw new RuntimeException(t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {

				if(gotHighResThumb && !highRes) return;
				try {

					synchronized(singleImageDecodeLock) {

						BitmapFactory.Options justDecodeBounds = new BitmapFactory.Options();
						justDecodeBounds.inJustDecodeBounds = true;
						BitmapFactory.decodeStream(cacheFile.getInputStream(), null, justDecodeBounds);
						final int width = justDecodeBounds.outWidth;
						final int height = justDecodeBounds.outHeight;

						int factor = 1;

						while(width / (factor + 1) > widthPixels
								&& height / (factor + 1) > widthPixels) factor *= 2;

						BitmapFactory.Options scaledOptions = new BitmapFactory.Options();
						scaledOptions.inSampleSize = factor;

						final Bitmap data = BitmapFactory.decodeStream(cacheFile.getInputStream(), null, scaledOptions);

						if(data == null) return;
						thumbnailCache = ThumbnailScaler.scale(data, widthPixels);
						if(thumbnailCache != data) data.recycle();
					}

					if(highRes) gotHighResThumb = true;

					if(thumbnailCallback != null) thumbnailCallback.betterThumbnailAvailable(thumbnailCache, usageId);

				} catch (OutOfMemoryError e) {
					// TODO handle this better - disable caching of images
					Log.e("RedditPreparedPost", "Out of memory trying to download image");
					e.printStackTrace();
				} catch(Throwable t) {
					// Just ignore it.
				}
			}
		});
	}

	// These operations are ordered so as to avoid race conditions
	public Bitmap getThumbnail(final ThumbnailLoadedCallback callback, final int usageId) {
		this.thumbnailCallback = callback;
		this.usageId = usageId;
		return thumbnailCache;
	}

	public boolean isSelf() {
		return src.is_self;
	}

	public void setRead(boolean read) {
		this.read = read;
	}

	public boolean isRead() {
		return read;
	}

	public boolean isSticky() { return stickied; }

	public void bind(RedditPostView boundView) {
		this.boundView = boundView;
	}

	public void unbind(RedditPostView boundView) {
		if(this.boundView == boundView) this.boundView = null;
	}

	// TODO handle download failure - show red "X" or something
	public static interface ThumbnailLoadedCallback {
		public void betterThumbnailAvailable(Bitmap thumbnail, int usageId);
	}

	public void markAsRead(final Context context) {
		setRead(true);
		refreshView(context);
		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		RedditChangeDataManager.getInstance(context).update("posts", user, RedditPreparedPost.this, true);
	}

	public void refreshView(final Context context) {
		General.UI_THREAD_HANDLER.post(new Runnable() {
			public void run() {
				rebuildSubtitle(context);
				if(boundView != null) {
					boundView.updateAppearance();
					boundView.requestLayout();
					boundView.invalidate();
				}
			}
		});
	}

	public void action(final Activity activity, final RedditAPI.RedditAction action) {

		if(RedditAccountManager.getInstance(activity).getDefaultAccount().isAnonymous()) {

			General.UI_THREAD_HANDLER.post(new Runnable() {
				public void run() {
					Toast.makeText(activity, "You must be logged in to do that.", Toast.LENGTH_SHORT).show();
				}
			});

			return;
		}

		final int lastVoteDirection = voteDirection;

		switch(action) {
			case DOWNVOTE:
				if(!src.archived) {
					voteDirection = -1;
				}
				break;
			case UNVOTE:
				if(!src.archived) {
					voteDirection = 0;
				}
				break;
			case UPVOTE:
				if(!src.archived) {
					voteDirection = 1;
				}
				break;

			case SAVE: saved = true; break;
			case UNSAVE: saved = false; break;

			case HIDE: hidden = true; break;
			case UNHIDE: hidden = false; break;

			case REPORT: hidden = true; break;

			default:
				throw new RuntimeException("Unknown post action");
		}

		refreshView(activity);

		boolean vote = (action == RedditAPI.RedditAction.DOWNVOTE
				| action == RedditAPI.RedditAction.UPVOTE
				| action == RedditAPI.RedditAction.UNVOTE);

		if(src.archived && vote){
			Toast.makeText(activity, R.string.error_archived_vote, Toast.LENGTH_SHORT)
					.show();
			return;
		}

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		RedditAPI.action(CacheManager.getInstance(activity),
				new APIResponseHandler.ActionResponseHandler(activity) {
					@Override
					protected void onCallbackException(final Throwable t) {
						BugReportActivity.handleGlobalError(context, t);
					}

					@Override
					protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
						revertOnFailure();
						if(t != null) t.printStackTrace();

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status,
								"Reddit API action: " + action.toString() + " " + url);
						General.UI_THREAD_HANDLER.post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						General.UI_THREAD_HANDLER.post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onSuccess() {
						lastChange = RRTime.utcCurrentTimeMillis();
						RedditChangeDataManager.getInstance(context).update("posts", user, RedditPreparedPost.this, true);
					}

					private void revertOnFailure() {

						switch(action) {
							case DOWNVOTE:
							case UNVOTE:
							case UPVOTE:
								voteDirection = lastVoteDirection; break;

							case SAVE: saved = false; break;
							case UNSAVE: saved = true; break;

							case HIDE: hidden = false; break;
							case UNHIDE: hidden = true; break;

							case REPORT: hidden = false; break;

							default:
								throw new RuntimeException("Unknown post action");
						}

						refreshView(context);
					}

				}, user, idAndType, action, activity);
	}

	public boolean isUpvoted() {
		return voteDirection == 1;
	}

	public boolean isDownvoted() {
		return voteDirection == -1;
	}

	public boolean isSaved() {
		return saved;
	}

	public boolean isHidden() {
		return hidden;
	}

	public int getVoteDirection() {
		return voteDirection;
	}

	public void updateFromChangeDb(final long dbTimestamp, final int voteDirection, final boolean saved, final boolean hidden, final boolean read) {
		this.lastChange = dbTimestamp;
		this.voteDirection = voteDirection;
		this.saved = saved;
		this.hidden = hidden;
		this.read = read;
	}

	private static class RPVMenuItem {
		public final String title;
		public final Action action;

		private RPVMenuItem(Context context, int titleRes, Action action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public VerticalToolbar generateToolbar(final Activity activity, boolean isComments, final SideToolbarOverlay overlay) {

		final VerticalToolbar toolbar = new VerticalToolbar(activity);
		final EnumSet<Action> itemsPref = PrefsUtility.pref_menus_post_toolbar_items(activity, PreferenceManager.getDefaultSharedPreferences(activity));

		final Action[] possibleItems = {
				Action.ACTION_MENU,
				isComments ? Action.LINK_SWITCH : Action.COMMENTS_SWITCH,
				Action.UPVOTE,
				Action.DOWNVOTE,
				Action.SAVE,
				Action.HIDE,
				Action.REPLY,
				Action.EXTERNAL,
				Action.SAVE_IMAGE,
				Action.SHARE,
				Action.COPY,
				Action.USER_PROFILE,
				Action.PROPERTIES
		};

		// TODO make static
		final EnumMap<Action, Integer> iconsDark = new EnumMap<Action, Integer>(Action.class);
		iconsDark.put(Action.ACTION_MENU, R.drawable.ic_action_overflow);
		iconsDark.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_dark);
		iconsDark.put(Action.LINK_SWITCH, imageUrl != null ? R.drawable.ic_action_image_dark : R.drawable.ic_action_page_dark);
		iconsDark.put(Action.UPVOTE, R.drawable.action_upvote_dark);
		iconsDark.put(Action.DOWNVOTE, R.drawable.action_downvote_dark);
		iconsDark.put(Action.SAVE, R.drawable.ic_action_star_filled_dark);
		iconsDark.put(Action.HIDE, R.drawable.ic_action_cross_dark);
		iconsDark.put(Action.REPLY, R.drawable.ic_action_reply_dark);
		iconsDark.put(Action.EXTERNAL, R.drawable.ic_action_globe_dark);
		iconsDark.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_dark);
		iconsDark.put(Action.SHARE, R.drawable.ic_action_share_dark);
		iconsDark.put(Action.COPY, R.drawable.ic_action_copy_dark);
		iconsDark.put(Action.USER_PROFILE, R.drawable.ic_action_person_dark);
		iconsDark.put(Action.PROPERTIES, R.drawable.ic_action_info_dark);

		final EnumMap<Action, Integer> iconsLight = new EnumMap<Action, Integer>(Action.class);
		iconsLight.put(Action.ACTION_MENU, R.drawable.ic_action_overflow);
		iconsLight.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_light);
		iconsLight.put(Action.LINK_SWITCH, imageUrl != null ? R.drawable.ic_action_image_light : R.drawable.ic_action_page_light);
		iconsLight.put(Action.UPVOTE, R.drawable.action_upvote_light);
		iconsLight.put(Action.DOWNVOTE, R.drawable.action_downvote_light);
		iconsLight.put(Action.SAVE, R.drawable.ic_action_star_filled_light);
		iconsLight.put(Action.HIDE, R.drawable.ic_action_cross_light);
		iconsLight.put(Action.REPLY, R.drawable.ic_action_reply_light);
		iconsLight.put(Action.EXTERNAL, R.drawable.ic_action_globe_light);
		iconsLight.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_light);
		iconsLight.put(Action.SHARE, R.drawable.ic_action_share_light);
		iconsLight.put(Action.COPY, R.drawable.ic_action_copy_light);
		iconsLight.put(Action.USER_PROFILE, R.drawable.ic_action_person_light);
		iconsLight.put(Action.PROPERTIES, R.drawable.ic_action_info_light);

		for(final Action action : possibleItems) {

			if(action == Action.SAVE_IMAGE && imageUrl == null) continue;

			if(itemsPref.contains(action)) {

				final FlatImageButton ib = new FlatImageButton(activity);

				final int buttonPadding = General.dpToPixels(activity, 10);
				ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding);

				if(action == Action.UPVOTE && isUpvoted()
						|| action == Action.DOWNVOTE && isDownvoted()
						|| action == Action.SAVE && isSaved()
						|| action == Action.HIDE && isHidden()) {

					ib.setBackgroundColor(Color.WHITE);
					ib.setImageResource(iconsLight.get(action));

				} else {
					ib.setImageResource(iconsDark.get(action));
					// TODO highlight on click
				}

				ib.setOnClickListener(new View.OnClickListener() {
					public void onClick(View v) {

						final Action actionToTake;

						switch(action) {
							case UPVOTE:
								actionToTake = isUpvoted() ? Action.UNVOTE : Action.UPVOTE;
								break;

							case DOWNVOTE:
								actionToTake = isDownvoted() ? Action.UNVOTE : Action.DOWNVOTE;
								break;

							case SAVE:
								actionToTake = isSaved() ? Action.UNSAVE : Action.SAVE;
								break;

							case HIDE:
								actionToTake = isHidden() ? Action.UNHIDE : Action.HIDE;
								break;

							default:
								actionToTake = action;
								break;
						}

						onActionMenuItemSelected(RedditPreparedPost.this, activity, actionToTake);
						overlay.hide();
					}
				});

				toolbar.addItem(ib);
			}
		}

		return toolbar;
	}
}
