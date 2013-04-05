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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.os.Handler;
import android.os.Looper;
import android.text.SpannableStringBuilder;
import android.util.Log;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.image.ThumbnailScaler;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.views.RedditPostView;

import java.net.URI;
import java.util.UUID;

public final class RedditPreparedPost {

	public final RedditPost src;

	public final String title;
	public SpannableStringBuilder postListDescription;
	public final String url;

	public final String idAlone, idAndType;

	private int voteDirection;
	private boolean saved, hidden, read;

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
	private final RedditSubreddit parentSubreddit;

	private RedditPostView boundView = null;

	// TODO too many parameters
	public RedditPreparedPost(final Context context, final CacheManager cm, final int listId, final RedditPost post,
							  final long timestamp, final boolean showSubreddit, final RedditSubreddit parentSubreddit,
							  final boolean updateNeeded, final boolean showThumbnails, final boolean precacheImages,
							  final RedditAccount user) {

		this.src = post;
		this.parentSubreddit = parentSubreddit;
		this.showSubreddit = showSubreddit;

		title = StringEscapeUtils.unescapeHtml4(post.title.replace('\n', ' ')).trim();

		idAlone = post.id;
		idAndType = post.name;
		url = post.url;
		commentCount = post.num_comments;

		if(post.likes == null) {
			voteDirection = 0;
		} else {
			voteDirection = Boolean.TRUE.equals(post.likes) ? 1 : -1;
		}

		imageUrl = LinkHandler.getImageUrl(post.url);
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
			RedditChangeDataManager.getInstance(context).update(parentSubreddit.url, user, this, true);
		} else if(updateNeeded) {
			RedditChangeDataManager.getInstance(context).update(parentSubreddit.url, user, this, false);
		}

		rebuildSubtitle(context);
	}

	private void rebuildSubtitle(Context context) {

		// TODO customise display
		// TODO strings
		// TODO preference for the X days, X hours thing

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
				R.attr.rrPostSubtitleBoldCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol
		});

		final int boldCol = appearance.getColor(0, 255),
				rrPostSubtitleUpvoteCol = appearance.getColor(1, 255),
				rrPostSubtitleDownvoteCol = appearance.getColor(2, 255);

		final BetterSSB postListDescSb = new BetterSSB();

		final int pointsCol;
		if(isUpvoted()) {
			pointsCol = rrPostSubtitleUpvoteCol;
		} else if(isDownvoted()) {
			pointsCol = rrPostSubtitleDownvoteCol;
		} else {
			pointsCol = boldCol;
		}

		if(src.over_18) {
			postListDescSb.append(" NSFW ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.RED, 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(String.valueOf(src.score), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, pointsCol, 0, 1f);
		postListDescSb.append(" pts ", 0);
		postListDescSb.append(RRTime.formatDurationMs(RRTime.utcCurrentTimeMillis() - src.created_utc * 1000), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" ago by ", 0);
		postListDescSb.append(src.author, BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);

		if(showSubreddit) {
			postListDescSb.append(" to ", 0);
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
			protected void onProgress(final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {

				// The lock avoids using too much memory
				synchronized(singleImageDecodeLock) {

					if(gotHighResThumb && !highRes) return;

					try {
						final Bitmap data = BitmapFactory.decodeStream(cacheFile.getInputStream());
						if(data == null) return;
						thumbnailCache = ThumbnailScaler.scale(data, widthPixels);
						data.recycle();

						if(highRes) gotHighResThumb = true;

						if(thumbnailCallback != null) thumbnailCallback.betterThumbnailAvailable(thumbnailCache, usageId);

					} catch (OutOfMemoryError e) {
						// TODO handle this better - disable caching of images
						Log.e("RedditPreparedPost", "Out of memory trying to download image");
						e.printStackTrace();
						return;

					} catch(Throwable t) {
						// Just ignore it.
					}
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
		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		RedditChangeDataManager.getInstance(context).update(parentSubreddit.url, user, RedditPreparedPost.this, true);
	}

	public void refreshView(final Context context) {
		new Handler(Looper.getMainLooper()).post(new Runnable() {
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

			new Handler(Looper.getMainLooper()).post(new Runnable() {
				public void run() {
					Toast.makeText(activity, "You must be logged in to do that.", Toast.LENGTH_SHORT).show();
				}
			});

			return;
		}

		final int lastVoteDirection = voteDirection;

		switch(action) {
			case DOWNVOTE: voteDirection = -1; break;
			case UNVOTE: voteDirection = 0; break;
			case UPVOTE: voteDirection = 1; break;

			case SAVE: saved = true; break;
			case UNSAVE: saved = false; break;

			case HIDE: hidden = true; break;
			case UNHIDE: hidden = false; break;

			case REPORT: hidden = true; break;

			default:
				throw new RuntimeException("Unknown post action");
		}

		refreshView(activity);

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

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status);
						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onSuccess() {
						lastChange = RRTime.utcCurrentTimeMillis();
						RedditChangeDataManager.getInstance(context).update(parentSubreddit.url, user, RedditPreparedPost.this, true);
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
}
