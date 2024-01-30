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
import android.text.SpannableStringBuilder;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.ScreenreaderPronunciation;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimeFormatHelper;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.image.ThumbnailScaler;
import org.quantumbadger.redreader.reddit.api.RedditPostActions;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.views.RedditPostView;

import java.io.IOException;
import java.net.URI;
import java.util.EnumSet;
import java.util.Locale;
import java.util.UUID;

public final class RedditPreparedPost implements RedditChangeDataManager.Listener {

	private static final String TAG = "RedditPreparedPost";

	public final RedditParsedPost src;
	private final RedditChangeDataManager mChangeDataManager;

	public final boolean isArchived;
	public final boolean isLocked;
	public final boolean canModerate;
	public final boolean hasThumbnail;
	public final boolean mIsProbablyAnImage;

	private final boolean mShowInlinePreviews;

	// TODO make it possible to turn off in-memory caching when out of memory
	private volatile Bitmap thumbnailCache = null;

	private ThumbnailLoadedCallback thumbnailCallback;
	private int usageId = -1;

	public TimestampUTC lastChange;

	public final boolean showSubreddit;

	private RedditPostView mBoundView = null;

	// TODO too many parameters
	public RedditPreparedPost(
			final Context context,
			final CacheManager cm,
			final int listId,
			final RedditParsedPost post,
			final TimestampUTC timestamp,
			final boolean showSubreddit,
			final boolean showThumbnails,
			final boolean allowHighResThumbnails,
			final boolean showInlinePreviews) {

		this.src = post;
		this.showSubreddit = showSubreddit;
		mShowInlinePreviews = showInlinePreviews;

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		mChangeDataManager = RedditChangeDataManager.getInstance(user);

		isArchived = post.isArchived();
		isLocked = post.isLocked();
		canModerate = post.getCanModerate();

		mIsProbablyAnImage = LinkHandler.isProbablyAnImage(post.getUrl());

		hasThumbnail = showThumbnails && hasThumbnail(post);

		final int thumbnailWidth = General.dpToPixels(
				context,
				PrefsUtility.images_thumbnail_size_dp());

		if(hasThumbnail && hasThumbnail(post) && !shouldShowInlinePreview()) {
			downloadThumbnail(context, allowHighResThumbnails, thumbnailWidth, cm, listId);
		}

		lastChange = timestamp;
		mChangeDataManager.update(timestamp, post.getSrc());
	}

	public boolean shouldShowInlinePreview() {
		return mShowInlinePreviews && (src.isPreviewEnabled()
				|| "gfycat.com".equals(src.getDomain())
				|| "i.imgur.com".equals(src.getDomain())
				|| "streamable.com".equals(src.getDomain())
				|| "i.redd.it".equals(src.getDomain())
				|| "v.redd.it".equals(src.getDomain()));
	}

	public boolean isVideoPreview() {
		return src.isVideoPreview();
	}

	public void performAction(final BaseActivity activity, final RedditPostActions.Action action) {
		RedditPostActions.INSTANCE.onActionMenuItemSelected(this, activity, action);
	}

	public int computeScore() {

		int score = src.getScoreExcludingOwnVote();

		if(isUpvoted()) {
			score++;
		} else if(isDownvoted()) {
			score--;
		}

		return score;
	}

	public SpannableStringBuilder buildSubtitle(
			final Context context,
			final boolean headerMode) {

		final EnumSet<PrefsUtility.AppearancePostSubtitleItem> mPostSubtitleItems;
		final int mPostAgeUnits;
		if(headerMode
				&& PrefsUtility.appearance_post_subtitle_items_use_different_settings()) {
			mPostSubtitleItems = PrefsUtility.appearance_post_header_subtitle_items();
			mPostAgeUnits = PrefsUtility.appearance_post_header_age_units();
		} else {
			mPostSubtitleItems = PrefsUtility.appearance_post_subtitle_items();
			mPostAgeUnits = PrefsUtility.appearance_post_age_units();
		}

		final TypedArray appearance = context.obtainStyledAttributes(new int[] {
				R.attr.rrPostSubtitleBoldCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol,
				R.attr.rrFlairBackCol,
				R.attr.rrFlairTextCol,
				R.attr.rrGoldTextCol,
				R.attr.rrGoldBackCol,
				R.attr.rrCrosspostTextCol,
				R.attr.rrCrosspostBackCol
		});

		final int boldCol;
		if(headerMode) {
			boldCol = Color.WHITE;
		} else {
			boldCol = appearance.getColor(0, 255);
		}

		final int rrPostSubtitleUpvoteCol = appearance.getColor(1, 255);
		final int rrPostSubtitleDownvoteCol = appearance.getColor(2, 255);
		final int rrFlairBackCol = appearance.getColor(3, 255);
		final int rrFlairTextCol = appearance.getColor(4, 255);
		final int rrGoldTextCol = appearance.getColor(5, 255);
		final int rrGoldBackCol = appearance.getColor(6, 255);
		final int rrCrosspostTextCol = appearance.getColor(7, 255);
		final int rrCrosspostBackCol = appearance.getColor(8, 255);

		appearance.recycle();

		final BetterSSB postListDescSb = new BetterSSB();

		final int pointsCol;

		final int score = computeScore();

		if(isUpvoted()) {
			pointsCol = rrPostSubtitleUpvoteCol;
		} else if(isDownvoted()) {
			pointsCol = rrPostSubtitleDownvoteCol;
		} else {
			pointsCol = boldCol;
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SPOILER)) {
			if(src.isSpoiler()) {
				postListDescSb.append(
						" SPOILER ",
						BetterSSB.BOLD
								| BetterSSB.FOREGROUND_COLOR
								| BetterSSB.BACKGROUND_COLOR,
						Color.WHITE,
						Color.rgb(50, 50, 50),
						1f);
				postListDescSb.append("  ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.STICKY)) {
			if(src.isStickied()) {
				postListDescSb.append(
						" STICKY ",
						BetterSSB.BOLD
								| BetterSSB.FOREGROUND_COLOR
								| BetterSSB.BACKGROUND_COLOR,
						Color.WHITE,
						Color.rgb(0, 170, 0),
						1f); // TODO color?
				postListDescSb.append("  ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.NSFW)) {
			if(src.isNsfw()) {
				postListDescSb.append(
						" NSFW ",
						BetterSSB.BOLD
								| BetterSSB.FOREGROUND_COLOR
								| BetterSSB.BACKGROUND_COLOR,
						Color.WHITE,
						Color.RED,
						1f); // TODO color?
				postListDescSb.append("  ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.FLAIR)) {
			if(src.getFlairText() != null) {
				postListDescSb.append(
						" "
								+ src.getFlairText()
								+ General.LTR_OVERRIDE_MARK
								+ " ",
						BetterSSB.BOLD
								| BetterSSB.FOREGROUND_COLOR
								| BetterSSB.BACKGROUND_COLOR,
						rrFlairTextCol,
						rrFlairBackCol,
						1f);
				postListDescSb.append("  ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.COMMENTS)) {
			postListDescSb.append(
					String.valueOf(src.getCommentCount()),
					BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
					boldCol,
					0,
					1f);
			postListDescSb.append(
					BetterSSB.NBSP + context.getString(R.string.subtitle_comments) + " ",
					0);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SCORE)) {
			postListDescSb.append(
					String.valueOf(score),
					BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
					pointsCol,
					0,
					1f);
			postListDescSb.append(
					BetterSSB.NBSP + context.getString(R.string.subtitle_points) + " ",
					0);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.UPVOTE_RATIO)) {
			postListDescSb.append("(", 0);
			postListDescSb.append(
					src.getUpvotePercentage() + "%",
					BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
					boldCol,
					0,
					1f);
			postListDescSb.append(
					BetterSSB.NBSP + context.getString(R.string.subtitle_upvote_ratio) + ") ", 0);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.GOLD)) {
			if(src.getGoldAmount() > 0) {
				if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SCORE)
						|| mPostSubtitleItems.contains(
								PrefsUtility.AppearancePostSubtitleItem.UPVOTE_RATIO)) {
					postListDescSb.append(" ", 0);
				}
				postListDescSb.append(
						" "
								+ context.getString(R.string.gold)
								+ BetterSSB.NBSP
								+ "x"
								+ src.getGoldAmount()
								+ " ",
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
						rrGoldTextCol,
						rrGoldBackCol,
						1f);
				postListDescSb.append("  ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.AGE)) {
			postListDescSb.append(
					TimeFormatHelper.format(
							src.getCreatedTimeUTC().elapsedPeriod(),
							context,
							R.string.time_ago,
							mPostAgeUnits),
					BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
					boldCol,
					0,
					1f);
			postListDescSb.append(" ", 0);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.AUTHOR)) {
			postListDescSb.append(context.getString(R.string.subtitle_by) + " ", 0);

			final boolean setBackgroundColour;
			final int backgroundColour; // TODO color from theme

			if("moderator".equals(src.getDistinguished())) {
				setBackgroundColour = true;
				backgroundColour = Color.rgb(0, 170, 0);
			} else if("admin".equals(src.getDistinguished())) {
				setBackgroundColour = true;
				backgroundColour = Color.rgb(170, 0, 0);
			} else {
				setBackgroundColour = false;
				backgroundColour = 0;
			}

			if(setBackgroundColour) {
				postListDescSb.append(
						BetterSSB.NBSP + src.getAuthor() + BetterSSB.NBSP,
						BetterSSB.BOLD
								| BetterSSB.FOREGROUND_COLOR
								| BetterSSB.BACKGROUND_COLOR,
						Color.WHITE,
						backgroundColour,
						1f);
			} else {
				postListDescSb.append(
						src.getAuthor(),
						BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
						boldCol,
						0,
						1f);
			}

			postListDescSb.append(" ", 0);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SUBREDDIT)) {
			if(showSubreddit) {
				postListDescSb.append(context.getString(R.string.subtitle_to) + " ", 0);
				postListDescSb.append(
						src.getSubreddit(),
						BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
						boldCol,
						0,
						1f);
				postListDescSb.append(" ", 0);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.DOMAIN)) {
			postListDescSb.append("(" + src.getDomain() + ")", 0);
		}

		//show crosspost at the end
		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.CROSSPOST)) {
			if(src.isCrosspost() != null) {
				postListDescSb.append(" ", 0);
				postListDescSb.append(
						" "
								+ context.getString(R.string.crosspost)
								+ BetterSSB.NBSP,
						BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
						rrCrosspostTextCol,
						rrCrosspostBackCol,
						1f);
			}
		}

		return postListDescSb.get();
	}

	public String buildAccessibilitySubtitle(
			final Context context,
			final boolean headerMode) {

		final EnumSet<PrefsUtility.AppearancePostSubtitleItem> mPostSubtitleItems;
		final int mPostAgeUnits;
		if(headerMode
				&& PrefsUtility.appearance_post_subtitle_items_use_different_settings()) {
			mPostSubtitleItems = PrefsUtility.appearance_post_header_subtitle_items();
			mPostAgeUnits = PrefsUtility.appearance_post_header_age_units();
		} else {
			mPostSubtitleItems = PrefsUtility.appearance_post_subtitle_items();
			mPostAgeUnits = PrefsUtility.appearance_post_age_units();
		}

		final StringBuilder accessibilitySubtitle = new StringBuilder();

		final int score = computeScore();

		final String separator = " \n";

		final boolean conciseMode = PrefsUtility.pref_accessibility_concise_mode();

		// When not in concise mode, add embellishments to the subtitle for greater clarity and
		// retention of familiar behaviour.
		if (!conciseMode) {
			accessibilitySubtitle.append(buildAccessibilityEmbellishments(context, headerMode));
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.COMMENTS)) {
			accessibilitySubtitle
					.append(context.getResources().getQuantityString(
							R.plurals.accessibility_subtitle_comments_withperiod_plural,
							src.getCommentCount(),
							src.getCommentCount()))
					.append(separator);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SCORE)) {
			accessibilitySubtitle
					.append(context.getResources().getQuantityString(
							conciseMode
									? R.plurals.
											accessibility_subtitle_points_withperiod_concise_plural
									: R.plurals.accessibility_subtitle_points_withperiod_plural,
							score,
							score))
					.append(separator);

			if(isUpvoted()) {
				accessibilitySubtitle
						.append(context.getString(
								R.string.accessibility_subtitle_upvoted_withperiod))
						.append(separator);
			}

			if(isDownvoted()) {
				accessibilitySubtitle
						.append(context.getString(
								R.string.accessibility_subtitle_downvoted_withperiod))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.UPVOTE_RATIO)) {
			accessibilitySubtitle
					.append(context.getString(
							conciseMode
									?R.string.accessibility_subtitle_upvote_ratio_withperiod_concise
									: R.string.accessibility_subtitle_upvote_ratio_withperiod,
							src.getUpvotePercentage()))
					.append(separator);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.GOLD)) {
			if(src.getGoldAmount() > 0) {
				accessibilitySubtitle
						.append(context.getString(
								R.string.accessibility_subtitle_gold_withperiod,
								src.getGoldAmount()))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.AGE)) {
			accessibilitySubtitle
					.append(context.getString(
							R.string.accessibility_subtitle_age_withperiod,
									TimeFormatHelper.format(
											src.getCreatedTimeUTC().elapsedPeriod(),
											context,
											R.string.time_ago,
											mPostAgeUnits)))
					.append(separator);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SUBREDDIT)) {
			if(showSubreddit) {
				accessibilitySubtitle
						.append(context.getString(
								conciseMode
										?(R.string.
												accessibility_subtitle_subreddit_withperiod_concise
										)
										: R.string.accessibility_subtitle_subreddit_withperiod,
								ScreenreaderPronunciation.getPronunciation(
										context,
										src.getSubreddit())))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.DOMAIN)) {

			final String domain = src.getDomain().toLowerCase(Locale.US);

			if(src.isSelfPost()) {
				accessibilitySubtitle
						.append(context.getString(
								conciseMode
										?R.string.accessibility_subtitle_selfpost_withperiod_concise
										: R.string.accessibility_subtitle_selfpost_withperiod))
						.append(separator);

			} else {
				accessibilitySubtitle
						.append(context.getString(
								conciseMode
										?R.string.accessibility_subtitle_domain_withperiod_concise
										: R.string.accessibility_subtitle_domain_withperiod,
								ScreenreaderPronunciation.getPronunciation(context, domain)))
						.append(separator);

				if(src.hasSelfText()) {
					accessibilitySubtitle
							.append(context.getString(
									conciseMode
											?R.string.
											accessibility_subtitle_selfpost_withperiod_concise
											: R.string.
											accessibility_subtitle_has_selftext_withperiod))
							.append(separator);
				}
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.AUTHOR)) {
			@StringRes final int authorString;

			if("moderator".equals(src.getDistinguished())) {
				authorString = conciseMode
					? R.string.accessibility_subtitle_author_moderator_withperiod_concise_post
					: R.string.accessibility_subtitle_author_moderator_withperiod;
			} else if("admin".equals(src.getDistinguished())) {
				authorString = conciseMode
					? R.string.accessibility_subtitle_author_admin_withperiod_concise_post
					: R.string.accessibility_subtitle_author_admin_withperiod;
			} else {
				authorString = conciseMode
					? R.string.accessibility_subtitle_author_withperiod_concise_post
					: R.string.accessibility_subtitle_author_withperiod;
			}

			accessibilitySubtitle
					.append(context.getString(
							authorString,
							ScreenreaderPronunciation.getPronunciation(
									context,
									src.getAuthor())))
					.append(separator);
		}

		return accessibilitySubtitle.toString();
	}

	public String buildAccessibilityTitle(
			final Context context,
			final boolean headerMode) {

		final StringBuilder a11yTitle = new StringBuilder();

		// When in concise mode, add embellishments to the title for greater interruptability when
		// navigating quickly.
		if (PrefsUtility.pref_accessibility_concise_mode()) {
			a11yTitle.append(buildAccessibilityEmbellishments(context, headerMode));
		}

		a11yTitle.append(src.getTitle());

		// Append full stop so that subtitle doesn't become part of title
		a11yTitle.append(".\n");

		return a11yTitle.toString();
	}

	private String buildAccessibilityEmbellishments(
			final Context context,
			final boolean headerMode) {

		final EnumSet<PrefsUtility.AppearancePostSubtitleItem> mPostSubtitleItems;
		if(headerMode
				&& PrefsUtility.appearance_post_subtitle_items_use_different_settings()) {
			mPostSubtitleItems = PrefsUtility.appearance_post_header_subtitle_items();
		} else {
			mPostSubtitleItems = PrefsUtility.appearance_post_subtitle_items();
		}

		final StringBuilder a11yEmbellish = new StringBuilder();

		final String separator = " \n";

		final boolean conciseMode = PrefsUtility.pref_accessibility_concise_mode();

		if (isRead()) {
				a11yEmbellish
						.append(
								ScreenreaderPronunciation.getAccessibilityString(
										context,
										R.string.accessibility_post_already_read_withperiod
								)
						)
						.append(separator);
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SPOILER)) {
			if(src.isSpoiler()) {
				a11yEmbellish
						.append(context.getString(
								R.string.accessibility_subtitle_spoiler_withperiod))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.STICKY)) {
			if(src.isStickied()) {
				a11yEmbellish
						.append(context.getString(
								R.string.accessibility_subtitle_sticky_withperiod))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.NSFW)) {
			if(src.isNsfw()) {
				a11yEmbellish
						.append(context.getString(
								conciseMode
										? R.string.accessibility_subtitle_nsfw_withperiod_concise
										: R.string.accessibility_subtitle_nsfw_withperiod))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.FLAIR)) {
			if(src.getFlairText() != null) {
				a11yEmbellish
						.append(context.getString(
								conciseMode
										? R.string.accessibility_subtitle_flair_withperiod_concise
										: R.string.accessibility_subtitle_flair_withperiod,
								src.getFlairText()
										+ General.LTR_OVERRIDE_MARK))
						.append(separator);
			}
		}

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.CROSSPOST)) {
			if(src.isCrosspost() != null) {
				a11yEmbellish
						.append(context.getString(
								R.string.accessibility_subtitle_crosspost))
						.append(separator);
			}
		}

		return a11yEmbellish.toString();
	}

	// lol, reddit api
	private static boolean hasThumbnail(final RedditParsedPost post) {

		final String url = post.getThumbnailUrl();

		return url != null
				&& !url.isEmpty()
				&& !url.equalsIgnoreCase("nsfw")
				&& !url.equalsIgnoreCase("self")
				&& !url.equalsIgnoreCase("default");
	}

	private void downloadThumbnail(
			final Context context,
			final boolean allowHighRes,
			final int sizePixels,
			final CacheManager cm,
			final int listId) {

		final RedditParsedPost.ImagePreviewDetails preview = allowHighRes
				? src.getPreview(sizePixels, sizePixels)
				: null;

		final String uriStr;

		if(preview != null) {
			uriStr = preview.url;
		} else {
			uriStr = src.getThumbnailUrl();
		}

		final URI uri = General.uriFromString(uriStr);

		final int priority = Constants.Priority.THUMBNAIL;
		final int fileType = Constants.FileType.THUMBNAIL;

		final RedditAccount anon = RedditAccountManager.getAnon();

		cm.makeRequest(new CacheRequest(
				uri,
				anon,
				null,
				new Priority(priority, listId),
				DownloadStrategyIfNotCached.INSTANCE,
				fileType,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				context,
				new CacheRequestCallbacks() {

					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException> factory,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						onThumbnailStreamAvailable(factory, sizePixels);
					}

					@Override
					public void onFailure(@NonNull final RRError error) {

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.e(
									TAG,
									"Failed to download thumbnail "
											+ uriStr
											+ " with error "
											+ error,
									error.t);
						}
					}
				}));
	}

	// These operations are ordered so as to avoid race conditions
	public Bitmap getThumbnail(
			final ThumbnailLoadedCallback callback,
			final int usageId) {
		this.thumbnailCallback = callback;
		this.usageId = usageId;
		return thumbnailCache;
	}

	public boolean isSelf() {
		return src.isSelfPost();
	}

	public boolean isRead() {
		return mChangeDataManager.isRead(src.getIdAndType());
	}

	public void bind(final RedditPostView boundView) {
		mBoundView = boundView;
		mChangeDataManager.addListener(src.getIdAndType(), this);
	}

	public void unbind(final RedditPostView boundView) {
		if(mBoundView == boundView) {
			mBoundView = null;
			mChangeDataManager.removeListener(src.getIdAndType(), this);
		}
	}

	@Override
	public void onRedditDataChange(final RedditIdAndType thingIdAndType) {
		if(mBoundView != null) {

			final Context context = mBoundView.getContext();

			if(context != null) {
				mBoundView.updateAppearance();
			}
		}
	}

	// TODO handle download failure - show red "X" or something
	public interface ThumbnailLoadedCallback {
		void betterThumbnailAvailable(Bitmap thumbnail, int usageId);
	}

	public void markAsRead(final Context context) {
		final RedditAccount user =
				RedditAccountManager.getInstance(context).getDefaultAccount();
		RedditChangeDataManager.getInstance(user)
				.markRead(TimestampUTC.now(), src.getIdAndType());
	}

	public boolean isUpvoted() {
		return mChangeDataManager.isUpvoted(src.getIdAndType());
	}

	public boolean isDownvoted() {
		return mChangeDataManager.isDownvoted(src.getIdAndType());
	}

	public int getVoteDirection() {
		return isUpvoted() ? 1 : (isDownvoted() ? -1 : 0);
	}

	public boolean isSaved() {
		return mChangeDataManager.isSaved(src.getIdAndType());
	}

	public boolean isHidden() {
		return Boolean.TRUE.equals(mChangeDataManager.isHidden(src.getIdAndType()));
	}

	private void onThumbnailStreamAvailable(
			final GenericFactory<SeekableInputStream, IOException> factory,
			final int desiredSizePixels) {

		try(SeekableInputStream seekableInputStream = factory.create()) {

			final BitmapFactory.Options justDecodeBounds = new BitmapFactory.Options();
			justDecodeBounds.inJustDecodeBounds = true;
			BitmapFactory.decodeStream(seekableInputStream, null, justDecodeBounds);
			final int width = justDecodeBounds.outWidth;
			final int height = justDecodeBounds.outHeight;

			int factor = 1;

			while(width / (factor + 1) > desiredSizePixels
					&& height / (factor + 1) > desiredSizePixels) {
				factor *= 2;
			}

			final BitmapFactory.Options scaledOptions = new BitmapFactory.Options();
			scaledOptions.inSampleSize = factor;

			seekableInputStream.seek(0);
			seekableInputStream.mark(0);

			final Bitmap data = BitmapFactory.decodeStream(
					seekableInputStream,
					null,
					scaledOptions);

			if(data == null) {
				return;
			}
			thumbnailCache = ThumbnailScaler.scale(data, desiredSizePixels);
			if(thumbnailCache != data) {
				data.recycle();
			}

			if(thumbnailCallback != null) {
				thumbnailCallback.betterThumbnailAvailable(
						thumbnailCache,
						usageId);
			}

		} catch(final Throwable t) {
			Log.e(
					TAG,
					"Exception while downloading thumbnail",
					t);
		}
	}
}
