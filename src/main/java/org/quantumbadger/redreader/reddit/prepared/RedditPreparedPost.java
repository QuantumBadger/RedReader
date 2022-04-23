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

import android.app.AlertDialog;
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.text.SpannableStringBuilder;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.ImageButton;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.TooltipCompat;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.CommentEditActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.activities.MainActivity;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.activities.WebViewActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.ScreenreaderPronunciation;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.fragments.PostPropertiesDialog;
import org.quantumbadger.redreader.fragments.ShareOrderDialog;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.image.ThumbnailScaler;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.bezelmenu.VerticalToolbar;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashSet;
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

	private final int mListId;
	private final boolean mShowInlinePreviews;

	// TODO make it possible to turn off in-memory caching when out of memory
	private volatile Bitmap thumbnailCache = null;

	private ThumbnailLoadedCallback thumbnailCallback;
	private int usageId = -1;

	public long lastChange;

	private final boolean showSubreddit;

	private RedditPostView mBoundView = null;

	public enum Action {
		UPVOTE(R.string.action_upvote),
		UNVOTE(R.string.action_vote_remove),
		DOWNVOTE(R.string.action_downvote),
		SAVE(R.string.action_save),
		HIDE(R.string.action_hide),
		UNSAVE(R.string.action_unsave),
		UNHIDE(R.string.action_unhide),
		EDIT(R.string.action_edit),
		DELETE(R.string.action_delete),
		REPORT(R.string.action_report),
		SHARE(R.string.action_share),
		REPLY(R.string.action_reply),
		USER_PROFILE(R.string.action_user_profile),
		EXTERNAL(R.string.action_external),
		PROPERTIES(R.string.action_properties),
		COMMENTS(R.string.action_comments),
		LINK(R.string.action_link),
		COMMENTS_SWITCH(R.string.action_comments_switch),
		LINK_SWITCH(R.string.action_link_switch),
		SHARE_COMMENTS(R.string.action_share_comments),
		SHARE_IMAGE(R.string.action_share_image),
		GOTO_SUBREDDIT(R.string.action_gotosubreddit),
		ACTION_MENU(R.string.action_actionmenu),
		SAVE_IMAGE(R.string.action_save_image),
		COPY(R.string.action_copy_link),
		COPY_SELFTEXT(R.string.action_copy_selftext),
		SELFTEXT_LINKS(R.string.action_selftext_links),
		BACK(R.string.action_back),
		BLOCK(R.string.action_block_subreddit),
		UNBLOCK(R.string.action_unblock_subreddit),
		PIN(R.string.action_pin_subreddit),
		UNPIN(R.string.action_unpin_subreddit),
		SUBSCRIBE(R.string.action_subscribe_subreddit),
		UNSUBSCRIBE(R.string.action_unsubscribe_subreddit);

		public final int descriptionResId;

		Action(final int descriptionResId) {
			this.descriptionResId = descriptionResId;
		}
	}

	// TODO too many parameters
	public RedditPreparedPost(
			final Context context,
			final CacheManager cm,
			final int listId,
			final RedditParsedPost post,
			final long timestamp,
			final boolean showSubreddit,
			final boolean showThumbnails,
			final boolean allowHighResThumbnails,
			final boolean showInlinePreviews) {

		this.src = post;
		this.showSubreddit = showSubreddit;
		mListId = listId;
		mShowInlinePreviews = showInlinePreviews;

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		mChangeDataManager = RedditChangeDataManager.getInstance(user);

		isArchived = post.isArchived();
		isLocked = post.isLocked();
		canModerate = post.canModerate();

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

	public int getListId() {
		return mListId;
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

		final JsonObject preview = src.getSrc().preview;

		if(preview == null) {
			return false;
		}

		return Boolean.TRUE.equals(src.getSrc().is_video)
				|| preview.getAtPath("images", 0, "variants", "mp4").isPresent()
				|| preview.getObject("reddit_video_preview") != null
				|| "v.redd.it".equals(src.getDomain())
				|| "streamable.com".equals(src.getDomain())
				|| "gfycat.com".equals(src.getDomain());
	}

	public static void showActionMenu(
			final BaseActivity activity,
			final RedditPreparedPost post) {

		final EnumSet<Action> itemPref
				= PrefsUtility.pref_menus_post_context_items();

		if(itemPref.isEmpty()) {
			return;
		}

		final RedditAccount user =
				RedditAccountManager.getInstance(activity).getDefaultAccount();

		final ArrayList<RPVMenuItem> menu = new ArrayList<>();

		if(!RedditAccountManager.getInstance(activity)
				.getDefaultAccount()
				.isAnonymous()) {

			if(itemPref.contains(Action.UPVOTE)) {
				if(!post.isUpvoted()) {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_upvote,
							Action.UPVOTE));
				} else {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_upvote_remove,
							Action.UNVOTE));
				}
			}

			if(itemPref.contains(Action.DOWNVOTE)) {
				if(!post.isDownvoted()) {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_downvote,
							Action.DOWNVOTE));
				} else {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_downvote_remove,
							Action.UNVOTE));
				}
			}
		}

		if(itemPref.contains(Action.COMMENTS)) {
			menu.add(new RPVMenuItem(
					String.format(
							activity.getText(R.string.action_comments_with_count).toString(),
							post.src.getSrc().num_comments),
					Action.COMMENTS));
		}

		if(!RedditAccountManager.getInstance(activity).getDefaultAccount().isAnonymous()) {

			if(itemPref.contains(Action.SAVE)) {
				if(!post.isSaved()) {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_save,
							Action.SAVE));
				} else {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_unsave,
							Action.UNSAVE));
				}
			}

			if(itemPref.contains(Action.HIDE)) {
				if(!post.isHidden()) {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_hide,
							Action.HIDE));
				} else {
					menu.add(new RPVMenuItem(
							activity,
							R.string.action_unhide,
							Action.UNHIDE));
				}
			}

			if(itemPref.contains(Action.EDIT)
					&& post.isSelf()
					&& user.username.equalsIgnoreCase(post.src.getAuthor())) {
				menu.add(new RPVMenuItem(activity, R.string.action_edit, Action.EDIT));
			}

			if(itemPref.contains(Action.DELETE) && user.username.equalsIgnoreCase(post.src
					.getAuthor())) {
				menu.add(new RPVMenuItem(
						activity,
						R.string.action_delete,
						Action.DELETE));
			}

			if(itemPref.contains(Action.REPORT)) {
				menu.add(new RPVMenuItem(
						activity,
						R.string.action_report,
						Action.REPORT));
			}

			if(itemPref.contains(Action.REPLY)
					&& !post.isArchived
					&& !(post.isLocked && !post.canModerate)) {
				menu.add(new RPVMenuItem(
						activity,
						R.string.action_reply,
						Action.REPLY));
			}
		}

		if(itemPref.contains(Action.EXTERNAL)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_external,
					Action.EXTERNAL));
		}

		if(itemPref.contains(Action.SELFTEXT_LINKS)
				&& post.src.getRawSelfTextMarkdown() != null
				&& post.src.getRawSelfTextMarkdown().length() > 1) {

			menu.add(new RPVMenuItem(
					activity,
					R.string.action_selftext_links,
					Action.SELFTEXT_LINKS));
		}

		if(itemPref.contains(Action.SAVE_IMAGE) && post.mIsProbablyAnImage) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_save_image,
					Action.SAVE_IMAGE));
		}
		if(itemPref.contains(Action.GOTO_SUBREDDIT)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_gotosubreddit,
					Action.GOTO_SUBREDDIT));
		}
		if(post.showSubreddit) {
			try {

				final SubredditCanonicalId subredditCanonicalId =
						new SubredditCanonicalId(post.src.getSubreddit());

				if(itemPref.contains(Action.BLOCK)) {
					if(PrefsUtility.pref_blocked_subreddits_check(subredditCanonicalId)) {
						menu.add(new RPVMenuItem(
								activity,
								R.string.action_unblock_subreddit,
								Action.UNBLOCK));
					} else {
						menu.add(new RPVMenuItem(
								activity,
								R.string.action_block_subreddit,
								Action.BLOCK));
					}
				}

				if(itemPref.contains(Action.PIN)) {
					if(PrefsUtility.pref_pinned_subreddits_check(subredditCanonicalId)) {
						menu.add(new RPVMenuItem(
								activity,
								R.string.action_unpin_subreddit,
								Action.UNPIN));
					} else {
						menu.add(new RPVMenuItem(
								activity,
								R.string.action_pin_subreddit,
								Action.PIN));
					}
				}

				if(!RedditAccountManager.getInstance(activity)
						.getDefaultAccount()
						.isAnonymous()) {
					if(itemPref.contains(Action.SUBSCRIBE)) {

						final RedditSubredditSubscriptionManager subscriptionManager =
								RedditSubredditSubscriptionManager
										.getSingleton(
												activity,
												RedditAccountManager.getInstance(activity)
														.getDefaultAccount());

						if(subscriptionManager.areSubscriptionsReady()) {

							if(subscriptionManager.getSubscriptionState(
									subredditCanonicalId)
									== SubredditSubscriptionState.SUBSCRIBED) {
								menu.add(new RPVMenuItem(
										activity,
										R.string.action_unsubscribe_subreddit,
										Action.UNSUBSCRIBE));
							} else {
								menu.add(new RPVMenuItem(
										activity,
										R.string.action_subscribe_subreddit,
										Action.SUBSCRIBE));
							}
						}
					}
				}

			} catch(final InvalidSubredditNameException ex) {
				throw new RuntimeException(ex);
			}
		}

		final boolean isRedditVideo = post.src.getUrl().contains("v.redd.it");

		if(itemPref.contains(Action.SHARE)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_share,
					isRedditVideo ? Action.SHARE_COMMENTS : Action.SHARE));
		}
		if(itemPref.contains(Action.SHARE_COMMENTS)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_share_comments,
					Action.SHARE_COMMENTS));
		}
		if(itemPref.contains(Action.SHARE_IMAGE) && post.mIsProbablyAnImage) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_share_image,
					Action.SHARE_IMAGE));
		}
		if(itemPref.contains(Action.COPY)) {
			menu.add(new RPVMenuItem(activity, R.string.action_copy_link, Action.COPY));
		}
		if(itemPref.contains(Action.COPY_SELFTEXT)
				&& post.src.getRawSelfTextMarkdown() != null
				&& post.src.getRawSelfTextMarkdown().length() > 1) {

			menu.add(new RPVMenuItem(
					activity,
					R.string.action_copy_selftext,
					Action.COPY_SELFTEXT));
		}
		if(itemPref.contains(Action.USER_PROFILE)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_user_profile,
					Action.USER_PROFILE));
		}
		if(itemPref.contains(Action.PROPERTIES)) {
			menu.add(new RPVMenuItem(
					activity,
					R.string.action_properties,
					Action.PROPERTIES));
		}


		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		builder.setItems(menuText,
				(dialog, which)
						-> onActionMenuItemSelected(post, activity, menu.get(which).action));

		//builder.setNeutralButton(R.string.dialog_cancel, null);

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	public void performAction(final BaseActivity activity, final Action action) {
		onActionMenuItemSelected(this, activity, action);
	}

	public static void onActionMenuItemSelected(
			final RedditPreparedPost post,
			final BaseActivity activity,
			final Action action) {

		switch(action) {

			case UPVOTE:
				post.action(activity, RedditAPI.ACTION_UPVOTE);
				break;

			case DOWNVOTE:
				post.action(activity, RedditAPI.ACTION_DOWNVOTE);
				break;

			case UNVOTE:
				post.action(activity, RedditAPI.ACTION_UNVOTE);
				break;

			case SAVE:
				post.action(activity, RedditAPI.ACTION_SAVE);
				break;

			case UNSAVE:
				post.action(activity, RedditAPI.ACTION_UNSAVE);
				break;

			case HIDE:
				post.action(activity, RedditAPI.ACTION_HIDE);
				break;

			case UNHIDE:
				post.action(activity, RedditAPI.ACTION_UNHIDE);
				break;
			case EDIT:
				final Intent editIntent = new Intent(activity, CommentEditActivity.class);
				editIntent.putExtra("commentIdAndType", post.src.getIdAndType());
				editIntent.putExtra(
						"commentText",
						StringEscapeUtils.unescapeHtml4(post.src.getRawSelfTextMarkdown()));
				editIntent.putExtra("isSelfPost", true);
				activity.startActivity(editIntent);
				break;


			case DELETE:
				new AlertDialog.Builder(activity)
						.setTitle(R.string.accounts_delete)
						.setMessage(R.string.delete_confirm)
						.setPositiveButton(
								R.string.action_delete,
								(dialog, which) -> post.action(activity, RedditAPI.ACTION_DELETE))
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();
				break;

			case REPORT:

				new AlertDialog.Builder(activity)
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(
								R.string.action_report,
								(dialog, which) -> {
									post.action(activity, RedditAPI.ACTION_REPORT);
									// TODO update the view to show the result
									// TODO don't forget, this also hides
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case EXTERNAL: {
				try {
					final Intent intent = new Intent(Intent.ACTION_VIEW);
					final String url = (activity instanceof WebViewActivity)
							? ((WebViewActivity)activity).getCurrentUrl()
							: post.src.getUrl();
					intent.setData(Uri.parse(url));
					activity.startActivity(intent);

				} catch(final ActivityNotFoundException e) {
					General.quickToast(
							activity,
							R.string.action_not_handled_by_installed_app_toast);
				}
				break;
			}

			case SELFTEXT_LINKS: {

				final HashSet<String> linksInComment =
						LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(post.src
								.getRawSelfTextMarkdown()));

				if(linksInComment.isEmpty()) {
					General.quickToast(activity, R.string.error_toast_no_urls_in_self);

				} else {

					final String[] linksArr =
							linksInComment.toArray(new String[0]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
					builder.setItems(linksArr, (dialog, which) -> {
						LinkHandler.onLinkClicked(
								activity,
								linksArr[which],
								false,
								post.src.getSrc());
						dialog.dismiss();
					});

					final AlertDialog alert = builder.create();
					alert.setTitle(R.string.action_selftext_links);
					alert.setCanceledOnTouchOutside(true);
					alert.show();
				}

				break;
			}

			case SAVE_IMAGE: {
				FileUtils.saveImageAtUri(activity, post.src.getUrl());
				break;
			}

			case SHARE: {

				final String subject
						= PrefsUtility.pref_behaviour_sharing_dialog()
						? post.src.getTitle()
						: null;

				LinkHandler.shareText(
						activity,
						subject,
						post.src.getUrl());

				break;
			}

			case SHARE_COMMENTS: {

				final boolean shareAsPermalink =
						PrefsUtility.pref_behaviour_share_permalink();

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				if(PrefsUtility.pref_behaviour_sharing_include_desc()) {
					mailer.putExtra(
							Intent.EXTRA_SUBJECT,
							String.format(activity.getText(R.string.share_comments_for)
									.toString(), post.src.getTitle())
					);
				}
				if(shareAsPermalink) {
					mailer.putExtra(
							Intent.EXTRA_TEXT,
							Constants.Reddit.getNonAPIUri(post.src.getPermalink())
									.toString());
				} else {
					mailer.putExtra(
							Intent.EXTRA_TEXT,
							Constants.Reddit.getNonAPIUri(Constants.Reddit.PATH_COMMENTS
									+ post.src.getIdAlone())
									.toString());
				}
				if(PrefsUtility.pref_behaviour_sharing_dialog()) {
					ShareOrderDialog.newInstance(mailer)
							.show(activity.getSupportFragmentManager(), null);
				} else {
					activity.startActivity(Intent.createChooser(
							mailer,
							activity.getString(R.string.action_share)));
				}
				break;
			}

			case SHARE_IMAGE: {
				FileUtils.shareImageAtUri(activity, post.src.getUrl());
				break;
			}

			case COPY: {

				final ClipboardManager clipboardManager =
						(ClipboardManager)activity.getSystemService(Context.CLIPBOARD_SERVICE);
				if(clipboardManager != null) {
					final ClipData data = ClipData.newPlainText(
							post.src.getAuthor(),
							post.src.getUrl());
					clipboardManager.setPrimaryClip(data);

					General.quickToast(
							activity.getApplicationContext(),
							R.string.post_link_copied_to_clipboard);
				}
				break;
			}

			case COPY_SELFTEXT: {
				final ClipboardManager clipboardManager =
						(ClipboardManager)activity.getSystemService(Context.CLIPBOARD_SERVICE);
				if(clipboardManager != null) {
					final ClipData data = ClipData.newPlainText(
							post.src.getAuthor(),
							post.src.getRawSelfTextMarkdown());
					clipboardManager.setPrimaryClip(data);

					General.quickToast(
							activity.getApplicationContext(),
							R.string.post_text_copied_to_clipboard);
				}
				break;
			}

			case GOTO_SUBREDDIT: {

				try {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(SubredditPostListURL.getSubreddit(post.src.getSubreddit())
							.generateJsonUri());
					activity.startActivityForResult(intent, 1);

				} catch(final InvalidSubredditNameException e) {
					Toast.makeText(
							activity,
							R.string.invalid_subreddit_name,
							Toast.LENGTH_LONG).show();

				} catch(final Exception e) {
					BugReportActivity.handleGlobalError(
							activity,
							new RuntimeException(
									"Got exception for subreddit: " + post.src.getSubreddit(),
									e));
				}

				break;
			}

			case USER_PROFILE:
				LinkHandler.onLinkClicked(
						activity,
						new UserProfileURL(post.src.getAuthor()).toString());
				break;

			case PROPERTIES:
				PostPropertiesDialog.newInstance(post.src.getSrc())
						.show(activity.getSupportFragmentManager(), null);
				break;

			case COMMENTS:
				((RedditPostView.PostSelectionListener)activity).onPostCommentsSelected(post);

				new Thread() {
					@Override
					public void run() {
						post.markAsRead(activity);
					}
				}.start();

				break;

			case LINK:
				((RedditPostView.PostSelectionListener)activity).onPostSelected(post);
				break;

			case COMMENTS_SWITCH:
				if(!(activity instanceof MainActivity)) {
					activity.finish();
				}
				((RedditPostView.PostSelectionListener)activity).onPostCommentsSelected(
						post);
				break;

			case LINK_SWITCH:
				if(!(activity instanceof MainActivity)) {
					activity.finish();
				}
				((RedditPostView.PostSelectionListener)activity).onPostSelected(post);
				break;

			case ACTION_MENU:
				showActionMenu(activity, post);
				break;

			case REPLY:
				if(post.isArchived) {
					General.quickToast(activity, R.string.error_archived_reply, Toast.LENGTH_SHORT);
					break;
				} else if(post.isLocked && !post.canModerate) {
					General.quickToast(activity, R.string.error_locked_reply, Toast.LENGTH_SHORT);
					break;
				}

				final Intent intent = new Intent(activity, CommentReplyActivity.class);
				intent.putExtra(
						CommentReplyActivity.PARENT_ID_AND_TYPE_KEY,
						post.src.getIdAndType());
				intent.putExtra(
						CommentReplyActivity.PARENT_MARKDOWN_KEY,
						post.src.getUnescapedSelfText());
				activity.startActivity(intent);
				break;

			case BACK:
				activity.onBackPressed();
				break;

			case PIN:

				try {
					PrefsUtility.pref_pinned_subreddits_add(
							activity,
							new SubredditCanonicalId(post.src.getSubreddit()));

				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}

				break;

			case UNPIN:

				try {
					PrefsUtility.pref_pinned_subreddits_remove(
							activity,
							new SubredditCanonicalId(post.src.getSubreddit()));

				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}

				break;

			case BLOCK:

				try {
					PrefsUtility.pref_blocked_subreddits_add(
							activity,
							new SubredditCanonicalId(post.src.getSubreddit()));

				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}

				break;

			case UNBLOCK:

				try {
					PrefsUtility.pref_blocked_subreddits_remove(
							activity,
							new SubredditCanonicalId(post.src.getSubreddit()));

				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}

				break;

			case SUBSCRIBE:

				try {
					final SubredditCanonicalId subredditCanonicalId =
							new SubredditCanonicalId(post.src.getSubreddit());
					final RedditSubredditSubscriptionManager subMan =
							RedditSubredditSubscriptionManager
									.getSingleton(
											activity,
											RedditAccountManager.getInstance(activity)
													.getDefaultAccount());

					if(subMan.getSubscriptionState(subredditCanonicalId)
							== SubredditSubscriptionState.NOT_SUBSCRIBED) {
						subMan.subscribe(subredditCanonicalId, activity);
						Toast.makeText(
								activity,
								R.string.options_subscribing,
								Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(
								activity,
								R.string.mainmenu_toast_subscribed,
								Toast.LENGTH_SHORT).show();
					}
				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				break;

			case UNSUBSCRIBE:

				try {
					final SubredditCanonicalId subredditCanonicalId =
							new SubredditCanonicalId(post.src.getSubreddit());
					final RedditSubredditSubscriptionManager subMan =
							RedditSubredditSubscriptionManager
									.getSingleton(
											activity,
											RedditAccountManager.getInstance(activity)
													.getDefaultAccount());
					if(subMan.getSubscriptionState(subredditCanonicalId)
							== SubredditSubscriptionState.SUBSCRIBED) {
						subMan.unsubscribe(subredditCanonicalId, activity);
						Toast.makeText(
								activity,
								R.string.options_unsubscribing,
								Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(
								activity,
								R.string.mainmenu_toast_not_subscribed,
								Toast.LENGTH_SHORT).show();
					}
				} catch(final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				break;
		}
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
				R.attr.rrGoldBackCol
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

		if(mPostSubtitleItems.contains(PrefsUtility.AppearancePostSubtitleItem.SCORE)) {
			postListDescSb.append(
					String.valueOf(score),
					BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR,
					pointsCol,
					0,
					1f);
			postListDescSb.append(
					" " + context.getString(R.string.subtitle_points) + " ",
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
					" " + context.getString(R.string.subtitle_upvote_ratio) + ") ", 0);
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
					RRTime.formatDurationFrom(
							context,
							src.getCreatedTimeSecsUTC() * 1000,
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
							RRTime.formatDurationFrom(
									context,
									src.getCreatedTimeSecsUTC() * 1000,
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
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						onThumbnailStreamAvailable(factory, sizePixels);
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.e(
									TAG,
									"Failed to download thumbnail "
											+ uriStr
											+ " with status "
											+ httpStatus,
									t);
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
		return mChangeDataManager.isRead(src);
	}

	public void bind(final RedditPostView boundView) {
		mBoundView = boundView;
		mChangeDataManager.addListener(src, this);
	}

	public void unbind(final RedditPostView boundView) {
		if(mBoundView == boundView) {
			mBoundView = null;
			mChangeDataManager.removeListener(src, this);
		}
	}

	@Override
	public void onRedditDataChange(final String thingIdAndType) {
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
				.markRead(RRTime.utcCurrentTimeMillis(), src);
	}

	public void action(
			final AppCompatActivity activity,
			final @RedditAPI.RedditAction int action) {

		final RedditAccount user =
				RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(user.isAnonymous()) {

			AndroidCommon.UI_THREAD_HANDLER.post(() -> General.showMustBeLoggedInDialog(activity));

			return;
		}

		final int lastVoteDirection = getVoteDirection();
		final boolean archived = src.isArchived();


		final long now = RRTime.utcCurrentTimeMillis();

		switch(action) {
			case RedditAPI.ACTION_DOWNVOTE:
				if(!archived) {
					mChangeDataManager.markDownvoted(now, src);
				}
				break;
			case RedditAPI.ACTION_UNVOTE:
				if(!archived) {
					mChangeDataManager.markUnvoted(now, src);
				}
				break;
			case RedditAPI.ACTION_UPVOTE:
				if(!archived) {
					mChangeDataManager.markUpvoted(now, src);
				}
				break;

			case RedditAPI.ACTION_SAVE:
				mChangeDataManager.markSaved(now, src, true);
				break;

			case RedditAPI.ACTION_UNSAVE:
				mChangeDataManager.markSaved(now, src, false);
				break;

			case RedditAPI.ACTION_HIDE:
				mChangeDataManager.markHidden(now, src, true);
				break;

			case RedditAPI.ACTION_UNHIDE:
				mChangeDataManager.markHidden(now, src, false);
				break;

			case RedditAPI.ACTION_REPORT:
				break;
			case RedditAPI.ACTION_DELETE:
				break;

			default:
				throw new RuntimeException("Unknown post action");
		}

		final boolean vote = (action == RedditAPI.ACTION_DOWNVOTE
				| action == RedditAPI.ACTION_UPVOTE
				| action == RedditAPI.ACTION_UNVOTE);

		if(archived && vote) {
			Toast.makeText(activity, R.string.error_archived_vote, Toast.LENGTH_SHORT)
					.show();
			return;
		}

		RedditAPI.action(CacheManager.getInstance(activity),
				new APIResponseHandler.ActionResponseHandler(activity) {
					@Override
					protected void onCallbackException(final Throwable t) {
						BugReportActivity.handleGlobalError(context, t);
					}

					@Override
					protected void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> response) {

						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(
								context,
								type,
								t,
								httpStatus,
								"Reddit API action code: "
										+ action
										+ " "
										+ src.getIdAndType(),
								response);
						General.showResultDialog(activity, error);
					}

					@Override
					protected void onFailure(
							@NonNull final APIFailureType type,
							@Nullable final String debuggingContext,
							@NonNull final Optional<FailedRequestBody> response) {

						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(
								context,
								type,
								debuggingContext,
								response);

						General.showResultDialog(activity, error);
					}

					@Override
					protected void onSuccess() {

						final long now = RRTime.utcCurrentTimeMillis();

						switch(action) {
							case RedditAPI.ACTION_DOWNVOTE:
								mChangeDataManager.markDownvoted(now, src);
								break;

							case RedditAPI.ACTION_UNVOTE:
								mChangeDataManager.markUnvoted(now, src);
								break;

							case RedditAPI.ACTION_UPVOTE:
								mChangeDataManager.markUpvoted(now, src);
								break;

							case RedditAPI.ACTION_SAVE:
								mChangeDataManager.markSaved(now, src, true);
								break;

							case RedditAPI.ACTION_UNSAVE:
								mChangeDataManager.markSaved(now, src, false);
								break;

							case RedditAPI.ACTION_HIDE:
								mChangeDataManager.markHidden(now, src, true);
								break;

							case RedditAPI.ACTION_UNHIDE:
								mChangeDataManager.markHidden(now, src, false);
								break;

							case RedditAPI.ACTION_REPORT:
								break;

							case RedditAPI.ACTION_DELETE:
								General.quickToast(activity, R.string.delete_success);
								break;

							default:
								throw new RuntimeException("Unknown post action");
						}
					}

					private void revertOnFailure() {

						final long now = RRTime.utcCurrentTimeMillis();

						switch(action) {
							case RedditAPI.ACTION_DOWNVOTE:
							case RedditAPI.ACTION_UNVOTE:
							case RedditAPI.ACTION_UPVOTE:
								switch(lastVoteDirection) {
									case -1:
										mChangeDataManager.markDownvoted(now, src);
										break;

									case 0:
										mChangeDataManager.markUnvoted(now, src);
										break;
									case 1:
										mChangeDataManager.markUpvoted(now, src);
										break;
								}

							case RedditAPI.ACTION_SAVE:
								mChangeDataManager.markSaved(now, src, false);
								break;

							case RedditAPI.ACTION_UNSAVE:
								mChangeDataManager.markSaved(now, src, true);
								break;

							case RedditAPI.ACTION_HIDE:
								mChangeDataManager.markHidden(now, src, false);
								break;

							case RedditAPI.ACTION_UNHIDE:
								mChangeDataManager.markHidden(now, src, true);
								break;

							case RedditAPI.ACTION_REPORT:
								break;
							case RedditAPI.ACTION_DELETE:
								break;

							default:
								throw new RuntimeException("Unknown post action");
						}
					}

				}, user, src.getIdAndType(), action, activity);
	}

	public boolean isUpvoted() {
		return mChangeDataManager.isUpvoted(src);
	}

	public boolean isDownvoted() {
		return mChangeDataManager.isDownvoted(src);
	}

	public int getVoteDirection() {
		return isUpvoted() ? 1 : (isDownvoted() ? -1 : 0);
	}

	public boolean isSaved() {
		return mChangeDataManager.isSaved(src);
	}

	public boolean isHidden() {
		return Boolean.TRUE.equals(mChangeDataManager.isHidden(src));
	}

	private static class RPVMenuItem {
		public final String title;
		public final Action action;

		private RPVMenuItem(final String title, final Action action) {
			this.title = title;
			this.action = action;
		}

		private RPVMenuItem(final Context context, final int titleRes, final Action action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public VerticalToolbar generateToolbar(
			final BaseActivity activity,
			final boolean isComments,
			final SideToolbarOverlay overlay) {

		final VerticalToolbar toolbar = new VerticalToolbar(activity);
		final EnumSet<Action> itemsPref = PrefsUtility.pref_menus_post_toolbar_items();

		final Action[] possibleItems = {
				Action.ACTION_MENU,
				isComments ? Action.LINK_SWITCH : Action.COMMENTS_SWITCH,
				Action.UPVOTE,
				Action.DOWNVOTE,
				Action.SAVE,
				Action.HIDE,
				Action.DELETE,
				Action.REPLY,
				Action.EXTERNAL,
				Action.SAVE_IMAGE,
				Action.SHARE,
				Action.COPY,
				Action.USER_PROFILE,
				Action.PROPERTIES
		};

		// TODO make static
		final EnumMap<Action, Integer> iconsDark = new EnumMap<>(Action.class);
		iconsDark.put(Action.ACTION_MENU, R.drawable.dots_vertical_dark);
		iconsDark.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_dark);
		iconsDark.put(
				Action.LINK_SWITCH,
				mIsProbablyAnImage
						? R.drawable.ic_action_image_dark
						: R.drawable.ic_action_link_dark);
		iconsDark.put(Action.UPVOTE, R.drawable.arrow_up_bold_dark);
		iconsDark.put(Action.DOWNVOTE, R.drawable.arrow_down_bold_dark);
		iconsDark.put(Action.SAVE, R.drawable.star_dark);
		iconsDark.put(Action.HIDE, R.drawable.ic_action_cross_dark);
		iconsDark.put(Action.REPLY, R.drawable.ic_action_reply_dark);
		iconsDark.put(Action.EXTERNAL, R.drawable.ic_action_external_dark);
		iconsDark.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_dark);
		iconsDark.put(Action.SHARE, R.drawable.ic_action_share_dark);
		iconsDark.put(Action.COPY, R.drawable.ic_action_copy_dark);
		iconsDark.put(Action.USER_PROFILE, R.drawable.ic_action_person_dark);
		iconsDark.put(Action.PROPERTIES, R.drawable.ic_action_info_dark);

		final EnumMap<Action, Integer> iconsLight = new EnumMap<>(Action.class);
		iconsLight.put(Action.ACTION_MENU, R.drawable.dots_vertical_light);
		iconsLight.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_light);
		iconsLight.put(
				Action.LINK_SWITCH,
				mIsProbablyAnImage
						? R.drawable.ic_action_image_light
						: R.drawable.ic_action_link_light);
		iconsLight.put(Action.UPVOTE, R.drawable.arrow_up_bold_light);
		iconsLight.put(Action.DOWNVOTE, R.drawable.arrow_down_bold_light);
		iconsLight.put(Action.SAVE, R.drawable.star_light);
		iconsLight.put(Action.HIDE, R.drawable.ic_action_cross_light);
		iconsLight.put(Action.REPLY, R.drawable.ic_action_reply_light);
		iconsLight.put(Action.EXTERNAL, R.drawable.ic_action_external_light);
		iconsLight.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_light);
		iconsLight.put(Action.SHARE, R.drawable.ic_action_share_light);
		iconsLight.put(Action.COPY, R.drawable.ic_action_copy_light);
		iconsLight.put(Action.USER_PROFILE, R.drawable.ic_action_person_light);
		iconsLight.put(Action.PROPERTIES, R.drawable.ic_action_info_light);

		for(final Action action : possibleItems) {

			if(action == Action.SAVE_IMAGE && !mIsProbablyAnImage) {
				continue;
			}

			if(itemsPref.contains(action)) {

				final ImageButton ib = (ImageButton)LayoutInflater.from(activity)
						.inflate(
								R.layout.flat_image_button,
								toolbar,
								false);

				final int buttonPadding = General.dpToPixels(activity, 14);
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

				ib.setOnClickListener(v -> {

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

					onActionMenuItemSelected(
							this,
							activity,
							actionToTake);
					overlay.hide();
				});

				Action accessibilityAction = action;

				if(accessibilityAction == Action.UPVOTE && isUpvoted()
						|| accessibilityAction == Action.DOWNVOTE && isDownvoted()) {
					accessibilityAction = Action.UNVOTE;
				}

				if(accessibilityAction == Action.SAVE && isSaved()) {
					accessibilityAction = Action.UNSAVE;
				}

				if(accessibilityAction == Action.HIDE && isHidden()) {
					accessibilityAction = Action.UNHIDE;
				}

				final String text = activity.getString(accessibilityAction.descriptionResId);

				ib.setContentDescription(text);
				TooltipCompat.setTooltipText(ib, text);

				toolbar.addItem(ib);
			}
		}

		return toolbar;
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
