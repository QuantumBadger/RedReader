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

package org.saiditnet.redreader.reddit.prepared;

import android.Manifest;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.preference.PreferenceManager;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.ClipboardManager;
import android.text.SpannableStringBuilder;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageButton;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.activities.*;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.saiditnet.redreader.common.*;
import org.saiditnet.redreader.fragments.PostPropertiesDialog;
import org.saiditnet.redreader.image.SaveImageCallback;
import org.saiditnet.redreader.image.ShareImageCallback;
import org.saiditnet.redreader.image.ThumbnailScaler;
import org.saiditnet.redreader.reddit.APIResponseHandler;
import org.saiditnet.redreader.reddit.RedditAPI;
import org.saiditnet.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;
import org.saiditnet.redreader.reddit.url.SubredditPostListURL;
import org.saiditnet.redreader.reddit.url.UserProfileURL;
import org.saiditnet.redreader.views.RedditPostView;
import org.saiditnet.redreader.views.bezelmenu.SideToolbarOverlay;
import org.saiditnet.redreader.views.bezelmenu.VerticalToolbar;

import java.net.URI;
import java.util.*;

public final class RedditPreparedPost {

	public final RedditParsedPost src;
	private final RedditChangeDataManager mChangeDataManager;

	public SpannableStringBuilder postListDescription;

	public final boolean isArchived;
	public final boolean hasThumbnail;
	public final boolean mIsProbablyAnImage;

	// TODO make it possible to turn off in-memory caching when out of memory
	private volatile Bitmap thumbnailCache = null;

	private static final Object singleImageDecodeLock = new Object();

	private ThumbnailLoadedCallback thumbnailCallback;
	private int usageId = -1;

	public long lastChange = Long.MIN_VALUE;

	private final boolean showSubreddit;

	private RedditPostView boundView = null;

	public enum Action {
		UPVOTE(R.string.action_upvote),
		// UNVOTE(R.string.action_vote_remove),
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
		COPY(R.string.action_copy),
		SELFTEXT_LINKS(R.string.action_selftext_links),
		BACK(R.string.action_back),
		BLOCK(R.string.action_block_subreddit),
		UNBLOCK(R.string.action_unblock_subreddit),
		PIN(R.string.action_pin_subreddit),
		UNPIN(R.string.action_unpin_subreddit),
		SUBSCRIBE(R.string.action_subscribe_subreddit),
		UNSUBSCRIBE(R.string.action_unsubscribe_subreddit),
		UNUPVOTE(R.string.action_upvote_remove),
		UNDOWNVOTE(R.string.action_downvote_remove);

		public final int descriptionResId;

		Action(final int descriptionResId)
		{
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
			final boolean showThumbnails) {

		this.src = post;
		this.showSubreddit = showSubreddit;

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		mChangeDataManager = RedditChangeDataManager.getInstance(user);

		isArchived = post.isArchived();

		mIsProbablyAnImage = LinkHandler.isProbablyAnImage(post.getUrl());

		hasThumbnail = showThumbnails && hasThumbnail(post);

		// TODO parameterise
		final int thumbnailWidth = General.dpToPixels(context, 64);

		if(hasThumbnail && hasThumbnail(post)) {
			downloadThumbnail(context, thumbnailWidth, cm, listId);
		}

		lastChange = timestamp;
		mChangeDataManager.update(timestamp, post.getSrc());

		rebuildSubtitle(context);
	}

	public static void showActionMenu(
			final AppCompatActivity activity,
			final RedditPreparedPost post) {

		final EnumSet<Action> itemPref = PrefsUtility.pref_menus_post_context_items(activity, PreferenceManager.getDefaultSharedPreferences(activity));

		if(itemPref.isEmpty()) return;

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		final ArrayList<RPVMenuItem> menu = new ArrayList<>();

		if(!RedditAccountManager.getInstance(activity).getDefaultAccount().isAnonymous()) {

			if(itemPref.contains(Action.UPVOTE)) {
				if(!post.isUpvoted()) {
					menu.add(new RPVMenuItem(activity, R.string.action_upvote, Action.UPVOTE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_upvote_remove, Action.UNUPVOTE));
				}
			}

			if(itemPref.contains(Action.DOWNVOTE)) {
				if(!post.isDownvoted()) {
					menu.add(new RPVMenuItem(activity, R.string.action_downvote, Action.DOWNVOTE));
				} else {
					menu.add(new RPVMenuItem(activity, R.string.action_downvote_remove, Action.UNDOWNVOTE));
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

			if(itemPref.contains(Action.EDIT) && post.isSelf() && user.username.equalsIgnoreCase(post.src.getAuthor())){
				menu.add(new RPVMenuItem(activity, R.string.action_edit, Action.EDIT));
			}

			if(itemPref.contains(Action.DELETE) && user.username.equalsIgnoreCase(post.src.getAuthor())) {
				menu.add(new RPVMenuItem(activity, R.string.action_delete, Action.DELETE));
			}

			if(itemPref.contains(Action.REPORT)) menu.add(new RPVMenuItem(activity, R.string.action_report, Action.REPORT));
		}

		if(itemPref.contains(Action.EXTERNAL)) menu.add(new RPVMenuItem(activity, R.string.action_external, Action.EXTERNAL));
		if(itemPref.contains(Action.SELFTEXT_LINKS) && post.src.getRawSelfText() != null && post.src.getRawSelfText().length() > 1) menu.add(new RPVMenuItem(activity, R.string.action_selftext_links, Action.SELFTEXT_LINKS));
		if(itemPref.contains(Action.SAVE_IMAGE) && post.mIsProbablyAnImage) menu.add(new RPVMenuItem(activity, R.string.action_save_image, Action.SAVE_IMAGE));
		if(itemPref.contains(Action.GOTO_SUBREDDIT)) menu.add(new RPVMenuItem(activity, R.string.action_gotosubreddit, Action.GOTO_SUBREDDIT));
		if (post.showSubreddit){
			try {
				String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());

				if (itemPref.contains(Action.BLOCK) && post.showSubreddit) {
					final List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));

					if (blockedSubreddits.contains(subredditCanonicalName)) {
						menu.add(new RPVMenuItem(activity, R.string.action_unblock_subreddit, Action.UNBLOCK));
					} else {
						menu.add(new RPVMenuItem(activity, R.string.action_block_subreddit, Action.BLOCK));
					}
				}

				if (itemPref.contains(Action.PIN) && post.showSubreddit) {
					List<String> pinnedSubreddits = PrefsUtility.pref_pinned_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));
					if (pinnedSubreddits.contains(subredditCanonicalName)) {
						menu.add(new RPVMenuItem(activity, R.string.action_unpin_subreddit, Action.UNPIN));
					} else {
						menu.add(new RPVMenuItem(activity, R.string.action_pin_subreddit, Action.PIN));
					}
				}

				if (!RedditAccountManager.getInstance(activity).getDefaultAccount().isAnonymous()) {
					if (itemPref.contains(Action.SUBSCRIBE)) {

						final RedditSubredditSubscriptionManager subscriptionManager = RedditSubredditSubscriptionManager
								.getSingleton(activity, RedditAccountManager.getInstance(activity).getDefaultAccount());

						if(subscriptionManager.areSubscriptionsReady()) {

							if(subscriptionManager.getSubscriptionState(subredditCanonicalName)
									== RedditSubredditSubscriptionManager.SubredditSubscriptionState.SUBSCRIBED) {
								menu.add(new RPVMenuItem(activity, R.string.action_unsubscribe_subreddit, Action.UNSUBSCRIBE));
							} else {
								menu.add(new RPVMenuItem(activity, R.string.action_subscribe_subreddit, Action.SUBSCRIBE));
							}
						}
					}
				}

			} catch (RedditSubreddit.InvalidSubredditNameException ex){
				throw new RuntimeException(ex);
			}
		}

		if(itemPref.contains(Action.SHARE)) menu.add(new RPVMenuItem(activity, R.string.action_share, Action.SHARE));
		if(itemPref.contains(Action.SHARE_COMMENTS)) menu.add(new RPVMenuItem(activity, R.string.action_share_comments, Action.SHARE_COMMENTS));
		if(itemPref.contains(Action.SHARE_IMAGE) && post.mIsProbablyAnImage) menu.add(new RPVMenuItem(activity, R.string.action_share_image, Action.SHARE_IMAGE));
		if(itemPref.contains(Action.COPY)) menu.add(new RPVMenuItem(activity, R.string.action_copy, Action.COPY));
		if(itemPref.contains(Action.USER_PROFILE)) menu.add(new RPVMenuItem(activity, R.string.action_user_profile, Action.USER_PROFILE));
		if(itemPref.contains(Action.PROPERTIES)) menu.add(new RPVMenuItem(activity, R.string.action_properties, Action.PROPERTIES));


		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		builder.setItems(menuText, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				onActionMenuItemSelected(post, activity, menu.get(which).action);
			}
		});

		//builder.setNeutralButton(R.string.dialog_cancel, null);

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	public static void onActionMenuItemSelected(final RedditPreparedPost post, final AppCompatActivity activity, final Action action) {

		switch(action) {

			case UPVOTE:
				post.action(activity, RedditAPI.ACTION_UPVOTE);
				break;

			case DOWNVOTE:
				post.action(activity, RedditAPI.ACTION_DOWNVOTE);
				break;

			// case UNVOTE:
			// 	post.action(activity, RedditAPI.ACTION_UNVOTE);
			// 	break;

			case UNUPVOTE:
				post.action(activity, RedditAPI.ACTION_UNUPVOTE);
				break;

			case UNDOWNVOTE:
				post.action(activity, RedditAPI.ACTION_UNDOWNVOTE);
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
				editIntent.putExtra("commentText", StringEscapeUtils.unescapeHtml4(post.src.getRawSelfText()));
				editIntent.putExtra("isSelfPost", true);
				activity.startActivity(editIntent);
				break;


			case DELETE:
				new AlertDialog.Builder(activity)
						.setTitle(R.string.accounts_delete)
						.setMessage(R.string.delete_confirm)
						.setPositiveButton(R.string.action_delete,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(final DialogInterface dialog, final int which) {
										post.action(activity, RedditAPI.ACTION_DELETE);
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();
				break;

			case REPORT:

				new AlertDialog.Builder(activity)
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(R.string.action_report,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(final DialogInterface dialog, final int which) {
										post.action(activity, RedditAPI.ACTION_REPORT);
										// TODO update the view to show the result
										// TODO don't forget, this also hides
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case EXTERNAL: {
				final Intent intent = new Intent(Intent.ACTION_VIEW);
				String url = (activity instanceof WebViewActivity) ? ((WebViewActivity) activity).getCurrentUrl() : post.src.getUrl();
				intent.setData(Uri.parse(url));
				activity.startActivity(intent);
				break;
			}

			case SELFTEXT_LINKS: {

				final HashSet<String> linksInComment = LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(post.src.getRawSelfText()));

				if(linksInComment.isEmpty()) {
					General.quickToast(activity, R.string.error_toast_no_urls_in_self);

				} else {

					final String[] linksArr = linksInComment.toArray(new String[linksInComment.size()]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
					builder.setItems(linksArr, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							LinkHandler.onLinkClicked(activity, linksArr[which], false, post.src.getSrc());
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

				((BaseActivity)activity).requestPermissionWithCallback(Manifest.permission.WRITE_EXTERNAL_STORAGE, new SaveImageCallback(activity, post.src.getUrl()));
				break;
			}

			case SHARE: {

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				if (PrefsUtility.pref_behaviour_sharing_include_desc(activity,
						PreferenceManager.getDefaultSharedPreferences(activity))) {
					mailer.putExtra(Intent.EXTRA_SUBJECT, post.src.getTitle());
				}
				mailer.putExtra(Intent.EXTRA_TEXT, post.src.getUrl());
				activity.startActivity(Intent.createChooser(mailer, activity.getString(R.string.action_share)));
				break;
			}

			case SHARE_COMMENTS: {

				final boolean shareAsPermalink = PrefsUtility.pref_behaviour_share_permalink(activity, PreferenceManager.getDefaultSharedPreferences(activity));

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				if (PrefsUtility.pref_behaviour_sharing_include_desc(activity,
						PreferenceManager.getDefaultSharedPreferences(activity))) {
					mailer.putExtra(Intent.EXTRA_SUBJECT,
							String.format(activity.getText(R.string.share_comments_for).toString(), post.src.getTitle())
					);
				}
				if (shareAsPermalink) {
					mailer.putExtra(Intent.EXTRA_TEXT, Constants.Reddit.getNonAPIUri(post.src.getPermalink()).toString());
				} else {
					mailer.putExtra(Intent.EXTRA_TEXT, Constants.Reddit.getNonAPIUri(Constants.Reddit.PATH_COMMENTS + post.src.getIdAlone()).toString());
				}
				activity.startActivity(Intent.createChooser(mailer, activity.getString(R.string.action_share_comments)));
				break;
			}

			case SHARE_IMAGE: {

				((BaseActivity)activity).requestPermissionWithCallback(Manifest.permission.WRITE_EXTERNAL_STORAGE, new ShareImageCallback(activity, post.src.getUrl()));

				break;
			}

			case COPY: {

				ClipboardManager manager = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
				manager.setText(post.src.getUrl());
				break;
			}

			case GOTO_SUBREDDIT: {

				try {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(SubredditPostListURL.getSubreddit(post.src.getSubreddit()).generateJsonUri());
					activity.startActivityForResult(intent, 1);

				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					Toast.makeText(activity, R.string.invalid_subreddit_name, Toast.LENGTH_LONG).show();
				}

				break;
			}

			case USER_PROFILE:
				LinkHandler.onLinkClicked(activity, new UserProfileURL(post.src.getAuthor()).toString());
				break;

			case PROPERTIES:
				PostPropertiesDialog.newInstance(post.src.getSrc()).show(activity.getSupportFragmentManager(), null);
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
				intent.putExtra(CommentReplyActivity.PARENT_ID_AND_TYPE_KEY, post.src.getIdAndType());
				intent.putExtra(CommentReplyActivity.PARENT_MARKDOWN_KEY, post.src.getUnescapedSelfText());
				activity.startActivity(intent);
				break;

			case BACK:
				activity.onBackPressed();
				break;

			case PIN:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					List<String> pinnedSubreddits = PrefsUtility.pref_pinned_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));
					if (!pinnedSubreddits.contains(subredditCanonicalName)){
						PrefsUtility.pref_pinned_subreddits_add(
								activity,
								PreferenceManager.getDefaultSharedPreferences(activity),
								subredditCanonicalName);
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_pinned, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}

				break;

			case UNPIN:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					List<String> pinnedSubreddits = PrefsUtility.pref_pinned_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));
					if (pinnedSubreddits.contains(subredditCanonicalName)) {
						PrefsUtility.pref_pinned_subreddits_remove(
								activity,
								PreferenceManager.getDefaultSharedPreferences(activity),
								subredditCanonicalName);
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_not_pinned, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e){
					throw new RuntimeException(e);
				}
				break;

			case BLOCK:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));
					if (!blockedSubreddits.contains(subredditCanonicalName)) {
						PrefsUtility.pref_blocked_subreddits_add(
								activity,
								PreferenceManager.getDefaultSharedPreferences(activity),
								subredditCanonicalName);
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_blocked, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e){
					throw new RuntimeException(e);
				}
				break;

			case UNBLOCK:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(activity, PreferenceManager.getDefaultSharedPreferences(activity));
					if (blockedSubreddits.contains(subredditCanonicalName)) {
						PrefsUtility.pref_blocked_subreddits_remove(
								activity,
								PreferenceManager.getDefaultSharedPreferences(activity),
								subredditCanonicalName);
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_not_blocked, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e){
					throw new RuntimeException(e);
				}
				break;

			case SUBSCRIBE:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					RedditSubredditSubscriptionManager subMan = RedditSubredditSubscriptionManager
							.getSingleton(activity, RedditAccountManager.getInstance(activity).getDefaultAccount());

					if (subMan.getSubscriptionState(subredditCanonicalName) == RedditSubredditSubscriptionManager.SubredditSubscriptionState.NOT_SUBSCRIBED) {
						subMan.subscribe(subredditCanonicalName, activity);
						Toast.makeText(activity, R.string.options_subscribing, Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_subscribed, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				break;

			case UNSUBSCRIBE:

				try {
					String subredditCanonicalName = RedditSubreddit.getCanonicalName(post.src.getSubreddit());
					RedditSubredditSubscriptionManager subMan = RedditSubredditSubscriptionManager
							.getSingleton(activity, RedditAccountManager.getInstance(activity).getDefaultAccount());
					if (subMan.getSubscriptionState(subredditCanonicalName) == RedditSubredditSubscriptionManager.SubredditSubscriptionState.SUBSCRIBED) {
						subMan.unsubscribe(subredditCanonicalName, activity);
						Toast.makeText(activity, R.string.options_unsubscribing, Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(activity, R.string.mainmenu_toast_not_subscribed, Toast.LENGTH_SHORT).show();
					}
				} catch (RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				break;
		}
	}

	public int computeScore() {

		int score = src.getScoreExcludingOwnVote();

		// if(isUpvoted()) {
		// 	score++;
		// } else if(isDownvoted()) {
		// 	score--;
		// }

		if(isUpvoted()) {
			score = score + 2;
		}
		if(isDownvoted()) {
			score++;
		}

		return score;
	}

	private void rebuildSubtitle(Context context) {

		// TODO customise display
		// TODO preference for the X days, X hours thing

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
				R.attr.rrPostSubtitleBoldCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol,
				R.attr.rrFlairBackCol,
				R.attr.rrFlairTextCol,
				R.attr.rrGoldTextCol,
				R.attr.rrGoldBackCol
		});

		final int boldCol = appearance.getColor(0, 255),
				rrPostSubtitleUpvoteCol = appearance.getColor(1, 255),
				rrPostSubtitleDownvoteCol = appearance.getColor(2, 255),
				rrFlairBackCol = appearance.getColor(3, 255),
				rrFlairTextCol = appearance.getColor(4, 255),
				rrGoldTextCol = appearance.getColor(5, 255),
				rrGoldBackCol = appearance.getColor(6, 255);

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

		if(src.isSpoiler()) {
			postListDescSb.append(" SPOILER ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.rgb(50, 50, 50), 1f);
			postListDescSb.append("  ", 0);
		}

		if(src.isStickied()) {
			postListDescSb.append(" STICKY ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.rgb(0, 170, 0), 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		if(src.isNsfw()) {
			postListDescSb.append(" NSFW ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					Color.WHITE, Color.RED, 1f); // TODO color?
			postListDescSb.append("  ", 0);
		}

		if(src.getFlairText() != null) {
			postListDescSb.append(" " + src.getFlairText() + "\u200E ", BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR,
					rrFlairTextCol, rrFlairBackCol, 1f);
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(String.valueOf(score), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, pointsCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_points) + " ", 0);

		if (src.getGoldAmount() > 0) {
			postListDescSb.append(" ", 0);
			postListDescSb.append(" " + context.getString(R.string.gold) + " x" + src.getGoldAmount() + " ",
					BetterSSB.FOREGROUND_COLOR | BetterSSB.BACKGROUND_COLOR, rrGoldTextCol, rrGoldBackCol, 1f);
			postListDescSb.append("  ", 0);
		}

		postListDescSb.append(RRTime.formatDurationFrom(context, src.getCreatedTimeSecsUTC() * 1000), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		postListDescSb.append(" " + context.getString(R.string.subtitle_by) + " ", 0);
		postListDescSb.append(src.getAuthor(), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);

		if(showSubreddit) {
			postListDescSb.append(" " + context.getString(R.string.subtitle_to) + " ", 0);
			postListDescSb.append(src.getSubreddit(), BetterSSB.BOLD | BetterSSB.FOREGROUND_COLOR, boldCol, 0, 1f);
		}

		postListDescSb.append(" (" + src.getDomain() + ")", 0);

		postListDescription = postListDescSb.get();
	}

	// lol, reddit api
	private static boolean hasThumbnail(final RedditParsedPost post) {

		final String url = post.getThumbnailUrl();

		return url != null
				&& url.length() != 0
				&& !url.equalsIgnoreCase("nsfw")
				&& !url.equalsIgnoreCase("self")
				&& !url.equalsIgnoreCase("default");
	}

	private void downloadThumbnail(final Context context, final int widthPixels, final CacheManager cm, final int listId) {

		final String uriStr = src.getThumbnailUrl();
		final URI uri = General.uriFromString(uriStr);

		final int priority = Constants.Priority.THUMBNAIL;
		final int fileType = Constants.FileType.THUMBNAIL;

		final RedditAccount anon = RedditAccountManager.getAnon();

		cm.makeRequest(new CacheRequest(uri, anon, null, priority, listId, DownloadStrategyIfNotCached.INSTANCE, fileType, CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE, false, false, context) {

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
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {

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
		return src.isSelfPost();
	}

	public boolean isRead() {
		return mChangeDataManager.isRead(src);
	}

	public void bind(RedditPostView boundView) {
		this.boundView = boundView;
	}

	public void unbind(RedditPostView boundView) {
		if(this.boundView == boundView) this.boundView = null;
	}

	// TODO handle download failure - show red "X" or something
	public interface ThumbnailLoadedCallback {
		void betterThumbnailAvailable(Bitmap thumbnail, int usageId);
	}

	public void markAsRead(final Context context) {
		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		RedditChangeDataManager.getInstance(user).markRead(RRTime.utcCurrentTimeMillis(), src);
		refreshView(context);
	}

	public void refreshView(final Context context) {
		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				rebuildSubtitle(context);
				if(boundView != null) {
					boundView.updateAppearance();
				}
			}
		});
	}

	public void action(final AppCompatActivity activity, final @RedditAPI.RedditAction int action) {

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(user.isAnonymous()) {

			AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
				@Override
				public void run() {
					Toast.makeText(activity, activity.getString(R.string.error_toast_notloggedin), Toast.LENGTH_SHORT).show();
				}
			});

			return;
		}

		// SAIDIT - Warning inaccurate
		final int lastVoteDirection = getVoteDirection();
		final boolean archived = src.isArchived();


		final long now = RRTime.utcCurrentTimeMillis();

		switch(action) {
			case RedditAPI.ACTION_DOWNVOTE:
				if(!archived) {
					mChangeDataManager.markDownvoted(now, src);
				}
				break;
			// case RedditAPI.ACTION_UNVOTE:
			// 	if(!archived) {
			// 		mChangeDataManager.markUnvoted(now, src);
			// 	}
			// 	break;
			case RedditAPI.ACTION_UPVOTE:
				if(!archived) {
					mChangeDataManager.markUpvoted(now, src);
				}
				break;

			case RedditAPI.ACTION_UNUPVOTE:
				if(!archived) {
					mChangeDataManager.markUnUpvoted(now, src);
				}
				break;

			case RedditAPI.ACTION_UNDOWNVOTE:
				if(!archived) {
					mChangeDataManager.markUnDownvoted(now, src);
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

			case RedditAPI.ACTION_REPORT: break;
			case RedditAPI.ACTION_DELETE: break;

			default:
				throw new RuntimeException("Unknown post action");
		}

		refreshView(activity);

		boolean vote = (action == RedditAPI.ACTION_DOWNVOTE
				| action == RedditAPI.ACTION_UPVOTE
				// | action == RedditAPI.ACTION_UNVOTE);
				| action == RedditAPI.ACTION_UNUPVOTE
				| action == RedditAPI.ACTION_UNDOWNVOTE);

		if(archived && vote){
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
					protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
						revertOnFailure();
						if(t != null) t.printStackTrace();

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status,
								"Reddit API action code: " + action + " " + src.getIdAndType());
						AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onSuccess(@Nullable final String redirectUrl) {

						final long now = RRTime.utcCurrentTimeMillis();

						switch(action) {
							case RedditAPI.ACTION_DOWNVOTE:
								mChangeDataManager.markDownvoted(now, src);
								break;

							// case RedditAPI.ACTION_UNVOTE:
							// 	mChangeDataManager.markUnvoted(now, src);
							// 	break;

							case RedditAPI.ACTION_UPVOTE:
								mChangeDataManager.markUpvoted(now, src);
								break;

							case RedditAPI.ACTION_UNUPVOTE:
								mChangeDataManager.markUnUpvoted(now, src);
								break;

							case RedditAPI.ACTION_UNDOWNVOTE:
								mChangeDataManager.markUnDownvoted(now, src);
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

							case RedditAPI.ACTION_REPORT: break;

							case RedditAPI.ACTION_DELETE:
								General.quickToast(activity, R.string.delete_success);
								break;

							default:
								throw new RuntimeException("Unknown post action");
						}

						refreshView(context);
					}

					private void revertOnFailure() {

						final long now = RRTime.utcCurrentTimeMillis();

						switch(action) {
							// case RedditAPI.ACTION_DOWNVOTE:
							// case RedditAPI.ACTION_UNVOTE:
							// case RedditAPI.ACTION_UPVOTE:
							// 	switch(lastVoteDirection) {
							// 		case -1:
							// 			mChangeDataManager.markDownvoted(now, src);
							// 			break;

							// 		case 0:
							// 			mChangeDataManager.markUnvoted(now, src);
							// 			break;
							// 		case 1:
							// 			mChangeDataManager.markUpvoted(now, src);
							// 			break;
							// 	}

							case RedditAPI.ACTION_DOWNVOTE:
								mChangeDataManager.markDownvoted(now, src);
								break;

							case RedditAPI.ACTION_UPVOTE:
								mChangeDataManager.markUpvoted(now, src);
								break;

							case RedditAPI.ACTION_UNUPVOTE:
								mChangeDataManager.markUnUpvoted(now, src);
								break;

							case RedditAPI.ACTION_UNDOWNVOTE:
								mChangeDataManager.markUnDownvoted(now, src);
								break;

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

							case RedditAPI.ACTION_REPORT: break;
							case RedditAPI.ACTION_DELETE: break;

							default:
								throw new RuntimeException("Unknown post action");
						}

						refreshView(context);
					}

				}, user, src.getIdAndType(), action, activity);
	}

	public boolean isUpvoted() {
		return mChangeDataManager.isUpvoted(src);
	}

	public boolean isDownvoted() {
		return mChangeDataManager.isDownvoted(src);
	}

	// SAIDIT - Warning inaccurate
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

		private RPVMenuItem(Context context, int titleRes, Action action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public VerticalToolbar generateToolbar(
			final AppCompatActivity activity,
			boolean isComments,
			final SideToolbarOverlay overlay) {

		final VerticalToolbar toolbar = new VerticalToolbar(activity);
		final EnumSet<Action> itemsPref = PrefsUtility.pref_menus_post_toolbar_items(activity, PreferenceManager.getDefaultSharedPreferences(activity));

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
		iconsDark.put(Action.ACTION_MENU, R.drawable.ic_action_overflow);
		iconsDark.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_dark);
		iconsDark.put(Action.LINK_SWITCH, mIsProbablyAnImage ? R.drawable.ic_action_image_dark : R.drawable.ic_action_link_dark);
		iconsDark.put(Action.UPVOTE, R.drawable.action_upvote_dark);
		iconsDark.put(Action.DOWNVOTE, R.drawable.action_downvote_dark);
		iconsDark.put(Action.SAVE, R.drawable.ic_action_star_filled_dark);
		iconsDark.put(Action.HIDE, R.drawable.ic_action_cross_dark);
		iconsDark.put(Action.REPLY, R.drawable.ic_action_reply_dark);
		iconsDark.put(Action.EXTERNAL, R.drawable.ic_action_external_dark);
		iconsDark.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_dark);
		iconsDark.put(Action.SHARE, R.drawable.ic_action_share_dark);
		iconsDark.put(Action.COPY, R.drawable.ic_action_copy_dark);
		iconsDark.put(Action.USER_PROFILE, R.drawable.ic_action_person_dark);
		iconsDark.put(Action.PROPERTIES, R.drawable.ic_action_info_dark);

		final EnumMap<Action, Integer> iconsLight = new EnumMap<>(Action.class);
		iconsLight.put(Action.ACTION_MENU, R.drawable.ic_action_overflow);
		iconsLight.put(Action.COMMENTS_SWITCH, R.drawable.ic_action_comments_light);
		iconsLight.put(Action.LINK_SWITCH, mIsProbablyAnImage ? R.drawable.ic_action_image_light : R.drawable.ic_action_link_light);
		iconsLight.put(Action.UPVOTE, R.drawable.action_upvote_light);
		iconsLight.put(Action.DOWNVOTE, R.drawable.action_downvote_light);
		iconsLight.put(Action.SAVE, R.drawable.ic_action_star_filled_light);
		iconsLight.put(Action.HIDE, R.drawable.ic_action_cross_light);
		iconsLight.put(Action.REPLY, R.drawable.ic_action_reply_light);
		iconsLight.put(Action.EXTERNAL, R.drawable.ic_action_external_light);
		iconsLight.put(Action.SAVE_IMAGE, R.drawable.ic_action_save_light);
		iconsLight.put(Action.SHARE, R.drawable.ic_action_share_light);
		iconsLight.put(Action.COPY, R.drawable.ic_action_copy_light);
		iconsLight.put(Action.USER_PROFILE, R.drawable.ic_action_person_light);
		iconsLight.put(Action.PROPERTIES, R.drawable.ic_action_info_light);

		for(final Action action : possibleItems) {

			if(action == Action.SAVE_IMAGE && !mIsProbablyAnImage) continue;

			if(itemsPref.contains(action)) {

				final ImageButton ib = (ImageButton) LayoutInflater.from(activity).inflate(R.layout.flat_image_button, toolbar, false);

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

				ib.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(View v) {

						final Action actionToTake;

						switch(action) {
							case UPVOTE:
								actionToTake = isUpvoted() ? Action.UNUPVOTE : Action.UPVOTE;
								break;

							case DOWNVOTE:
								actionToTake = isDownvoted() ? Action.UNDOWNVOTE : Action.DOWNVOTE;
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

				Action accessibilityAction = action;

				// if(accessibilityAction == Action.UPVOTE && isUpvoted()
				// 		|| accessibilityAction == Action.DOWNVOTE && isDownvoted())
				// {
				// 	accessibilityAction = Action.UNVOTE;
				// }
				if(accessibilityAction == Action.UPVOTE && isUpvoted())
				{
					accessibilityAction = Action.UNUPVOTE;
				} else if (accessibilityAction == Action.DOWNVOTE && isDownvoted()) {
					accessibilityAction = Action.UNDOWNVOTE;
				}

				if(accessibilityAction == Action.SAVE && isSaved())
				{
					accessibilityAction = Action.UNSAVE;
				}

				if(accessibilityAction == Action.HIDE && isHidden())
				{
					accessibilityAction = Action.UNHIDE;
				}

				final int textRes = accessibilityAction.descriptionResId;

				ib.setContentDescription(activity.getString(textRes));

				ib.setOnLongClickListener(new View.OnLongClickListener()
				{
					@Override
					public boolean onLongClick(final View view)
					{
						General.quickToast(activity, textRes);
						return true;
					}
				});

				toolbar.addItem(ib);
			}
		}

		return toolbar;
	}
}
