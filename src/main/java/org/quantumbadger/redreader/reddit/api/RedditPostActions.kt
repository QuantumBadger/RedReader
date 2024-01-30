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

package org.quantumbadger.redreader.reddit.api

import android.content.ActivityNotFoundException
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.DialogInterface
import android.content.Intent
import android.graphics.Color
import android.view.LayoutInflater
import android.widget.ImageButton
import android.widget.Toast
import androidx.annotation.StringRes
import androidx.appcompat.widget.TooltipCompat
import com.google.android.material.dialog.MaterialAlertDialogBuilder
import org.apache.commons.text.StringEscapeUtils
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.activities.BugReportActivity
import org.quantumbadger.redreader.activities.CommentEditActivity
import org.quantumbadger.redreader.activities.CommentReplyActivity
import org.quantumbadger.redreader.activities.MainActivity
import org.quantumbadger.redreader.activities.PostListingActivity
import org.quantumbadger.redreader.activities.WebViewActivity
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.common.AndroidCommon
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.FileUtils
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.common.PrefsUtility.PostFlingAction
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.fragments.PostPropertiesDialog
import org.quantumbadger.redreader.reddit.APIResponseHandler.ActionResponseHandler
import org.quantumbadger.redreader.reddit.RedditAPI
import org.quantumbadger.redreader.reddit.RedditAPI.RedditAction
import org.quantumbadger.redreader.reddit.api.RedditPostActions.ActionDescriptionPair.Companion.from
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL
import org.quantumbadger.redreader.reddit.url.UserProfileURL
import org.quantumbadger.redreader.views.AccessibilityActionManager
import org.quantumbadger.redreader.views.RedditPostView.PostSelectionListener
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay
import org.quantumbadger.redreader.views.bezelmenu.VerticalToolbar


object RedditPostActions {

	enum class Action(@StringRes val descriptionResId: Int) {
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
		SHARE(R.string.action_share_link),
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
		UNSUBSCRIBE(R.string.action_unsubscribe_subreddit),
		CROSSPOST_ORIGIN(R.string.action_crosspost_origin)

	}

	data class ActionDescriptionPair(
		val action: Action,
		@StringRes val descriptionRes: Int
	) {
		companion object {
			@JvmStatic
			fun from(
				post: RedditPreparedPost,
				pref: PostFlingAction
			): ActionDescriptionPair? {
				return when (pref) {
					PostFlingAction.UPVOTE -> if (post.isUpvoted) {
						ActionDescriptionPair(
							Action.UNVOTE,
							R.string.action_vote_remove
						)
					} else {
						ActionDescriptionPair(
							Action.UPVOTE,
							R.string.action_upvote
						)
					}

					PostFlingAction.DOWNVOTE -> if (post.isDownvoted) {
						ActionDescriptionPair(
							Action.UNVOTE,
							R.string.action_vote_remove
						)
					} else {
						ActionDescriptionPair(
							Action.DOWNVOTE,
							R.string.action_downvote
						)
					}

					PostFlingAction.SAVE -> if (post.isSaved) {
						ActionDescriptionPair(
							Action.UNSAVE,
							R.string.action_unsave
						)
					} else {
						ActionDescriptionPair(
							Action.SAVE,
							R.string.action_save
						)
					}

					PostFlingAction.HIDE -> if (post.isHidden) {
						ActionDescriptionPair(
							Action.UNHIDE,
							R.string.action_unhide
						)
					} else {
						ActionDescriptionPair(
							Action.HIDE,
							R.string.action_hide
						)
					}

					PostFlingAction.COMMENTS -> ActionDescriptionPair(
						Action.COMMENTS,
						R.string.action_comments_short
					)

					PostFlingAction.LINK -> ActionDescriptionPair(
						Action.LINK,
						R.string.action_link_short
					)

					PostFlingAction.BROWSER -> ActionDescriptionPair(
						Action.EXTERNAL,
						R.string.action_external_short
					)

					PostFlingAction.REPORT -> ActionDescriptionPair(
						Action.REPORT,
						R.string.action_report
					)

					PostFlingAction.SAVE_IMAGE -> ActionDescriptionPair(
						Action.SAVE_IMAGE,
						R.string.action_save_image
					)

					PostFlingAction.GOTO_SUBREDDIT -> ActionDescriptionPair(
						Action.GOTO_SUBREDDIT,
						R.string.action_gotosubreddit
					)

					PostFlingAction.SHARE -> ActionDescriptionPair(
						Action.SHARE,
						R.string.action_share_link
					)

					PostFlingAction.SHARE_COMMENTS -> ActionDescriptionPair(
						Action.SHARE_COMMENTS,
						R.string.action_share_comments
					)

					PostFlingAction.SHARE_IMAGE -> ActionDescriptionPair(
						Action.SHARE_IMAGE,
						R.string.action_share_image
					)

					PostFlingAction.COPY -> ActionDescriptionPair(
						Action.COPY,
						R.string.action_copy_link
					)

					PostFlingAction.USER_PROFILE -> ActionDescriptionPair(
						Action.USER_PROFILE,
						R.string.action_user_profile_short
					)

					PostFlingAction.PROPERTIES -> ActionDescriptionPair(
						Action.PROPERTIES,
						R.string.action_properties
					)

					PostFlingAction.ACTION_MENU -> ActionDescriptionPair(
						Action.ACTION_MENU,
						R.string.action_actionmenu_short
					)

					PostFlingAction.BACK -> ActionDescriptionPair(
						Action.BACK,
						R.string.action_back
					)

					PostFlingAction.DISABLED -> null
				}
			}
		}
	}

	private data class RPVMenuItem(
		val title: String,
		val action: Action
	) {
		constructor(context: Context, titleRes: Int, action: Action) :
				this(context.getString(titleRes), action)
	}

	fun setupAccessibilityActions(
		accessibilityActionManager: AccessibilityActionManager,
		post: RedditPreparedPost,
		activity: BaseActivity,
		isOpen: Boolean
	) {
		fun addAccessibilityActionFromDescriptionPair(
			pair: ActionDescriptionPair?
		) {
			if (pair == null) {
				return
			}
			accessibilityActionManager.addAction(pair.descriptionRes) {
				onActionMenuItemSelected(
					post,
					activity,
					pair.action
				)
			}
		}

		val defaultAccount = RedditAccountManager.getInstance(activity).defaultAccount
		val isOP = defaultAccount.username.equals(
			post.src.author,
			ignoreCase = true
		)
		val isAuthenticated = defaultAccount.isNotAnonymous

		accessibilityActionManager.removeAllActions()

		if (isOpen) {
			// TODO: add an action here to jump focus from the body of the post to its comments.
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.GOTO_SUBREDDIT))
			if (isAuthenticated) {
				if (!post.isArchived && !(post.isLocked && !post.canModerate))
					addAccessibilityActionFromDescriptionPair(
						ActionDescriptionPair(
							Action.REPLY,
							R.string.action_reply
						)
					)
				if (isOP && post.isSelf)
					addAccessibilityActionFromDescriptionPair(
						ActionDescriptionPair(
							Action.EDIT,
							R.string.action_edit
						)
					)
			}
		} else {
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.COMMENTS))
		}

		if (isAuthenticated)
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.SAVE))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.USER_PROFILE))
		if (isOP)
			addAccessibilityActionFromDescriptionPair(
				ActionDescriptionPair(
					Action.DELETE,
					R.string.action_delete
				)
			)
		if (isAuthenticated)
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.REPORT))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.SHARE))
		if (isAuthenticated) {
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.DOWNVOTE))
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.UPVOTE))
		}
	}


	fun onActionMenuItemSelected(
		post: RedditPreparedPost,
		activity: BaseActivity,
		action: Action
	) {
		when (action) {
			Action.UPVOTE -> action(post, activity, RedditAPI.ACTION_UPVOTE)
			Action.DOWNVOTE -> action(post, activity, RedditAPI.ACTION_DOWNVOTE)
			Action.UNVOTE -> action(post, activity, RedditAPI.ACTION_UNVOTE)
			Action.SAVE -> action(post, activity, RedditAPI.ACTION_SAVE)
			Action.UNSAVE -> action(post, activity, RedditAPI.ACTION_UNSAVE)
			Action.HIDE -> action(post, activity, RedditAPI.ACTION_HIDE)
			Action.UNHIDE -> action(post, activity, RedditAPI.ACTION_UNHIDE)
			Action.EDIT -> {
				val editIntent = Intent(activity, CommentEditActivity::class.java)
				editIntent.putExtra("commentIdAndType", post.src.idAndType)
				editIntent.putExtra(
					"commentText",
					StringEscapeUtils.unescapeHtml4(post.src.rawSelfTextMarkdown)
				)
				editIntent.putExtra("isSelfPost", true)
				activity.startActivity(editIntent)
			}

			Action.DELETE -> MaterialAlertDialogBuilder(activity)
				.setTitle(R.string.accounts_delete)
				.setMessage(R.string.delete_confirm)
				.setPositiveButton(
					R.string.action_delete
				) { _, _ -> action(post, activity, RedditAPI.ACTION_DELETE) }
				.setNegativeButton(R.string.dialog_cancel, null)
				.show()

			Action.REPORT -> MaterialAlertDialogBuilder(activity)
				.setTitle(R.string.action_report)
				.setMessage(R.string.action_report_sure)
				.setPositiveButton(
					R.string.action_report
				) { _, _ -> action(post, activity, RedditAPI.ACTION_REPORT) }
				.setNegativeButton(R.string.dialog_cancel, null)
				.show()

			Action.CROSSPOST_ORIGIN -> {
				val crosspostOriginPost = PostCommentListingURL.forPostId(post.src.isCrosspost)
				LinkHandler.onLinkClicked(activity, crosspostOriginPost.toString())
			}

			Action.EXTERNAL -> {
				try {
					val url = if (activity is WebViewActivity) activity.currentUrl else post.src.url
					if (url == null) {
						General.quickToast(activity, R.string.link_does_not_exist)
						return
					}
					val intent = Intent(Intent.ACTION_VIEW)
					intent.data = LinkHandler.convertAndNormalizeUri(url)
					activity.startActivity(intent)
				} catch (e: ActivityNotFoundException) {
					General.quickToast(
						activity,
						R.string.action_not_handled_by_installed_app_toast
					)
				}
			}

			Action.SELFTEXT_LINKS -> {
				val linksInComment: HashSet<String> = LinkHandler.computeAllLinks(
					StringEscapeUtils.unescapeHtml4(
						post.src
							.rawSelfTextMarkdown
					)
				)
				if (linksInComment.isEmpty()) {
					General.quickToast(activity, R.string.error_toast_no_urls_in_self)
				} else {
					val linksArr = linksInComment.toTypedArray()
					val builder = MaterialAlertDialogBuilder(activity)
					builder.setItems(linksArr) { dialog: DialogInterface, which: Int ->
						LinkHandler.onLinkClicked(
							activity,
							linksArr.get(which),
							false,
							post.src.src
						)
						dialog.dismiss()
					}
					val alert = builder.create()
					alert.setTitle(R.string.action_selftext_links)
					alert.setCanceledOnTouchOutside(true)
					alert.show()
				}
			}

			Action.SAVE_IMAGE -> {
				FileUtils.saveImageAtUri(activity, post.src.url)
			}

			Action.SHARE -> {
				val subject =
					if (PrefsUtility.pref_behaviour_sharing_include_desc()) post.src.title else null
				val body = LinkHandler.getPreferredRedditUriString(post.src.url)

				LinkHandler.shareText(activity, subject, body)
			}

			Action.SHARE_COMMENTS -> {
				val subject = if (PrefsUtility.pref_behaviour_sharing_include_desc()) {
					String.format(activity.getText(R.string.share_comments_for)
							.toString(), post.src.title)
				} else {
					null
				}

				var body = if (PrefsUtility.pref_behaviour_share_permalink()) {
					Constants.Reddit.getNonAPIUri(post.src.permalink).toString()
				} else {
					Constants.Reddit.getNonAPIUri(
							Constants.Reddit.PATH_COMMENTS + post.src.idAlone)
							.toString()
				}

				body = LinkHandler.getPreferredRedditUriString(body)

				LinkHandler.shareText(activity, subject, body)
			}

			Action.SHARE_IMAGE -> {
				FileUtils.shareImageAtUri(activity, post.src.url)
			}

			Action.COPY -> {
				val clipboardManager: ClipboardManager =
					activity.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
				val data = ClipData.newPlainText(
					post.src.author,
					post.src.url
				)
				clipboardManager.setPrimaryClip(data)
				General.quickToast(
					activity.applicationContext,
					R.string.post_link_copied_to_clipboard
				)
			}

			Action.COPY_SELFTEXT -> {
				val clipboardManager: ClipboardManager =
					activity.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
				val data = ClipData.newPlainText(
					post.src.author,
					post.src.rawSelfTextMarkdown
				)
				clipboardManager.setPrimaryClip(data)
				General.quickToast(
					activity.applicationContext,
					R.string.post_text_copied_to_clipboard
				)
			}

			Action.GOTO_SUBREDDIT -> {
				try {
					val intent = Intent(activity, PostListingActivity::class.java)
					intent.data = SubredditPostListURL.getSubreddit(post.src.subreddit)
						.generateJsonUri()
					activity.startActivityForResult(intent, 1)
				} catch (e: InvalidSubredditNameException) {
					Toast.makeText(
						activity,
						R.string.invalid_subreddit_name,
						Toast.LENGTH_LONG
					).show()
				} catch (e: Exception) {
					BugReportActivity.handleGlobalError(
						activity,
						RuntimeException(
							"Got exception for subreddit: " + post.src.subreddit,
							e
						)
					)
				}
			}

			Action.USER_PROFILE -> LinkHandler.onLinkClicked(
				activity,
				UserProfileURL(post.src.author).toString()
			)

			Action.PROPERTIES -> PostPropertiesDialog.newInstance(post.src.src)
				.show(activity.supportFragmentManager, null)

			Action.COMMENTS -> {
				(activity as PostSelectionListener).onPostCommentsSelected(post)
				object : Thread() {
					override fun run() {
						post.markAsRead(activity)
					}
				}.start()
			}

			Action.LINK -> (activity as PostSelectionListener).onPostSelected(post)
			Action.COMMENTS_SWITCH -> {
				if (activity !is MainActivity) {
					activity.finish()
				}
				(activity as PostSelectionListener).onPostCommentsSelected(
					post
				)
			}

			Action.LINK_SWITCH -> {
				if (activity !is MainActivity) {
					activity.finish()
				}
				(activity as PostSelectionListener).onPostSelected(post)
			}

			Action.ACTION_MENU -> showActionMenu(activity, post)
			Action.REPLY -> {
				if (post.isArchived) {
					General.quickToast(activity, R.string.error_archived_reply, Toast.LENGTH_SHORT)
				} else if (post.isLocked && !post.canModerate) {
					General.quickToast(activity, R.string.error_locked_reply, Toast.LENGTH_SHORT)
				} else {
					val intent = Intent(activity, CommentReplyActivity::class.java)
					intent.putExtra(
						CommentReplyActivity.PARENT_ID_AND_TYPE_KEY,
						post.src.idAndType
					)
					intent.putExtra(
						CommentReplyActivity.PARENT_MARKDOWN_KEY,
						post.src.unescapedSelfText
					)
					activity.startActivity(intent)
				}
			}

			Action.BACK -> activity.onBackPressed()
			Action.PIN -> try {
				PrefsUtility.pref_pinned_subreddits_add(
					activity,
					SubredditCanonicalId(post.src.subreddit)
				)
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}

			Action.UNPIN -> try {
				PrefsUtility.pref_pinned_subreddits_remove(
					activity,
					SubredditCanonicalId(post.src.subreddit)
				)
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}

			Action.BLOCK -> try {
				PrefsUtility.pref_blocked_subreddits_add(
					activity,
					SubredditCanonicalId(post.src.subreddit)
				)
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}

			Action.UNBLOCK -> try {
				PrefsUtility.pref_blocked_subreddits_remove(
					activity,
					SubredditCanonicalId(post.src.subreddit)
				)
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}

			Action.SUBSCRIBE -> try {
				val subredditCanonicalId = SubredditCanonicalId(post.src.subreddit)
				val subMan = RedditSubredditSubscriptionManager
					.getSingleton(
						activity,
						RedditAccountManager.getInstance(activity)
							.defaultAccount
					)
				if ((subMan.getSubscriptionState(subredditCanonicalId)
							== SubredditSubscriptionState.NOT_SUBSCRIBED)
				) {
					subMan.subscribe(subredditCanonicalId, activity)
					Toast.makeText(
						activity,
						R.string.options_subscribing,
						Toast.LENGTH_SHORT
					).show()
				} else {
					Toast.makeText(
						activity,
						R.string.mainmenu_toast_subscribed,
						Toast.LENGTH_SHORT
					).show()
				}
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}

			Action.UNSUBSCRIBE -> try {
				val subredditCanonicalId = SubredditCanonicalId(post.src.subreddit)
				val subMan = RedditSubredditSubscriptionManager
					.getSingleton(
						activity,
						RedditAccountManager.getInstance(activity)
							.defaultAccount
					)
				if ((subMan.getSubscriptionState(subredditCanonicalId)
							== SubredditSubscriptionState.SUBSCRIBED)
				) {
					subMan.unsubscribe(subredditCanonicalId, activity)
					Toast.makeText(
						activity,
						R.string.options_unsubscribing,
						Toast.LENGTH_SHORT
					).show()
				} else {
					Toast.makeText(
						activity,
						R.string.mainmenu_toast_not_subscribed,
						Toast.LENGTH_SHORT
					).show()
				}
			} catch (e: InvalidSubredditNameException) {
				throw RuntimeException(e)
			}
		}
	}

	fun showActionMenu(
		activity: BaseActivity,
		post: RedditPreparedPost
	) {
		val itemPref = PrefsUtility.pref_menus_post_context_items()
		if (itemPref.isEmpty()) {
			return
		}
		val user = RedditAccountManager.getInstance(activity).defaultAccount
		val menu = ArrayList<RPVMenuItem>()
		if (!RedditAccountManager.getInstance(activity)
				.defaultAccount
				.isAnonymous
		) {
			if (itemPref.contains(Action.UPVOTE)) {
				if (!post.isUpvoted) {
					menu.add(RPVMenuItem(activity, R.string.action_upvote, Action.UPVOTE))
				} else {
					menu.add(RPVMenuItem(activity, R.string.action_upvote_remove, Action.UNVOTE))
				}
			}
			if (itemPref.contains(Action.DOWNVOTE)) {
				if (!post.isDownvoted) {
					menu.add(RPVMenuItem(activity, R.string.action_downvote, Action.DOWNVOTE))
				} else {
					menu.add(RPVMenuItem(activity, R.string.action_downvote_remove, Action.UNVOTE))
				}
			}
		}
		if (itemPref.contains(Action.COMMENTS)) {
			menu.add(
				RPVMenuItem(
					String.format(
						activity.getText(R.string.action_comments_with_count).toString(),
						post.src.src.num_comments
					),
					Action.COMMENTS
				)
			)
		}
		if (post.src.isCrosspost != null) {
			if (itemPref.contains(Action.CROSSPOST_ORIGIN)) {
				menu.add(
						RPVMenuItem(
								String.format(
										activity.getText(R.string.action_crosspost_origin).toString(),
								),
								Action.CROSSPOST_ORIGIN
						)
				)
			}
		}
		if (!RedditAccountManager.getInstance(activity).defaultAccount.isAnonymous) {
			if (itemPref.contains(Action.SAVE)) {
				if (!post.isSaved) {
					menu.add(RPVMenuItem(activity, R.string.action_save, Action.SAVE))
				} else {
					menu.add(RPVMenuItem(activity, R.string.action_unsave, Action.UNSAVE))
				}
			}
			if (itemPref.contains(Action.HIDE)) {
				if (!post.isHidden) {
					menu.add(RPVMenuItem(activity, R.string.action_hide, Action.HIDE))
				} else {
					menu.add(RPVMenuItem(activity, R.string.action_unhide, Action.UNHIDE))
				}
			}
			if (itemPref.contains(Action.EDIT)
				&& post.isSelf
				&& user.username.equals(post.src.author, ignoreCase = true)
			) {
				menu.add(RPVMenuItem(activity, R.string.action_edit, Action.EDIT))
			}
			if (itemPref.contains(Action.DELETE)
					&& user.username.equals(post.src.author, ignoreCase = true)) {
				menu.add(RPVMenuItem(activity, R.string.action_delete, Action.DELETE))
			}
			if (itemPref.contains(Action.REPORT)) {
				menu.add(RPVMenuItem(activity, R.string.action_report, Action.REPORT))
			}
			if (itemPref.contains(Action.REPLY) && !post.isArchived && !(post.isLocked && !post.canModerate)) {
				menu.add(RPVMenuItem(activity, R.string.action_reply, Action.REPLY))
			}
		}
		if (itemPref.contains(Action.EXTERNAL)) {
			menu.add(RPVMenuItem(activity, R.string.action_external, Action.EXTERNAL))
		}
		if (itemPref.contains(Action.SELFTEXT_LINKS)
				&& post.src.hasSelfText()) {
			menu.add(RPVMenuItem(activity, R.string.action_selftext_links, Action.SELFTEXT_LINKS))
		}
		if (itemPref.contains(Action.SAVE_IMAGE) && post.mIsProbablyAnImage) {
			menu.add(RPVMenuItem(activity, R.string.action_save_image, Action.SAVE_IMAGE))
		}
		if (itemPref.contains(Action.GOTO_SUBREDDIT)) {
			menu.add(RPVMenuItem(activity, R.string.action_gotosubreddit, Action.GOTO_SUBREDDIT))
		}
		if (post.showSubreddit) {
			try {
				val subredditCanonicalId = SubredditCanonicalId(post.src.subreddit)
				if (itemPref.contains(Action.BLOCK)) {
					if (PrefsUtility.pref_blocked_subreddits_check(subredditCanonicalId)) {
						menu.add(
							RPVMenuItem(
								activity,
								R.string.action_unblock_subreddit,
								Action.UNBLOCK
							)
						)
					} else {
						menu.add(
							RPVMenuItem(
								activity,
								R.string.action_block_subreddit,
								Action.BLOCK
							)
						)
					}
				}
				if (itemPref.contains(Action.PIN)) {
					if (PrefsUtility.pref_pinned_subreddits_check(subredditCanonicalId)) {
						menu.add(
							RPVMenuItem(
								activity,
								R.string.action_unpin_subreddit,
								Action.UNPIN
							)
						)
					} else {
						menu.add(
							RPVMenuItem(
								activity,
								R.string.action_pin_subreddit,
								Action.PIN
							)
						)
					}
				}
				if (!RedditAccountManager.getInstance(activity)
						.defaultAccount
						.isAnonymous
				) {
					if (itemPref.contains(Action.SUBSCRIBE)) {
						val subscriptionManager = RedditSubredditSubscriptionManager
							.getSingleton(
								activity,
								RedditAccountManager.getInstance(activity)
									.defaultAccount
							)
						if (subscriptionManager.areSubscriptionsReady()) {
							if (subscriptionManager.getSubscriptionState(
									subredditCanonicalId
								)
								== SubredditSubscriptionState.SUBSCRIBED
							) {
								menu.add(
									RPVMenuItem(
										activity,
										R.string.action_unsubscribe_subreddit,
										Action.UNSUBSCRIBE
									)
								)
							} else {
								menu.add(
									RPVMenuItem(
										activity,
										R.string.action_subscribe_subreddit,
										Action.SUBSCRIBE
									)
								)
							}
						}
					}
				}
			} catch (ex: InvalidSubredditNameException) {
				throw java.lang.RuntimeException(ex)
			}
		}
		val url = post.src.url
		val isRedditVideo = url != null && url.contains("v.redd.it")
		if (itemPref.contains(Action.SHARE)) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_share_link,
					if (isRedditVideo) Action.SHARE_COMMENTS else Action.SHARE
				)
			)
		}
		if (itemPref.contains(Action.SHARE_COMMENTS)) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_share_comments,
					Action.SHARE_COMMENTS
				)
			)
		}
		if (itemPref.contains(Action.SHARE_IMAGE) && post.mIsProbablyAnImage) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_share_image,
					Action.SHARE_IMAGE
				)
			)
		}
		if (itemPref.contains(Action.COPY)) {
			menu.add(RPVMenuItem(activity, R.string.action_copy_link, Action.COPY))
		}
		if (itemPref.contains(Action.COPY_SELFTEXT) && post.src.hasSelfText()) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_copy_selftext,
					Action.COPY_SELFTEXT
				)
			)
		}
		if (itemPref.contains(Action.USER_PROFILE)) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_user_profile,
					Action.USER_PROFILE
				)
			)
		}
		if (itemPref.contains(Action.PROPERTIES)) {
			menu.add(
				RPVMenuItem(
					activity,
					R.string.action_properties,
					Action.PROPERTIES
				)
			)
		}
		val menuText = arrayOfNulls<String>(menu.size)
		for (i in menuText.indices) {
			menuText[i] = menu[i].title
		}
		val builder = MaterialAlertDialogBuilder(activity)
		builder.setItems(menuText) { _: DialogInterface?, which: Int ->
			onActionMenuItemSelected(
				post,
				activity,
				menu[which].action
			)
		}

		val alert = builder.create()
		alert.setCanceledOnTouchOutside(true)
		alert.show()
	}

	fun generateToolbar(
		post: RedditPreparedPost,
		activity: BaseActivity,
		isComments: Boolean,
		overlay: SideToolbarOverlay
	): VerticalToolbar {

		val toolbar = VerticalToolbar(activity)
		
		fun darkIcon(action: Action) = when (action) {
			Action.ACTION_MENU -> R.drawable.dots_vertical_dark
			Action.COMMENTS_SWITCH -> R.drawable.ic_action_comments_dark
			Action.LINK_SWITCH -> if (post.mIsProbablyAnImage) R.drawable.ic_action_image_dark else R.drawable.ic_action_link_dark
			Action.UPVOTE -> R.drawable.arrow_up_bold_dark
			Action.DOWNVOTE -> R.drawable.arrow_down_bold_dark
			Action.SAVE -> R.drawable.star_dark
			Action.HIDE -> R.drawable.ic_action_cross_dark
			Action.REPLY -> R.drawable.ic_action_reply_dark
			Action.EXTERNAL -> R.drawable.ic_action_external_dark
			Action.SAVE_IMAGE -> R.drawable.ic_action_save_dark
			Action.SHARE -> R.drawable.ic_action_share_dark
			Action.COPY -> R.drawable.ic_action_copy_dark
			Action.USER_PROFILE -> R.drawable.ic_action_person_dark
			Action.PROPERTIES -> R.drawable.ic_action_info_dark
			else -> throw RuntimeException("Unknown drawable for $action")
		}

		fun lightIcon(action: Action) = when (action) {
			Action.ACTION_MENU -> R.drawable.dots_vertical_light
			Action.COMMENTS_SWITCH -> R.drawable.ic_action_comments_light
			Action.LINK_SWITCH -> if (post.mIsProbablyAnImage) R.drawable.ic_action_image_light else R.drawable.ic_action_link_light
			Action.UPVOTE -> R.drawable.arrow_up_bold_light
			Action.DOWNVOTE -> R.drawable.arrow_down_bold_light
			Action.SAVE -> R.drawable.star_light
			Action.HIDE -> R.drawable.ic_action_cross_light
			Action.REPLY -> R.drawable.ic_action_reply_light
			Action.EXTERNAL -> R.drawable.ic_action_external_light
			Action.SAVE_IMAGE -> R.drawable.ic_action_save_light
			Action.SHARE -> R.drawable.ic_action_share_light
			Action.COPY -> R.drawable.ic_action_copy_light
			Action.USER_PROFILE -> R.drawable.ic_action_person_light
			Action.PROPERTIES -> R.drawable.ic_action_info_light
			else -> throw RuntimeException("Unknown drawable for $action")
		}

		val itemsToShow = PrefsUtility.pref_menus_post_toolbar_items()
		
		listOf(
			Action.ACTION_MENU,
			Action.COMMENTS_SWITCH,
			Action.LINK_SWITCH,
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
		)
			.filter { itemsToShow.contains(it) }
			.filterNot {
				isComments && it == Action.COMMENTS_SWITCH
						|| !isComments && it == Action.LINK_SWITCH
						|| !post.mIsProbablyAnImage && it == Action.SAVE_IMAGE
			}
			.forEach { action ->

				val ib = LayoutInflater.from(activity)
					.inflate(
						R.layout.flat_image_button,
						toolbar,
						false
					) as ImageButton
				val buttonPadding = General.dpToPixels(activity, 14f)
				ib.setPadding(buttonPadding, buttonPadding, buttonPadding, buttonPadding)
				if (action === Action.UPVOTE && post.isUpvoted
						|| action === Action.DOWNVOTE && post.isDownvoted
						|| action === Action.SAVE && post.isSaved
						|| action === Action.HIDE && post.isHidden
				) {
					ib.setBackgroundColor(Color.WHITE)
					ib.setImageResource(lightIcon(action))
				} else {
					ib.setImageResource(darkIcon(action))
				}
				ib.setOnClickListener {
					val actionToTake: Action = when (action) {
						Action.UPVOTE -> if (post.isUpvoted) Action.UNVOTE else Action.UPVOTE
						Action.DOWNVOTE -> if (post.isDownvoted) Action.UNVOTE else Action.DOWNVOTE
						Action.SAVE -> if (post.isSaved) Action.UNSAVE else Action.SAVE
						Action.HIDE -> if (post.isHidden) Action.UNHIDE else Action.HIDE
						else -> action
					}
					onActionMenuItemSelected(post, activity, actionToTake)
					overlay.hide()
				}
				var accessibilityAction = action
				if (accessibilityAction === Action.UPVOTE && post.isUpvoted
					|| accessibilityAction === Action.DOWNVOTE && post.isDownvoted
				) {
					accessibilityAction = Action.UNVOTE
				}
				if (accessibilityAction === Action.SAVE && post.isSaved) {
					accessibilityAction = Action.UNSAVE
				}
				if (accessibilityAction === Action.HIDE && post.isHidden) {
					accessibilityAction = Action.UNHIDE
				}
				val text = activity.getString(accessibilityAction.descriptionResId)
				ib.contentDescription = text
				TooltipCompat.setTooltipText(ib, text)
				toolbar.addItem(ib)
		}

		return toolbar
	}

	fun action(
		post: RedditPreparedPost,
		activity: BaseActivity,
		@RedditAction action: Int
	) {
		val user = RedditAccountManager.getInstance(activity).defaultAccount
		if (user.isAnonymous) {
			AndroidCommon.UI_THREAD_HANDLER.post {
				General.showMustBeLoggedInDialog(
					activity
				)
			}
			return
		}
		val changeDataManager = RedditChangeDataManager.getInstance(user)

		val lastVoteDirection: Int = post.voteDirection
		val archived: Boolean = post.src.isArchived
		val now = TimestampUTC.now()
		when (action) {
			RedditAPI.ACTION_DOWNVOTE -> if (!archived) {
				changeDataManager.markDownvoted(now, post.src.idAndType)
			}

			RedditAPI.ACTION_UNVOTE -> if (!archived) {
				changeDataManager.markUnvoted(now, post.src.idAndType)
			}

			RedditAPI.ACTION_UPVOTE -> if (!archived) {
				changeDataManager.markUpvoted(now, post.src.idAndType)
			}

			RedditAPI.ACTION_SAVE -> changeDataManager.markSaved(now, post.src.idAndType, true)
			RedditAPI.ACTION_UNSAVE -> changeDataManager.markSaved(now, post.src.idAndType, false)
			RedditAPI.ACTION_HIDE -> changeDataManager.markHidden(now, post.src.idAndType, true)
			RedditAPI.ACTION_UNHIDE -> changeDataManager.markHidden(now, post.src.idAndType, false)
			RedditAPI.ACTION_REPORT -> {}
			RedditAPI.ACTION_DELETE -> {}
			else -> throw java.lang.RuntimeException("Unknown post action $action")
		}
		val vote = (action == RedditAPI.ACTION_DOWNVOTE || action == RedditAPI.ACTION_UPVOTE || action == RedditAPI.ACTION_UNVOTE)
		if (archived && vote) {
			Toast.makeText(activity, R.string.error_archived_vote, Toast.LENGTH_SHORT).show()
			return
		}
		RedditAPI.action(CacheManager.getInstance(activity),
			object : ActionResponseHandler(activity) {
				override fun onCallbackException(t: Throwable) {
					BugReportActivity.handleGlobalError(context, t)
				}

				override fun onFailure(
					error: RRError
				) {
					revertOnFailure()
					General.showResultDialog(activity, error)
				}

				override fun onSuccess() {
					@Suppress("NAME_SHADOWING") val now = TimestampUTC.now()
					when (action) {
						RedditAPI.ACTION_DOWNVOTE -> changeDataManager.markDownvoted(now, post.src.idAndType)
						RedditAPI.ACTION_UNVOTE -> changeDataManager.markUnvoted(now, post.src.idAndType)
						RedditAPI.ACTION_UPVOTE -> changeDataManager.markUpvoted(now, post.src.idAndType)
						RedditAPI.ACTION_SAVE -> changeDataManager.markSaved(now, post.src.idAndType, true)
						RedditAPI.ACTION_UNSAVE -> changeDataManager.markSaved(now, post.src.idAndType, false)
						RedditAPI.ACTION_HIDE -> changeDataManager.markHidden(now, post.src.idAndType, true)
						RedditAPI.ACTION_UNHIDE -> changeDataManager.markHidden(now, post.src.idAndType, false)
						RedditAPI.ACTION_REPORT -> {}
						RedditAPI.ACTION_DELETE -> General.quickToast(
							activity,
							R.string.delete_success
						)

						else -> throw java.lang.RuntimeException("Unknown post action")
					}
				}

				private fun revertOnFailure() {
					@Suppress("NAME_SHADOWING") val now = TimestampUTC.now()
					when (action) {
						RedditAPI.ACTION_DOWNVOTE, RedditAPI.ACTION_UNVOTE, RedditAPI.ACTION_UPVOTE -> {
							when (lastVoteDirection) {
								-1 -> changeDataManager.markDownvoted(now, post.src.idAndType)
								0 -> changeDataManager.markUnvoted(now, post.src.idAndType)
								1 -> changeDataManager.markUpvoted(now, post.src.idAndType)
							}
							changeDataManager.markSaved(now, post.src.idAndType, false)
						}

						RedditAPI.ACTION_SAVE -> changeDataManager.markSaved(now, post.src.idAndType, false)
						RedditAPI.ACTION_UNSAVE -> changeDataManager.markSaved(now, post.src.idAndType, true)
						RedditAPI.ACTION_HIDE -> changeDataManager.markHidden(now, post.src.idAndType, false)
						RedditAPI.ACTION_UNHIDE -> changeDataManager.markHidden(now, post.src.idAndType, true)
						RedditAPI.ACTION_REPORT -> {}
						RedditAPI.ACTION_DELETE -> {}
						else -> throw java.lang.RuntimeException("Unknown post action $action")
					}
				}
			}, user, post.src.idAndType, action, activity
		)
	}
}
