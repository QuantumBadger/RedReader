package org.quantumbadger.redreader.reddit.api

import androidx.annotation.StringRes
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.common.PrefsUtility.PostFlingAction
import org.quantumbadger.redreader.reddit.api.RedditPostActions.ActionDescriptionPair.Companion.from
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost
import org.quantumbadger.redreader.views.AccessibilityActionManager

object RedditPostActions {

	data class ActionDescriptionPair(
		val action: RedditPreparedPost.Action,
		@StringRes val descriptionRes: Int
	) {
		companion object {
			@JvmStatic
			fun from(
				post: RedditPreparedPost,
				pref: PostFlingAction
			): ActionDescriptionPair? {
				return when (pref) {
					PostFlingAction.UPVOTE -> if (post.isUpvoted()) {
						ActionDescriptionPair(
							RedditPreparedPost.Action.UNVOTE,
							R.string.action_vote_remove
						)
					} else {
						ActionDescriptionPair(
							RedditPreparedPost.Action.UPVOTE,
							R.string.action_upvote
						)
					}

					PostFlingAction.DOWNVOTE -> if (post.isDownvoted()) {
						ActionDescriptionPair(
							RedditPreparedPost.Action.UNVOTE,
							R.string.action_vote_remove
						)
					} else {
						ActionDescriptionPair(
							RedditPreparedPost.Action.DOWNVOTE,
							R.string.action_downvote
						)
					}

					PostFlingAction.SAVE -> if (post.isSaved()) {
						ActionDescriptionPair(
							RedditPreparedPost.Action.UNSAVE,
							R.string.action_unsave
						)
					} else {
						ActionDescriptionPair(
							RedditPreparedPost.Action.SAVE,
							R.string.action_save
						)
					}

					PostFlingAction.HIDE -> if (post.isHidden()) {
						ActionDescriptionPair(
							RedditPreparedPost.Action.UNHIDE,
							R.string.action_unhide
						)
					} else {
						ActionDescriptionPair(
							RedditPreparedPost.Action.HIDE,
							R.string.action_hide
						)
					}

					PostFlingAction.COMMENTS -> ActionDescriptionPair(
						RedditPreparedPost.Action.COMMENTS,
						R.string.action_comments_short
					)

					PostFlingAction.LINK -> ActionDescriptionPair(
						RedditPreparedPost.Action.LINK,
						R.string.action_link_short
					)

					PostFlingAction.BROWSER -> ActionDescriptionPair(
						RedditPreparedPost.Action.EXTERNAL,
						R.string.action_external_short
					)

					PostFlingAction.REPORT -> ActionDescriptionPair(
						RedditPreparedPost.Action.REPORT,
						R.string.action_report
					)

					PostFlingAction.SAVE_IMAGE -> ActionDescriptionPair(
						RedditPreparedPost.Action.SAVE_IMAGE,
						R.string.action_save_image
					)

					PostFlingAction.GOTO_SUBREDDIT -> ActionDescriptionPair(
						RedditPreparedPost.Action.GOTO_SUBREDDIT,
						R.string.action_gotosubreddit
					)

					PostFlingAction.SHARE -> ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE,
						R.string.action_share
					)

					PostFlingAction.SHARE_COMMENTS -> ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE_COMMENTS,
						R.string.action_share_comments
					)

					PostFlingAction.SHARE_IMAGE -> ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE_IMAGE,
						R.string.action_share_image
					)

					PostFlingAction.COPY -> ActionDescriptionPair(
						RedditPreparedPost.Action.COPY,
						R.string.action_copy_link
					)

					PostFlingAction.USER_PROFILE -> ActionDescriptionPair(
						RedditPreparedPost.Action.USER_PROFILE,
						R.string.action_user_profile_short
					)

					PostFlingAction.PROPERTIES -> ActionDescriptionPair(
						RedditPreparedPost.Action.PROPERTIES,
						R.string.action_properties
					)

					PostFlingAction.ACTION_MENU -> ActionDescriptionPair(
						RedditPreparedPost.Action.ACTION_MENU,
						R.string.action_actionmenu_short
					)

					PostFlingAction.BACK -> ActionDescriptionPair(
						RedditPreparedPost.Action.BACK,
						R.string.action_back
					)

					PostFlingAction.DISABLED -> null
				}
			}
		}
	}

	fun setupAccessibilityActions(
		accessibilityActionManager: AccessibilityActionManager,
		post: RedditPreparedPost,
		activity: BaseActivity,
		showCommentsOption: Boolean
	) {
		fun addAccessibilityActionFromDescriptionPair(
			pair: ActionDescriptionPair?
		) {
			if (pair == null) {
				return
			}
			accessibilityActionManager.addAction(pair.descriptionRes) {
				RedditPreparedPost.onActionMenuItemSelected(
					post,
					activity,
					pair.action
				)
			}
		}

		accessibilityActionManager.removeAllActions()

		if (showCommentsOption) {
			addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.COMMENTS))
		}

		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.SAVE))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.USER_PROFILE))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.REPORT))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.SHARE))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.DOWNVOTE))
		addAccessibilityActionFromDescriptionPair(from(post, PostFlingAction.UPVOTE))
	}
}
