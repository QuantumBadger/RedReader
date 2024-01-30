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

package org.quantumbadger.redreader.fragments

import android.content.Intent
import android.graphics.BitmapFactory
import android.util.Log
import android.view.View
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.ScrollView
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.widget.AppCompatImageView
import com.google.android.material.card.MaterialCardView
import com.google.android.material.chip.Chip
import com.google.android.material.dialog.MaterialAlertDialogBuilder
import com.google.android.material.textview.MaterialTextView
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.BugReportActivity
import org.quantumbadger.redreader.activities.PMSendActivity
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached
import org.quantumbadger.redreader.common.*
import org.quantumbadger.redreader.common.AndroidCommon.runOnUiThread
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.General.uriFromString
import org.quantumbadger.redreader.common.Optional
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.common.time.TimeFormatHelper.format
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.common.time.TimestampUTC.Companion.fromUtcSecs
import org.quantumbadger.redreader.common.time.TimestampUTC.Companion.now
import org.quantumbadger.redreader.reddit.APIResponseHandler.UserResponseHandler
import org.quantumbadger.redreader.reddit.RedditAPI
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException
import org.quantumbadger.redreader.reddit.things.RedditUser
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId
import org.quantumbadger.redreader.reddit.url.UserPostListingURL
import org.quantumbadger.redreader.views.LoadingSpinnerView
import org.quantumbadger.redreader.views.liststatus.ErrorView
import java.io.IOException
import java.net.URISyntaxException
import java.text.NumberFormat
import java.util.*

object UserProfileDialog {
	@JvmStatic
	fun show(
		activity: AppCompatActivity,
		username: String
	) {
		val builder = MaterialAlertDialogBuilder(activity)
		builder.setView(R.layout.user_profile_dialog)

		val dialog = builder.show()

		val loadingView = dialog.findViewById<LoadingSpinnerView>(R.id.user_profile_loading)!!
		val scrollView = dialog.findViewById<ScrollView>(R.id.user_profile_scrollview)!!
		val textviewUsername = dialog.findViewById<MaterialTextView>(R.id.user_profile_name)!!
		val textviewAccountAge = dialog.findViewById<MaterialTextView>(R.id.user_profile_account_age)!!
		val chipYou = dialog.findViewById<Chip>(R.id.user_profile_chip_you)!!
		val chipSuspended = dialog.findViewById<Chip>(R.id.user_profile_chip_suspended)!!
		val chipFriend = dialog.findViewById<Chip>(R.id.user_profile_chip_friend)!!
		val chipAdmin = dialog.findViewById<Chip>(R.id.user_profile_chip_admin)!!
		val chipMod = dialog.findViewById<Chip>(R.id.user_profile_chip_moderator)!!
		val chipGold = dialog.findViewById<Chip>(R.id.user_profile_chip_gold)!!
		val postsCard = dialog.findViewById<MaterialCardView>(R.id.user_profile_posts)!!
		val commentsCard = dialog.findViewById<MaterialCardView>(R.id.user_profile_comments)!!
		val messageCard = dialog.findViewById<MaterialCardView>(R.id.user_profile_send_message)!!
		val postsKarma = dialog.findViewById<MaterialTextView>(R.id.user_profile_posts_karma)!!
		val commentsKarma = dialog.findViewById<MaterialTextView>(R.id.user_profile_comments_karma)!!
		val chipMoreInfo = dialog.findViewById<Chip>(R.id.user_profile_chip_more_info)!!
		val chipFollow = dialog.findViewById<Chip>(R.id.user_profile_chip_follow)!!
		val chipFollowed = dialog.findViewById<Chip>(R.id.user_profile_chip_followed)!!
		val chipUnfollow = dialog.findViewById<Chip>(R.id.user_profile_chip_unfollow)!!

		val cm = CacheManager.getInstance(activity)
		val accountManager = RedditAccountManager.getInstance(activity)

		RedditAPI.getUser(
			cm,
			username,
			object : UserResponseHandler(activity) {

				override fun onDownloadStarted() {}

				override fun onSuccess(user: RedditUser, timestamp: TimestampUTC) {
					AndroidCommon.UI_THREAD_HANDLER.post {
						if (!dialog.isShowing) {
							return@post
						}
						loadingView.visibility = View.GONE
						scrollView.visibility = View.VISIBLE
						textviewUsername.text = user.name

						val createdUtc = user.created_utc

						if (createdUtc == null) {
							textviewAccountAge.visibility = View.GONE

						} else {
							textviewAccountAge.text = format(
								now().elapsedPeriodSince(fromUtcSecs(createdUtc)),
								activity,
								R.string.user_profile_account_age,
								1
							)
						}

						if (StringUtils.asciiLowercase(user.name) != StringUtils.asciiLowercase(
							accountManager.getDefaultAccount().canonicalUsername
						)) {
							chipYou.visibility = View.GONE
						}

						if (user.is_suspended != true) {
							chipSuspended.visibility = View.GONE
						}

						if (user.is_friend != true) {
							chipFriend.visibility = View.GONE
						}

						if (user.is_employee != true) {
							chipAdmin.visibility = View.GONE
						}

						if (user.is_mod != true) {
							chipMod.visibility = View.GONE
						}

						if (user.is_gold != true) {
							chipGold.visibility = View.GONE
						}

						val userSubredditCanonicalIdA = SubredditCanonicalId("/user/$username")
						val userSubredditCanonicalIdB = SubredditCanonicalId("u_$username")
						val subMan = getSubMan(activity)
						if (subMan.getSubscriptionState(userSubredditCanonicalIdA) == SubredditSubscriptionState.NOT_SUBSCRIBED &&
								subMan.getSubscriptionState(userSubredditCanonicalIdB) == SubredditSubscriptionState.NOT_SUBSCRIBED) {
							chipFollowed.visibility = View.GONE
							chipFollow.visibility = View.VISIBLE
							chipUnfollow.visibility = View.GONE
						} else {
							chipFollow.visibility = View.GONE
							chipUnfollow.visibility = View.VISIBLE
						}

						if (PrefsUtility.appearance_user_show_avatars()) {
							val iconUrl = user.iconUrl
							if (!iconUrl.isNullOrEmpty()) {

								val avatarView =
									dialog.findViewById<AppCompatImageView>(R.id.avatar_image)!!
								val avatarViewHolder =
									dialog.findViewById<View>(R.id.avatar_image_holder)!!

								avatarViewHolder.visibility = View.VISIBLE

								try {
									assignUserAvatar(iconUrl, avatarView, context)
								} catch (e: URISyntaxException) {
									Log.e("UserProfileDialog", "Error decoding uri", e)
								}
							}
						}

						val linkKarma = user.link_karma ?: 0
						val commentKarma = user.comment_karma ?: 0

						postsKarma.text =
							NumberFormat.getNumberInstance().format(linkKarma.toLong())
						commentsKarma.text =
							NumberFormat.getNumberInstance().format(commentKarma.toLong())
						postsKarma.setContentDescription(
							activity.getString(
								R.string.userprofile_accessibility_karma,
								user.link_karma
							)
						)
						commentsKarma.setContentDescription(
							activity.getString(
								R.string.userprofile_accessibility_karma,
								user.comment_karma
							)
						)
						postsCard.setOnClickListener {
							LinkHandler.onLinkClicked(
								context,
								UserPostListingURL.getSubmitted(username)
									.generateJsonUri()
									.toString(),
								false
							)
						}
						commentsCard.setOnClickListener {
							LinkHandler.onLinkClicked(
								context,
								Constants.Reddit.getUri(
									"/user/$username/comments.json"
								).toString(),
								false
							)
						}
						if (!RedditAccountManager.getInstance(context)
								.getDefaultAccount()
								.isAnonymous
						) {
							messageCard.setOnClickListener {
								Intent(context, PMSendActivity::class.java).apply {
									putExtra(PMSendActivity.EXTRA_RECIPIENT, username)
									activity.startActivity(this)
								}
							}
						} else {
							messageCard.visibility = View.GONE
						}
						chipMoreInfo.setOnClickListener {
							UserPropertiesDialog.newInstance(user)
								.show(activity.supportFragmentManager, null)
						}
						chipFollow.setOnClickListener {
							subscribeToUser(activity, username)
						}
						chipUnfollow.setOnClickListener {
							unsubscribeToUser(activity, username)
						}
					}
				}

				override fun onCallbackException(t: Throwable) {
					BugReportActivity.handleGlobalError(context, t)
				}

				override fun onFailure(error: RRError) {
					AndroidCommon.UI_THREAD_HANDLER.post {
						if (!dialog.isShowing) {
							return@post
						}
						val root = dialog.findViewById<FrameLayout>(R.id.user_profile_root)!!
						root.removeAllViews()
						root.addView(ErrorView(context, error))
					}
				}
			},
			accountManager.getDefaultAccount(),
			DownloadStrategyAlways.INSTANCE,
			activity
		)
	}

	private fun subscribeToUser(activity: AppCompatActivity, username: String) {
		try {
			val usernameToSubreddit = "u_$username"
			val userSubredditCanonicalId = SubredditCanonicalId(usernameToSubreddit)

			val subMan = getSubMan(activity)
			if ((subMan.getSubscriptionState(userSubredditCanonicalId)
							== SubredditSubscriptionState.NOT_SUBSCRIBED)
			) {
				subMan.subscribe(userSubredditCanonicalId, activity)
				Toast.makeText(
						activity,
						R.string.userprofile_toast_follow_loading,
						Toast.LENGTH_SHORT
				).show()
			} else {
				Toast.makeText(
						activity,
						R.string.userprofile_toast_followed,
						Toast.LENGTH_SHORT
				).show()
			}
		} catch (e: InvalidSubredditNameException) {
			throw RuntimeException(e)
		}
	}

	private fun unsubscribeToUser(activity: AppCompatActivity, username: String) {
		try {
			val userSubredditCanonicalIdA = SubredditCanonicalId("/user/$username")
			val userSubredditCanonicalIdB = SubredditCanonicalId("u_$username")

			val subMan = getSubMan(activity)

			fun unsubscribeIfSubscribed(canonicalId: SubredditCanonicalId): Boolean {
				return if (subMan.getSubscriptionState(canonicalId) == SubredditSubscriptionState.SUBSCRIBED) {
					subMan.unsubscribe(canonicalId, activity)
					Toast.makeText(activity, R.string.userprofile_toast_unfollow_loading, Toast.LENGTH_SHORT).show()
					true
				} else {
					false
				}
			}

			if (!unsubscribeIfSubscribed(userSubredditCanonicalIdA) && !unsubscribeIfSubscribed(userSubredditCanonicalIdB)) {
				Toast.makeText(activity, R.string.userprofile_toast_not_following, Toast.LENGTH_SHORT).show()
			}
		} catch (e: InvalidSubredditNameException) {
			throw RuntimeException(e)
		}
	}

	private fun getSubMan(activity: AppCompatActivity): RedditSubredditSubscriptionManager {
		val subMan = RedditSubredditSubscriptionManager.getSingleton(
				activity,
				RedditAccountManager.getInstance(activity).defaultAccount
		)
		return subMan
	}

	@Throws(URISyntaxException::class)
	private fun assignUserAvatar(
		url: String,
		imageOutput: ImageView,
		context: AppCompatActivity
	) {
		CacheManager.getInstance(context).makeRequest(CacheRequest(
			uriFromString(url),
			RedditAccountManager.getAnon(),
			null,
			Priority(Constants.Priority.INLINE_IMAGE_PREVIEW),
			DownloadStrategyIfNotCached.INSTANCE,
			Constants.FileType.INLINE_IMAGE_PREVIEW,
			CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
			context,
			object : CacheRequestCallbacks {
				override fun onDataStreamComplete(
					streamFactory: GenericFactory<SeekableInputStream, IOException>,
					timestamp: TimestampUTC,
					session: UUID,
					fromCache: Boolean,
					mimetype: String?
				) {
					try {
						streamFactory.create().use { `is` ->
							val data = BitmapFactory.decodeStream(`is`)
								?: throw IOException("Failed to decode bitmap")
							runOnUiThread {
								imageOutput.setImageBitmap(data)
								imageOutput.setOnClickListener {
									LinkHandler.onLinkClicked(
										context,
										url
									)
								}
							}
						}
					} catch (t: Throwable) {
						onFailure(
							getGeneralErrorForFailure(
								context,
								CacheRequest.REQUEST_FAILURE_CONNECTION,
								t,
								null,
								url,
								Optional.empty()
							)
						)
					}
				}

				override fun onFailure(error: RRError) {
					Log.d(
						"UserProfileDialog",
						"Failed to download user avatar: $error"
					)
				}
			}
		))
	}
}
