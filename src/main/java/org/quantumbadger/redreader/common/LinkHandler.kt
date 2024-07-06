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
package org.quantumbadger.redreader.common

import android.content.ActivityNotFoundException
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.DialogInterface
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.os.Parcelable
import android.util.Log
import android.util.TypedValue
import androidx.appcompat.app.AppCompatActivity
import com.google.android.material.dialog.MaterialAlertDialogBuilder
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.activities.AlbumListingActivity
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.activities.CommentListingActivity
import org.quantumbadger.redreader.activities.ImageViewActivity
import org.quantumbadger.redreader.activities.PMSendActivity
import org.quantumbadger.redreader.activities.PostListingActivity
import org.quantumbadger.redreader.activities.WebViewActivity
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.General.isSensitiveDebugLoggingEnabled
import org.quantumbadger.redreader.common.General.quickToast
import org.quantumbadger.redreader.common.PrefsUtility.AlbumViewMode
import org.quantumbadger.redreader.common.PrefsUtility.SharingDomain
import org.quantumbadger.redreader.fragments.ShareOrderDialog
import org.quantumbadger.redreader.fragments.UserProfileDialog.show
import org.quantumbadger.redreader.http.HTTPBackend
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.image.DeviantArtAPI
import org.quantumbadger.redreader.image.GetAlbumInfoListener
import org.quantumbadger.redreader.image.GetImageInfoListener
import org.quantumbadger.redreader.image.GfycatAPI
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.image.ImageInfo.HasAudio
import org.quantumbadger.redreader.image.ImageUrlInfo
import org.quantumbadger.redreader.image.ImgurAPI
import org.quantumbadger.redreader.image.ImgurAPIV3
import org.quantumbadger.redreader.image.RedditGalleryAPI.Companion.getAlbumInfo
import org.quantumbadger.redreader.image.RedditVideosAPI
import org.quantumbadger.redreader.image.RedgifsAPI
import org.quantumbadger.redreader.image.RedgifsAPIV2
import org.quantumbadger.redreader.image.StreamableAPI
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import org.quantumbadger.redreader.reddit.url.OpaqueSharedURL
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL
import org.quantumbadger.redreader.reddit.url.RedditURLParser
import java.util.Locale
import java.util.regex.Pattern
import kotlin.concurrent.thread

object LinkHandler {
	private val youtubeDotComPattern = Pattern.compile("^https?://[.\\w]*youtube\\.\\w+/.*")

	private val youtuDotBePattern =
		Pattern.compile("^https?://[.\\w]*youtu\\.be/([A-Za-z0-9\\-_]+)(\\?.*|).*")

	private val vimeoPattern = Pattern.compile("^https?://[.\\w]*vimeo\\.\\w+/.*")

	private val googlePlayPattern = Pattern.compile("^https?://[.\\w]*play\\.google\\.\\w+/.*")

	@JvmStatic
	@JvmOverloads
	fun onLinkClicked(
		activity: AppCompatActivity,
		url: UriString?,
		forceNoImage: Boolean = false,
		post: RedditPost? = null,
		albumInfo: AlbumInfo? = null,
		albumImageIndex: Int? = null,
		fromExternalIntent: Boolean = false
	) {
		if (url == null) {
			quickToast(activity, R.string.link_does_not_exist)
			return
		}

		if (url.value.startsWith("rr://")) {
			val rrUri = Uri.parse(url.value)

			if (rrUri.authority == "msg") {
				AndroidCommon.runOnUiThread {
					MaterialAlertDialogBuilder(activity).apply {
						setTitle(rrUri.getQueryParameter("title"))
						setMessage(rrUri.getQueryParameter("message"))
					}.create().show()
				}

				return
			}
		}

		val normalUrl = convertAndNormalizeUri(url)
		val normalUrlString = UriString(normalUrl.toString())

		if (!forceNoImage && isProbablyAnImage(normalUrlString)) {
			val intent = Intent(activity, ImageViewActivity::class.java)
			intent.setData(normalUrl)
			intent.putExtra("post", post)

			if (albumInfo != null && albumImageIndex != null) {
				intent.putExtra("albumUrl", albumInfo.url)
				intent.putExtra("albumImageIndex", albumImageIndex)
			}

			activity.startActivity(intent)
			return
		}

		if (!forceNoImage && (imgurAlbumPattern.matcher(normalUrlString.value).matches()
					|| redditGalleryPattern.matcher(normalUrlString.value).matches())
		) {
			val albumViewMode = PrefsUtility.pref_behaviour_albumview_mode()

			when (albumViewMode) {
				AlbumViewMode.INTERNAL_LIST -> {
					val intent = Intent(
						activity,
						AlbumListingActivity::class.java
					)
					intent.setData(normalUrl)
					intent.putExtra("post", post)
					activity.startActivity(intent)
					return
				}

				AlbumViewMode.INTERNAL_BROWSER -> {
					if (PrefsUtility.pref_behaviour_usecustomtabs()) {
						openCustomTab(activity, normalUrl, post)
					} else {
						openInternalBrowser(activity, normalUrlString, post)
					}
					return
				}

				AlbumViewMode.EXTERNAL_BROWSER -> {
					openWebBrowser(activity, normalUrl, fromExternalIntent)
					return
				}
			}
		}

		val redditURL = RedditURLParser.parse(normalUrl)
		if (redditURL != null) {
			when (redditURL.pathType()) {
				RedditURLParser.OPAQUE_SHARED_URL -> {

					// kick off a thread to get the real url
					thread {
						val toFetchUrl = (redditURL as OpaqueSharedURL).urlToFetch
						val result = HTTPBackend.backend
							.resolveRedirectUri(activity.applicationContext, toFetchUrl)

						when (result) {
							is Result.Err -> {
								General.showResultDialog(activity, result.error)
							}
							is Result.Ok -> {
								activity.runOnUiThread {
									onLinkClicked(
										activity,
										result.value,
										forceNoImage,
										post,
										albumInfo,
										albumImageIndex,
										fromExternalIntent
									)
								}
							}
						}
					}
					return
				}

				RedditURLParser.SUBREDDIT_POST_LISTING_URL, RedditURLParser.MULTIREDDIT_POST_LISTING_URL, RedditURLParser.USER_POST_LISTING_URL, RedditURLParser.SEARCH_POST_LISTING_URL, RedditURLParser.UNKNOWN_POST_LISTING_URL -> {
					val intent = Intent(activity, PostListingActivity::class.java)
					intent.setData(redditURL.generateJsonUri())
					activity.startActivityForResult(intent, 1)
					return
				}

				RedditURLParser.POST_COMMENT_LISTING_URL, RedditURLParser.USER_COMMENT_LISTING_URL, RedditURLParser.UNKNOWN_COMMENT_LISTING_URL -> {
					val intent = Intent(
						activity,
						CommentListingActivity::class.java
					)
					intent.setData(redditURL.generateJsonUri())
					activity.startActivityForResult(intent, 1)
					return
				}

				RedditURLParser.COMPOSE_MESSAGE_URL -> {
					val intent = Intent(
						activity,
						PMSendActivity::class.java
					)
					val cmUrl = redditURL.asComposeMessageURL()

					if (cmUrl.recipient != null) {
						intent.putExtra(PMSendActivity.EXTRA_RECIPIENT, cmUrl.recipient)
					}
					if (cmUrl.subject != null) {
						intent.putExtra(PMSendActivity.EXTRA_SUBJECT, cmUrl.subject)
					}
					if (cmUrl.message != null) {
						intent.putExtra(PMSendActivity.EXTRA_TEXT, cmUrl.message)
					}

					activity.startActivityForResult(intent, 1)
					return
				}

				RedditURLParser.USER_PROFILE_URL -> {
					show(activity, redditURL.asUserProfileURL().username)
					return
				}
			}
		}

		// Use a browser
		if (!PrefsUtility.pref_behaviour_useinternalbrowser()) {
			if (openWebBrowser(activity, normalUrl, fromExternalIntent)) {
				return
			}
		}

		if (youtubeDotComPattern.matcher(normalUrlString.value).matches()
			|| vimeoPattern.matcher(normalUrlString.value).matches()
			|| googlePlayPattern.matcher(normalUrlString.value).matches()
			|| normalUrlString.value.startsWith("mailto:")
		) {
			if (openWebBrowser(activity, normalUrl, fromExternalIntent)) {
				return
			}
		}

		val youtuDotBeMatcher = youtuDotBePattern.matcher(normalUrlString.value)

		if (youtuDotBeMatcher.find() && youtuDotBeMatcher.group(1) != null) {
			val youtuBeUrl = ("http://youtube.com/watch?v="
					+ youtuDotBeMatcher.group(1)
					+ (youtuDotBeMatcher.group(2)?.takeUnless { it.isEmpty() }
				?.let { "&${it.substring(1)}" } ?: ""))
			if (openWebBrowser(activity, Uri.parse(youtuBeUrl), fromExternalIntent)) {
				return
			}
		}

		if (PrefsUtility.pref_behaviour_usecustomtabs()) {
			openCustomTab(activity, normalUrl, post)
		} else {
			openInternalBrowser(activity, normalUrlString, post)
		}
	}

	@JvmOverloads
	@JvmStatic
	fun onLinkLongClicked(
		activity: BaseActivity,
		uri: UriString?,
		forceNoImage: Boolean = false
	) {
		if (uri == null) {
			return
		}

		val normalUriString = UriString(convertAndNormalizeUri(uri).toString())

		val itemPref = PrefsUtility.pref_menus_link_context_items()

		if (itemPref.isEmpty()) {
			return
		}

		val menu = ArrayList<LinkMenuItem>()

		if (itemPref.contains(LinkAction.COPY_URL)) {
			menu.add(
				LinkMenuItem(
					activity,
					R.string.action_copy_link,
					LinkAction.COPY_URL
				)
			)
		}
		if (itemPref.contains(LinkAction.EXTERNAL)) {
			menu.add(
				LinkMenuItem(
					activity,
					R.string.action_external,
					LinkAction.EXTERNAL
				)
			)
		}
		if (itemPref.contains(LinkAction.SAVE_IMAGE)
			&& isProbablyAnImage(normalUriString)
			&& !forceNoImage
		) {
			menu.add(
				LinkMenuItem(
					activity,
					R.string.action_save_image,
					LinkAction.SAVE_IMAGE
				)
			)
		}
		if (itemPref.contains(LinkAction.SHARE)) {
			menu.add(LinkMenuItem(activity, R.string.action_share, LinkAction.SHARE))
		}
		if (itemPref.contains(LinkAction.SHARE_IMAGE)
			&& isProbablyAnImage(normalUriString)
			&& !forceNoImage
		) {
			menu.add(
				LinkMenuItem(
					activity,
					R.string.action_share_image,
					LinkAction.SHARE_IMAGE
				)
			)
		}
		val menuText = menu.map { it.title }.toTypedArray()

		val builder = MaterialAlertDialogBuilder(activity)

		builder.setItems(menuText) { _: DialogInterface?, which: Int ->
			onActionMenuItemSelected(
				normalUriString,
				activity,
				menu[which].action
			)
		}

		//builder.setNeutralButton(R.string.dialog_cancel, null);
		val alert = builder.create()
		alert.setCanceledOnTouchOutside(true)
		alert.show()
	}

	fun onActionMenuItemSelected(
        uri: UriString,
        activity: BaseActivity,
        action: LinkAction
	) {
		when (action) {
			LinkAction.SHARE -> shareText(activity, null, getPreferredRedditUriString(uri).value)
			LinkAction.COPY_URL -> {
				val clipboardManager =
					activity.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager?
				if (clipboardManager != null) {
					// Using newPlainText here instead of newRawUri because links from
					// comments/self-text are often not valid URIs
					val data = ClipData.newPlainText(null, uri.value)
					clipboardManager.setPrimaryClip(data)

					quickToast(
						activity.applicationContext,
						R.string.link_copied_to_clipboard
					)
				}
			}

			LinkAction.EXTERNAL -> try {
				val intent = Intent(Intent.ACTION_VIEW)
				intent.setData(Uri.parse(uri.value))
				activity.startActivity(intent)
			} catch (e: ActivityNotFoundException) {
				quickToast(
					activity,
					R.string.error_no_suitable_apps_available
				)
			}

			LinkAction.SHARE_IMAGE -> FileUtils.shareImageAtUri(activity, uri)
			LinkAction.SAVE_IMAGE -> FileUtils.saveImageAtUri(activity, uri)
		}
	}

	@JvmStatic
	fun openWebBrowser(
		activity: AppCompatActivity,
		uri: Uri,
		fromExternalIntent: Boolean
	): Boolean {
		if (!fromExternalIntent) {
			try {
				val intent = Intent(Intent.ACTION_VIEW)
				intent.setData(uri)
				activity.startActivity(intent)
				return true
			} catch (e: Exception) {
				quickToast(
					activity,
					String.format(
						activity.getString(
							R.string.error_toast_failed_open_external_browser
						),
						uri.toString()
					)
				)
			}
		} else {
			// We want to make sure we don't just pass this back to ourselves

			val baseIntent = Intent(Intent.ACTION_VIEW)
			baseIntent.setData(uri)

			val targetIntents = ArrayList<Intent>()

			for (info in activity.packageManager
				.queryIntentActivities(baseIntent, 0)) {
				val packageName = info.activityInfo.packageName

				if (packageName != null && !packageName.startsWith(
						"org.quantumbadger.redreader"
					)
				) {
					val intent = Intent(Intent.ACTION_VIEW)
					intent.setData(uri)
					intent.setPackage(packageName)
					targetIntents.add(intent)
				}
			}

			if (targetIntents.isNotEmpty()) {
				val chooserIntent = Intent.createChooser(
					targetIntents.removeAt(0),
					activity.getString(R.string.open_with)
				)

				if (targetIntents.isNotEmpty()) {
					chooserIntent.putExtra(
						Intent.EXTRA_INITIAL_INTENTS,
						targetIntents.toArray(arrayOf<Parcelable>())
					)
				}
				activity.startActivity(chooserIntent)

				return true
			}
		}

		return false
	}

	private fun openInternalBrowser(
		activity: AppCompatActivity,
		url: UriString?,
		post: RedditPost?
	) {
		if (url != null) {
			val intent = Intent()
			intent.setClass(activity, WebViewActivity::class.java)
			intent.putExtra("url", url)
			intent.putExtra("post", post)
			activity.startActivity(intent)
		}
	}

	@JvmStatic
	fun openCustomTab(
		activity: AppCompatActivity,
		uri: Uri,
		post: RedditPost?
	) {
		try {
			val intent = Intent()
			intent.setAction(Intent.ACTION_VIEW)
			intent.setData(uri)
			intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)

			val bundle = Bundle()
			bundle.putBinder("android.support.customtabs.extra.SESSION", null)
			intent.putExtras(bundle)

			intent.putExtra("android.support.customtabs.extra.SHARE_MENU_ITEM", true)

			val typedValue = TypedValue()
			activity.theme.resolveAttribute(
				com.google.android.material.R.attr.colorPrimary,
				typedValue,
				true
			)

			intent.putExtra(
				"android.support.customtabs.extra.TOOLBAR_COLOR",
				typedValue.data
			)

			intent.putExtra("android.support.customtabs.extra.ENABLE_URLBAR_HIDING", true)

			activity.startActivity(intent)
		} catch (e: ActivityNotFoundException) {
			// No suitable web browser installed. Use internal browser.
			openInternalBrowser(activity, UriString(uri.toString()), post)
		}
	}

	private val imgurPattern = Pattern.compile("https?://?(i\\.)?imgur\\.com/(\\w+).*")

	@JvmField
	val imgurAlbumPattern: Pattern = Pattern.compile(".*[^A-Za-z]imgur\\.com/(a|gallery)/([\\w\\-]+).*")
	private val redditGalleryPattern: Pattern = Pattern.compile(".*[^A-Za-z]reddit\\.com/gallery/(\\w+).*")
	private val qkmePattern1: Pattern = Pattern.compile(".*[^A-Za-z]qkme\\.me/(\\w+).*")
	private val qkmePattern2: Pattern = Pattern.compile(".*[^A-Za-z]quickmeme\\.com/meme/(\\w+).*")
	private val lvmePattern: Pattern = Pattern.compile(".*[^A-Za-z]livememe\\.com/(\\w+).*")
	private val gfycatPattern: Pattern = Pattern.compile(".*[^A-Za-z]gfycat\\.com/(?:gifs/detail/)?(\\w+).*")
	private val redgifsPattern: Pattern =
		Pattern.compile(".*[^A-Za-z]redgifs\\.com/watch/(?:gifs/detail/)?(\\w+).*")
	private val streamablePattern: Pattern = Pattern.compile(".*[^A-Za-z]streamable\\.com/(\\w+).*")
	private val reddituploadsPattern: Pattern =
		Pattern.compile(".*[^A-Za-z]i\\.reddituploads\\.com/(\\w+).*")
	private val redditVideosPattern: Pattern = Pattern.compile(".*[^A-Za-z]v.redd.it/(\\w+).*")
	private val imgflipPattern: Pattern = Pattern.compile(".*[^A-Za-z]imgflip\\.com/i/(\\w+).*")
	private val makeamemePattern: Pattern = Pattern.compile(".*[^A-Za-z]makeameme\\.org/meme/([\\w\\-]+).*")
	private val deviantartPattern: Pattern =
		Pattern.compile("https://www\\.deviantart\\.com/([\\w\\-]+)/art/([\\w\\-]+)")
	private val giphyPattern: Pattern = Pattern.compile(".*[^A-Za-z]giphy\\.com/gifs/(\\w+).*")

	@JvmStatic
	fun isProbablyAnImage(url: UriString?): Boolean {
		if (url == null) {
			return false
		}

		run {
			val matchImgur = imgurPattern.matcher(url.value)
			if (matchImgur.find()) {
				matchImgur.group(2)?.let { imgId ->
					if (imgId.length > 2 && !imgId.startsWith("gallery")) {
						return true
					}
				}
			}
		}

		run {
			val matchGfycat = gfycatPattern.matcher(url.value)
			if (matchGfycat.find()) {
				matchGfycat.group(1)?.let { imgId ->
					if (imgId.length > 5) {
						return true
					}
				}
			}
		}

		run {
			val matchRedgifs = redgifsPattern.matcher(url.value)
			if (matchRedgifs.find()) {
				matchRedgifs.group(1)?.let { imgId ->
					if (imgId.length > 5) {
						return true
					}
				}
			}
		}

		run {
			val matchStreamable = streamablePattern.matcher(url.value)
			if (matchStreamable.find()) {
				matchStreamable.group(1)?.let { imgId ->
					if (imgId.length > 2) {
						return true
					}
				}
			}
		}

		run {
			val matchRedditUploads = reddituploadsPattern.matcher(url.value)
			if (matchRedditUploads.find()) {
				matchRedditUploads.group(1)?.let { imgId ->
					if (imgId.length > 10) {
						return true
					}
				}
			}
		}

		run {
			val matchImgflip = imgflipPattern.matcher(url.value)
			if (matchImgflip.find()) {
				matchImgflip.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						return true
					}
				}
			}
		}

		run {
			val matchMakeameme = makeamemePattern.matcher(url.value)
			if (matchMakeameme.find()) {
				matchMakeameme.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						return true
					}
				}
			}
		}

		run {
			val matchDeviantart = deviantartPattern.matcher(url.value)
			if (matchDeviantart.find()) {
				if (url.value.length > 40) {
					return true
				}
			}
		}

		run {
			val matchRedditVideos = redditVideosPattern.matcher(url.value)
			if (matchRedditVideos.find()) {
				matchRedditVideos.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						return true
					}
				}
			}
		}

		return getImageUrlPatternMatch(url) != null
	}

	@JvmStatic
	fun getImgurImageInfo(
		context: Context,
		imgId: String,
		priority: Priority,
		returnUrlOnFailure: Boolean,
		listener: GetImageInfoListener
	) {
		if (isSensitiveDebugLoggingEnabled) {
			Log.i("getImgurImageInfo", "Image $imgId: trying API v3 with auth")
		}

		ImgurAPIV3.getImageInfo(
			context,
			imgId,
			priority,
			true,
			object : ImageInfoRetryListener(listener) {
				override fun onFailure(firstError: RRError) {
					if (isSensitiveDebugLoggingEnabled) {
						Log.i(
							"getImgurImageInfo",
							"Image $imgId: trying API v3 without auth"
						)
					}

					ImgurAPIV3.getImageInfo(
						context,
						imgId,
						priority,
						false,
						object : ImageInfoRetryListener(listener) {
							override fun onFailure(error: RRError) {
								if (isSensitiveDebugLoggingEnabled) {
									Log.i(
										"getImgurImageInfo",
										"Image $imgId: trying API v2"
									)
								}

								ImgurAPI.getImageInfo(
									context,
									imgId,
									priority,
									object : ImageInfoRetryListener(listener) {
										override fun onFailure(
											error: RRError
										) {
											Log.i(
												"getImgurImageInfo",
												"All API requests failed!"
											)

											if (returnUrlOnFailure) {
												listener.onSuccess(
													ImageInfo(
														original = ImageUrlInfo(url = UriString("https://i.imgur.com/$imgId.jpg")),
														hasAudio = HasAudio.MAYBE_AUDIO
													)
												)
											} else {
												listener.onFailure(firstError)
											}
										}
									})
							}
						})
				}
			})
	}

	private fun getImgurAlbumInfo(
		context: Context,
		albumUrl: UriString?,
		albumId: String,
		priority: Priority,
		listener: GetAlbumInfoListener
	) {
		if (isSensitiveDebugLoggingEnabled) {
			Log.i("getImgurAlbumInfo", "Album $albumId: trying API v3 with auth")
		}

		ImgurAPIV3.getAlbumInfo(
			context,
			albumUrl,
			albumId,
			priority,
			true,
			object : AlbumInfoRetryListener(listener) {
				override fun onFailure(firstError: RRError) {
					if (isSensitiveDebugLoggingEnabled) {
						Log.i(
							"getImgurAlbumInfo",
							"Album $albumId: trying API v3 without auth"
						)
					}

					ImgurAPIV3.getAlbumInfo(
						context,
						albumUrl,
						albumId,
						priority,
						false,
						object : AlbumInfoRetryListener(listener) {
							override fun onFailure(error: RRError) {
								if (isSensitiveDebugLoggingEnabled) {
									Log.i(
										"getImgurAlbumInfo",
										"Album $albumId: trying API v2"
									)
								}

								ImgurAPI.getAlbumInfo(
									context,
									albumUrl,
									albumId,
									priority,
									object : AlbumInfoRetryListener(listener) {
										override fun onFailure(
											error: RRError
										) {
											Log.i(
												"getImgurImageInfo",
												"All API requests failed!"
											)
											listener.onFailure(firstError)
										}
									})
							}
						})
				}
			})
	}

	@JvmStatic
	fun getAlbumInfo(
		context: Context,
		url: UriString,
		priority: Priority,
		listener: GetAlbumInfoListener
	) {
		run {
			val matchImgur = imgurAlbumPattern.matcher(url.value)
			if (matchImgur.find()) {
				matchImgur.group(2)?.let {
					val albumId = it.split("-").last()
					if (albumId.length > 2) {
						getImgurAlbumInfo(context, url, albumId, priority, listener)
						return
					}
				}
			}
		}

		run {
			val matchReddit = redditGalleryPattern.matcher(url.value)
			if (matchReddit.find()) {
				matchReddit.group(1)?.let { albumId ->
					if (albumId.length > 2) {
						getAlbumInfo(context, url, albumId, priority, listener)
						return
					}
				}
			}
		}

		listener.onFailure(
			getGeneralErrorForFailure(
				context,
				CacheRequest.RequestFailureType.MALFORMED_URL,
				null,
				null,
				url,
				Optional.empty()
			)
		)
	}

	@JvmStatic
	fun getImageInfo(
		context: Context,
		url: UriString?,
		priority: Priority,
		listener: GetImageInfoListener
	) {
		if (url == null) {
			listener.onNotAnImage()
			return
		}

		run {
			val matchImgur = imgurPattern.matcher(url.value)
			if (matchImgur.find()) {
				matchImgur.group(2)?.let { imgId ->
					if (imgId.length > 2 && !imgId.startsWith("gallery")) {
						getImgurImageInfo(context, imgId, priority, true, listener)
						return
					}
				}
			}
		}

		run {
			val matchGfycat = gfycatPattern.matcher(url.value)
			if (matchGfycat.find()) {
				matchGfycat.group(1)?.let { imgId ->
					if (imgId.length > 5) {
						GfycatAPI.getImageInfo(context, imgId, priority, listener)
						return
					}
				}
			}
		}

		run {
			val matchRedgifs = redgifsPattern.matcher(url.value)
			if (matchRedgifs.find()) {
				matchRedgifs.group(1)?.let { imgId ->
					if (imgId.length > 5) {
						RedgifsAPIV2.getImageInfo(
							context,
							imgId,
							priority,
							object : ImageInfoRetryListener(listener) {
								override fun onFailure(error: RRError) {
									Log.e(
										"getImageInfo",
										"RedGifs V2 failed, trying V1 ($error)",
										error.t
									)

									RedgifsAPI.getImageInfo(
										context,
										imgId,
										priority,
										object : ImageInfoRetryListener(listener) {
											override fun onFailure(error: RRError) {
												// Retry V2 so that the final error which is logged
												// relates to the V2 API

												Log.e(
													"getImageInfo",
													"RedGifs V1 also failed, retrying V2: $error",
													error.t
												)

												RedgifsAPIV2.getImageInfo(
													context,
													imgId,
													priority,
													listener
												)
											}
										})
								}
							})
						return
					}
				}
			}
		}

		run {
			val matchStreamable = streamablePattern.matcher(url.value)
			if (matchStreamable.find()) {
				matchStreamable.group(1)?.let { imgId ->
					if (imgId.length > 2) {
						StreamableAPI.getImageInfo(
							context,
							imgId,
							priority,
							listener
						)
						return
					}
				}
			}
		}
		run {
			val matchDeviantart = deviantartPattern.matcher(url.value)
			if (matchDeviantart.find()) {
				if (url.value.length > 40) {
					DeviantArtAPI.getImageInfo(
						context,
						url,
						priority,
						listener
					)
					return
				}
			}
		}
		run {
			val matchRedditVideos = redditVideosPattern.matcher(url.value)
			if (matchRedditVideos.find()) {
				matchRedditVideos.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						RedditVideosAPI.getImageInfo(
							context,
							imgId,
							priority,
							listener
						)
						return
					}
				}
			}
		}

		val imageUrlPatternMatch = getImageUrlPatternMatch(url)

		if (imageUrlPatternMatch != null) {
			listener.onSuccess(imageUrlPatternMatch)
		} else {
			listener.onNotAnImage()
		}
	}

	private fun getImageUrlPatternMatch(url: UriString): ImageInfo? {
		val urlLower = StringUtils.asciiLowercase(url.value)

		run {
			val matchRedditUploads = reddituploadsPattern.matcher(url.value)
			if (matchRedditUploads.find()) {
				matchRedditUploads.group(1)?.let { imgId ->
					if (imgId.length > 10) {
						return ImageInfo(
							original = ImageUrlInfo(url = url),
							mediaType = ImageInfo.MediaType.IMAGE,
							hasAudio = HasAudio.NO_AUDIO
						)
					}
				}
			}
		}

		run {
			val matchImgflip = imgflipPattern.matcher(url.value)
			if (matchImgflip.find()) {
				matchImgflip.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						val imageUrl = "https://i.imgflip.com/$imgId.jpg"
						return ImageInfo(
							original = ImageUrlInfo(UriString(imageUrl)),
							mediaType = ImageInfo.MediaType.IMAGE,
							hasAudio = HasAudio.NO_AUDIO
						)
					}
				}
			}
		}

		run {
			val matchMakeameme = makeamemePattern.matcher(url.value)
			if (matchMakeameme.find()) {
				matchMakeameme.group(1)?.let { imgId ->
					if (imgId.length > 3) {
						val imageUrl = ("https://media.makeameme.org/created/"
								+ imgId
								+ ".jpg")
						return ImageInfo(
							original = ImageUrlInfo(UriString(imageUrl)),
							mediaType = ImageInfo.MediaType.IMAGE,
							hasAudio = HasAudio.NO_AUDIO
						)
					}
				}
			}
		}

		run {
			val matchGiphy = giphyPattern.matcher(url.value)
			if (matchGiphy.find()) {
				return ImageInfo(
					original = ImageUrlInfo(UriString(
						"https://media.giphy.com/media/"
								+ matchGiphy.group(1)
								+ "/giphy.mp4"
					)),
					mediaType = ImageInfo.MediaType.VIDEO,
					hasAudio = HasAudio.NO_AUDIO
				)
			}
		}

		val imageExtensions = arrayOf(".jpg", ".jpeg", ".png")

		val videoExtensions = arrayOf(
			".webm",
			".mp4",
			".h264",
			".gifv",
			".mkv",
			".3gp"
		)


		for (ext in imageExtensions) {
			if (urlLower.endsWith(ext)) {
				return ImageInfo(
					original = ImageUrlInfo(url),
					mediaType = ImageInfo.MediaType.IMAGE,
					hasAudio = HasAudio.MAYBE_AUDIO
				)
			}
		}

		for (ext in videoExtensions) {
			if (urlLower.endsWith(ext)) {
				return ImageInfo(
					original = ImageUrlInfo(url),
					mediaType = ImageInfo.MediaType.VIDEO,
					hasAudio = HasAudio.MAYBE_AUDIO
				)
			}
		}

		if (urlLower.endsWith(".gif")) {
			val audio = if (urlLower.contains(".redd.it")) { // preview.redd.it or i.redd.it
				HasAudio.NO_AUDIO
			} else {
				HasAudio.MAYBE_AUDIO
			}

			return ImageInfo(
				ImageUrlInfo(url),
				mediaType = ImageInfo.MediaType.GIF,
				hasAudio = audio,
				isAnimated = true
			)
		}


		if (url.value.contains("?")) {
			val urlBeforeQ = urlLower.split("\\?".toRegex()).dropLastWhile { it.isEmpty() }
				.toTypedArray()[0]

			for (ext in imageExtensions) {
				if (urlBeforeQ.endsWith(ext)) {
					return ImageInfo(
						ImageUrlInfo(url),
						mediaType = ImageInfo.MediaType.IMAGE,
						hasAudio = HasAudio.MAYBE_AUDIO
					)
				}
			}

			for (ext in videoExtensions) {
				if (urlBeforeQ.endsWith(ext)) {
					return ImageInfo(
						ImageUrlInfo(url),
						mediaType = ImageInfo.MediaType.VIDEO,
						hasAudio = HasAudio.MAYBE_AUDIO
					)
				}
			}

			if (urlBeforeQ.endsWith(".gif")) {
				val audio = if (urlLower.contains(".redd.it")) { // preview.redd.it or i.redd.it
					HasAudio.NO_AUDIO
				} else {
					HasAudio.MAYBE_AUDIO
				}

				return ImageInfo(
					ImageUrlInfo(url),
					mediaType = ImageInfo.MediaType.GIF,
					hasAudio = audio
				)
			}
		}

		val matchQkme1 = qkmePattern1.matcher(url.value)

		if (matchQkme1.find()) {
			matchQkme1.group(1)?.let { imgId ->
				if (imgId.length > 2) {
					return ImageInfo(
						original = ImageUrlInfo(UriString(
							String.format(
								Locale.US,
								"http://i.qkme.me/%s.jpg",
								imgId
							)
						)),
						mediaType = ImageInfo.MediaType.IMAGE,
						hasAudio = HasAudio.NO_AUDIO
					)
				}
			}
		}

		val matchQkme2 = qkmePattern2.matcher(url.value)

		if (matchQkme2.find()) {
			matchQkme2.group(1)?.let { imgId ->
				if (imgId.length > 2) {
					return ImageInfo(
						original = ImageUrlInfo(UriString(
							String.format(
								Locale.US,
								"http://i.qkme.me/%s.jpg",
								imgId
							)
						)),
						mediaType = ImageInfo.MediaType.IMAGE,
						hasAudio = HasAudio.NO_AUDIO
					)
				}
			}
		}

		val matchLvme = lvmePattern.matcher(url.value)

		if (matchLvme.find()) {
			matchLvme.group(1)?.let { imgId ->
				if (imgId.length > 2) {
					return ImageInfo(
						original = ImageUrlInfo(UriString(
							String.format(
								Locale.US,
								"http://www.livememe.com/%s.jpg",
								imgId
							)
						)),
						mediaType = ImageInfo.MediaType.IMAGE,
						hasAudio = HasAudio.NO_AUDIO
					)
				}
			}
		}

		return null
	}

	@JvmStatic
	fun computeAllLinks(text: String): LinkedHashSet<UriString> {
		val result = LinkedHashSet<UriString>()

		// From http://stackoverflow.com/a/1806161/1526861
		// TODO may not handle .co.uk, similar (but should handle .co/.us/.it/etc fine)
		val urlPattern = Pattern.compile(
			"\\b((((ht|f)tp(s?)://|~/|/)|www.)" +
					"(\\w+:\\w+@)?(([-\\w]+\\.)+(com|org|net|gov" +
					"|mil|biz|info|mobi|name|aero|jobs|museum" +
					"|travel|[a-z]{2}))(:\\d{1,5})?" +
					"(((/([-\\w~!$+|.,=]|%[a-f\\d]{2})+)+|/)+|\\?|#)?" +
					"((\\?([-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
					"([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)" +
					"(&(?:[-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
					"([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)*)*" +
					"(#([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)?)\\b"
		)

		val urlMatcher = urlPattern.matcher(text)

		while (urlMatcher.find()) {
			urlMatcher.group(1)?.let { result.add(UriString(it)) }
		}

		val subredditMatcher = Pattern.compile("(?<!\\w)(/?[ru]/\\w+)\\b")
			.matcher(text)

		while (subredditMatcher.find()) {
			subredditMatcher.group(1)?.let { result.add(UriString(it)) }
		}

		return result
	}

	@JvmStatic
	fun shareText(
		activity: AppCompatActivity,
		subject: String?,
		text: String?
	) {
		val mailer = Intent(Intent.ACTION_SEND)
		mailer.setType("text/plain")
		mailer.putExtra(Intent.EXTRA_TEXT, text ?: "<null>")

		if (subject != null) {
			mailer.putExtra(Intent.EXTRA_SUBJECT, subject)
		}

		if (PrefsUtility.pref_behaviour_sharing_dialog()) {
			ShareOrderDialog.newInstance(mailer).show(
				activity.supportFragmentManager,
				null
			)
		} else {
			activity.startActivity(
				Intent.createChooser(
					mailer,
					activity.getString(R.string.action_share)
				)
			)
		}
	}

	fun convertAndNormalizeUri(uri: UriString): Uri {
		@Suppress("NAME_SHADOWING") var uri = uri.value
		if (uri.startsWith("r/") || uri.startsWith("u/")) {
			uri = "/$uri"
		}

		if (uri.startsWith("/")) {
			uri = "https://reddit.com$uri"
		}

		if (!uri.contains("://") && !uri.startsWith("mailto:")) {
			uri = "http://$uri"
		}

		val parsedUri = Uri.parse(uri).normalizeScheme()
		val uriBuilder = parsedUri.buildUpon()

		val authority = parsedUri.encodedAuthority
		if (authority != null) {
			val normalAuthority: String

			//Don't lowercase the rare userinfo component if present.
			if (authority.contains("@")) {
				val authorityParts = authority.split("@".toRegex(), limit = 2).toTypedArray()
				normalAuthority =
					authorityParts[0] + "@" + StringUtils.asciiLowercase(
						authorityParts[1]
					)
			} else {
				normalAuthority = StringUtils.asciiLowercase(authority)
			}

			uriBuilder.encodedAuthority(normalAuthority)
		}

		return uriBuilder.build()
	}

	@JvmStatic
	fun getPreferredRedditUriString(uri: UriString): UriString {
		val parsedUri = convertAndNormalizeUri(uri)

		//Return non-Reddit links normalized but otherwise unaltered
		if (RedditURLParser.parse(parsedUri) == null) {
			return UriString.from(parsedUri)
		}

		//Respect non-participation links
		if (parsedUri.host == "np.reddit.com") {
			return UriString.from(parsedUri)
		}

		val potentialPostLink = PostCommentListingURL.parse(parsedUri)
		val postId = if (potentialPostLink != null && potentialPostLink.commentId == null) {
			//Direct link to a post, not to a comment or anything else
			potentialPostLink.postId
		} else {
			null
		}

		val preferredDomain = PrefsUtility.pref_behaviour_sharing_domain()

		//Only direct links to posts will be converted to redd.it links
		if (preferredDomain == SharingDomain.SHORT_REDDIT && postId == null) {
			return UriString.from(parsedUri)
		}

		val uriBuilder = parsedUri.buildUpon()

		uriBuilder.encodedAuthority(preferredDomain.domain)
		if (preferredDomain == SharingDomain.SHORT_REDDIT) {
			uriBuilder.encodedPath("/$postId")
		}

		return UriString.from(uriBuilder.build())
	}

	enum class LinkAction(val descriptionResId: Int) {
		SHARE(R.string.action_share),
		COPY_URL(R.string.action_copy_link),
		SHARE_IMAGE(R.string.action_share_image),
		SAVE_IMAGE(R.string.action_save),
		EXTERNAL(R.string.action_external)
	}

	private abstract class ImageInfoRetryListener(private val mListener: GetImageInfoListener) :
		GetImageInfoListener {
		override fun onSuccess(info: ImageInfo) {
			mListener.onSuccess(info)
		}

		override fun onNotAnImage() {
			mListener.onNotAnImage()
		}
	}

	private abstract class AlbumInfoRetryListener(private val mListener: GetAlbumInfoListener) :
		GetAlbumInfoListener {
		override fun onGalleryRemoved() {
			mListener.onGalleryRemoved()
		}

		override fun onGalleryDataNotPresent() {
			mListener.onGalleryDataNotPresent()
		}

		override fun onSuccess(info: AlbumInfo) {
			mListener.onSuccess(info)
		}
	}

	private class LinkMenuItem(context: Context, titleRes: Int, val action: LinkAction) {
		val title: String = context.getString(titleRes)
	}
}
