package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL
import org.quantumbadger.redreader.reddit.url.RedditURLParser
import org.quantumbadger.redreader.reddit.url.RedditURLParser.RedditURL

@Suppress("PropertyName")
@Serializable
@Parcelize
data class RedditMore(
	var count: Int,
	var children: List<String>,
	var parent_id: String
) : Parcelable {

	fun getMoreUrls(
		commentListingURL: RedditURL
	): List<PostCommentListingURL> {

		val urls = ArrayList<PostCommentListingURL>(16)

		if (commentListingURL.pathType() == RedditURLParser.POST_COMMENT_LISTING_URL) {
			if (count > 0) {
				for (child in children) {
					urls.add(commentListingURL.asPostCommentListURL().commentId(child))
				}
			} else {
				urls.add(commentListingURL.asPostCommentListURL().commentId(parent_id))
			}
		}
		return urls
	}
}
