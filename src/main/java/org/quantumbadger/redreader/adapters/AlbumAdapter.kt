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
package org.quantumbadger.redreader.adapters

import android.annotation.SuppressLint
import android.graphics.BitmapFactory
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached
import org.quantumbadger.redreader.common.AndroidCommon.runOnUiThread
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.General.isConnectionWifi
import org.quantumbadger.redreader.common.General.isSensitiveDebugLoggingEnabled
import org.quantumbadger.redreader.common.General.showResultDialog
import org.quantumbadger.redreader.common.GenericFactory
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.NeverAlwaysOrWifiOnly
import org.quantumbadger.redreader.common.Optional
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.common.Priority
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.viewholders.VH3TextIcon
import java.io.IOException
import java.util.Locale
import java.util.UUID

class AlbumAdapter(
	private val activity: BaseActivity,
	private val albumInfo: AlbumInfo
) : RecyclerView.Adapter<VH3TextIcon>() {

	override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): VH3TextIcon {
		val v = LayoutInflater.from(parent.context)
			.inflate(R.layout.list_item_3_text_icon, parent, false)
		return VH3TextIcon(v)
	}

	@SuppressLint("SetTextI18n")
	override fun onBindViewHolder(vh: VH3TextIcon, position: Int) {
		val bindingId = ++vh.bindingId

		val imageInfo = albumInfo.images[position]

		if (imageInfo.title == null || imageInfo.title.trim { it <= ' ' }.isEmpty()) {
			vh.text.text = activity.getString(
				R.string.album_image_default_text,
				position + 1
			)
		} else {
			vh.text.text = (position + 1).toString() + ". " + imageInfo.title.trim { it <= ' ' }
		}

		var subtitle = ""

		if (imageInfo.type != null) {
			subtitle += imageInfo.type
		}

		imageInfo.original?.size?.apply {
			if (subtitle.isNotEmpty()) {
				subtitle += ", "
			}
			subtitle += "${width}x$height"
		}

		imageInfo.original?.sizeBytes?.let { sizeBytes ->
			if (subtitle.isNotEmpty()) {
				subtitle += ", "
			}

			subtitle += if (sizeBytes < 512 * 1024) {
				String.format(Locale.US, "%.1f kB", sizeBytes.toFloat() / 1024)
			} else {
				String.format(
					Locale.US,
					"%.1f MB",
					sizeBytes.toFloat() / (1024 * 1024)
				)
			}
		}


		vh.text2.visibility = if (subtitle.isEmpty()) View.GONE else View.VISIBLE

		vh.text2.text = subtitle

		if (!imageInfo.caption.isNullOrEmpty()) {
			vh.text3.text = imageInfo.caption
			vh.text3.visibility = View.VISIBLE
		} else {
			vh.text3.visibility = View.GONE
		}

		vh.removeExtras()

		if (!imageInfo.outboundUrl.isNullOrEmpty()) {
			vh.addLinkButton(activity, imageInfo.outboundUrl)
		}

		vh.icon.setImageBitmap(null)

		val isConnectionWifi = isConnectionWifi(
			activity
		)

		val thumbnailsPref = PrefsUtility.appearance_thumbnails_show()

		val downloadThumbnails = (thumbnailsPref === NeverAlwaysOrWifiOnly.ALWAYS
				|| (thumbnailsPref === NeverAlwaysOrWifiOnly.WIFIONLY
				&& isConnectionWifi))

		if (!downloadThumbnails || (imageInfo.bigSquare == null && imageInfo.original == null)) {
			vh.icon.visibility = View.GONE
		} else {
			vh.text2.visibility = View.VISIBLE

			val thumbnailUrl = (imageInfo.bigSquare ?: imageInfo.preview ?: imageInfo.original)?.url

			CacheManager.getInstance(activity).makeRequest(
				CacheRequest(
					thumbnailUrl!!,
					RedditAccountManager.getAnon(),
					null,
					Priority(Constants.Priority.THUMBNAIL, position),
					DownloadStrategyIfNotCached.INSTANCE,
					Constants.FileType.THUMBNAIL,
					CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
					activity,
					object : CacheRequestCallbacks {
						override fun onFailure(error: RRError) {
							if (isSensitiveDebugLoggingEnabled) {
								Log.e(
									"AlbumAdapter",
									"Failed to fetch thumbnail $thumbnailUrl: $error",
									error.t
								)
							}
						}

						override fun onDataStreamComplete(
							streamFactory: GenericFactory<SeekableInputStream, IOException>,
							timestamp: TimestampUTC,
							session: UUID,
							fromCache: Boolean,
							mimetype: String?
						) {
							try {
								val stream = streamFactory.create()

								val bitmap = BitmapFactory.decodeStream(stream)

								runOnUiThread {
									if (vh.bindingId == bindingId) {
										vh.icon.setImageBitmap(bitmap)
									}
								}
							} catch (e: IOException) {
								onFailure(
									getGeneralErrorForFailure(
										activity,
										CacheRequest.REQUEST_FAILURE_CONNECTION,
										e,
										null,
										null,
										Optional.empty()
									)
								)
							}
						}
					})
			)
		}

		if (imageInfo.original != null) {
			vh.itemView.setOnClickListener { _: View? ->
				LinkHandler.onLinkClicked(
					activity,
					imageInfo.original.url,
					false,
					null,
					albumInfo,
					vh.adapterPosition
				)
			}

			vh.itemView.setOnLongClickListener { _: View? ->
				LinkHandler.onLinkLongClicked(
					activity, imageInfo.original.url, false
				)
				true
			}

		} else {
			vh.itemView.setOnClickListener { v: View? ->
				showResultDialog(
					activity,
					RRError(
						activity.getString(R.string.image_gallery_no_image_present_title),
						activity.getString(R.string.image_gallery_no_image_present_message),
						true,
						RuntimeException(),
						null,
						albumInfo.url,
						null
					)
				)
			}
		}
	}

	override fun getItemCount() = albumInfo.images.size
}
