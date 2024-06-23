package org.quantumbadger.redreader.compose.ui

import androidx.compose.animation.animateContentSize
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.widthIn
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchImage
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.ImageUrlInfo

@Composable
fun NetImage(
	image: ImageUrlInfo?,
	maxImageWidth: Dp? = null,
	maxImageHeight: Dp? = null,
	cropToAspect: Float? = null
) {
	Box(
		modifier = Modifier
            .fillMaxWidth()
            .animateContentSize(),
		contentAlignment = Alignment.Center,
	) {
		val theme = LocalComposeTheme.current
		val url = image?.url?.let(General::uriFromString)

		if (url == null) {
			// TODO show error
			return
		}

		val data by fetchImage(
			uri = url,
			user = RedditAccountId(""), // TODO
		)

		val aspectRatio = cropToAspect ?: image.size?.takeIf { it.height > 0 }?.let {
			it.width.toFloat() / it.height.toFloat()
		}

		val imageModifier = Modifier
            .fillMaxWidth()
            .widthIn(max = maxImageWidth ?: Dp.Unspecified)
            .heightIn(max = maxImageHeight ?: Dp.Unspecified)

		if (data !is NetRequestStatus.Success && aspectRatio != null) {
			Box(modifier = imageModifier.aspectRatio(aspectRatio))
		}

		when (val it = data) {
			NetRequestStatus.Connecting -> {
				CircularProgressIndicator(
					Modifier.padding(24.dp)
				)
			}

			is NetRequestStatus.Downloading -> {
				CircularProgressIndicator(
					modifier = Modifier.padding(24.dp),
					progress = it.fractionComplete
				)
			}

			is NetRequestStatus.Failed -> {
				// TODO
			}

			is NetRequestStatus.Success -> {
				Box(
                    Modifier
                        .fillMaxWidth()
                        .background(
                            theme.postCard.previewImageBackgroundColor
                        )
				) {
					Image(
						modifier = imageModifier,
						bitmap = it.result,
						contentDescription = null,
						contentScale = if (cropToAspect == null) {
							ContentScale.Fit
						} else {
							ContentScale.Crop
						}
					)
				}
			}
		}
	}
}
