package org.quantumbadger.redreader.compose.ui

import androidx.compose.animation.animateContentSize
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.invokeIf
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchImage
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.ImageUrlInfo

// TODO dynamically select the best preview based on size
@Composable
fun NetImage(
	modifier: Modifier,
	image: ImageUrlInfo,
	cropToAspect: Float? = null,
	showVideoPlayOverlay: Boolean = false
) {
	val aspectRatio = cropToAspect ?: image.size?.takeIf { it.height > 0 }?.let {
		it.width.toFloat() / it.height.toFloat()
	}

	val url = image.url

	val data by fetchImage(url)

	Box(
		modifier = modifier
			.invokeIf(
				(cropToAspect != null &&
						data !is NetRequestStatus.Success &&
						data !is NetRequestStatus.Failed) || aspectRatio != null
			) {
				aspectRatio(aspectRatio!!)
			}
			.animateContentSize(),
		contentAlignment = Alignment.Center,
	) {
		val theme = LocalComposeTheme.current

		when (val it = data) {
			NetRequestStatus.Connecting -> {
				CircularProgressIndicator(
					Modifier.padding(24.dp)
				)
			}

			is NetRequestStatus.Downloading -> {
				CircularProgressIndicator(
					modifier = Modifier.padding(24.dp),
					progress = { it.fractionComplete }
				)
			}

			is NetRequestStatus.Failed -> {
				RRErrorView(error = it.error)
			}

			is NetRequestStatus.Success -> {
				Box(Modifier.background(theme.postCard.previewImageBackgroundColor)) {
					Image(
						modifier = Modifier.fillMaxSize(),
						bitmap = it.result.data,
						contentDescription = null,
						contentScale = if (cropToAspect == null) {
							ContentScale.Fit
						} else {
							ContentScale.Crop
						}
					)

					if (showVideoPlayOverlay) {
						Box(
							Modifier
								.background(Color(0f, 0f, 0f, 0.2f))
								.matchParentSize(),
							contentAlignment = Alignment.Center
						) {
							Box(Modifier
								.clip(CircleShape)
								.background(Color(0f, 0f, 0f, 0.7f))
								.padding(12.dp)) {
								Icon(
									painter = painterResource(R.drawable.icon_play),
									contentDescription = null,
									tint = Color.White
								)
							}
						}
					}
				}
			}
		}
	}
}
