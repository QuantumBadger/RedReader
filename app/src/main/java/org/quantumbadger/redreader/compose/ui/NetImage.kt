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

package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
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
import org.quantumbadger.redreader.common.invokeIfNotNull
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
	showVideoPlayOverlay: Boolean = false,
	maxCanvasDimension: Int = 2048,
) {
	val theme = LocalComposeTheme.current

	val expectedImageAspect = image.size?.takeIf { it.height > 0 }?.let {
		it.width.toFloat() / it.height.toFloat()
	}

	val url = image.url

	val data by fetchImage(url, scaleToMaxAxis = maxCanvasDimension)

	Box(
		modifier = modifier
            .invokeIfNotNull(cropToAspect, Modifier::aspectRatio)
            .invokeIf(data is NetRequestStatus.Success) {
                background(theme.postCard.previewImageBackgroundColor)
            },
		contentAlignment = Alignment.Center,
	) {

		if (expectedImageAspect != null
			&& data !is NetRequestStatus.Success
			&& data !is NetRequestStatus.Failed
		) {
			// Pad the view to the desired aspect ratio
			Box(
                Modifier
                    .fillMaxWidth()
                    .aspectRatio(expectedImageAspect)
			)
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
					progress = { it.fractionComplete }
				)
			}

			is NetRequestStatus.Failed -> {
				RRErrorView(error = it.error)
			}

			is NetRequestStatus.Success -> {
				Image(
					modifier = if (cropToAspect == null) {
						val bitmap = it.result.data

						val imageAspect = if (bitmap.height > 0) {
							bitmap.width.toDouble() / bitmap.height.toDouble()
						} else {
							0.0
						}

                        Modifier
                            .fillMaxWidth()
                            .aspectRatio(imageAspect.toFloat(), matchHeightConstraintsFirst = true)
					} else {
						Modifier.fillMaxSize()
					},
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
						Box(
                            Modifier
                                .clip(CircleShape)
                                .background(Color(0f, 0f, 0f, 0.7f))
                                .padding(12.dp)
						) {
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
