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

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Icon
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.constraintlayout.compose.ConstraintLayout
import androidx.constraintlayout.compose.Dimension
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.invokeIf
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.GlobalNetworkRetry
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText

@Composable
fun RRErrorView(error: RRError) {

	val theme = LocalComposeTheme.current.error
	val launch = LocalLauncher.current

	var size by remember { mutableStateOf(IntSize.Zero) }

	Box(
		modifier = Modifier
			.padding(8.dp)
			.fillMaxWidth()
			.onSizeChanged { size = it },
		contentAlignment = Alignment.Center
	) {
		val smallWidth = with(LocalDensity.current) {
			size.width.toDp() < 150.dp
		}

		Box(
			Modifier
				.fillMaxWidth()
				.border(1.dp, theme.border, RoundedCornerShape(6.dp))
				.clip(RoundedCornerShape(6.dp))
				.background(theme.background)
				.invokeIf(smallWidth) {
					fillMaxHeight().clickable(onClick = {
						launch(Dest.ResultDialog(error))
					})
				}
				.padding(12.dp),
			contentAlignment = Alignment.Center
		) {
			if (smallWidth) {
				Icon(
					painter = painterResource(id = R.drawable.alert_circle_outline),
					contentDescription = stringResource(R.string.error_title),
					tint = theme.border,
				)

			} else {
				ConstraintLayout(Modifier.fillMaxWidth()) {

					val (icon, text) = createRefs()

					Icon(
						painter = painterResource(id = R.drawable.alert_circle_outline),
						contentDescription = null,
						modifier = Modifier
							.constrainAs(icon) {
								start.linkTo(parent.start)
								top.linkTo(parent.top)
							},
						tint = theme.border
					)

					Column(
						Modifier
							.constrainAs(text) {
								start.linkTo(icon.end, 16.dp)
								end.linkTo(parent.end)
								top.linkTo(parent.top)
								bottom.linkTo(parent.bottom)
								width = Dimension.fillToConstraints
							}
					) {

						theme.title.StyledText(error.title ?: stringResource(R.string.error_title))

						error.message?.let {
							Spacer(Modifier.height(3.dp))
							theme.message.StyledText(it)
						}

						Spacer(Modifier.height(14.dp))

						Row(
							Modifier.fillMaxWidth(),
							horizontalArrangement = Arrangement.End
						) {
							RRButton(
								onClick = {
									launch(Dest.ErrorPropertiesDialog(error))
								},
								text = stringResource(R.string.button_error_details),
								theme = if (error.resolution == null) {
									theme.primaryButton
								} else {
									theme.secondaryButton
								}
							)

							if (error.resolution != null) {
								Spacer(Modifier.width(8.dp))

								RRButton(
									onClick = {
										when (error.resolution) {
											RRError.Resolution.ACCEPT_REDDIT_TERMS -> {
												launch(Dest.RedditTerms)
											}
											RRError.Resolution.ACCOUNTS_LIST -> {
												launch(Dest.AccountsList)
											}
											RRError.Resolution.RETRY -> {
												GlobalNetworkRetry.intValue++
											}
										}
									},
									text = stringResource(error.resolution.buttonText),
									theme = theme.primaryButton
								)
							}
						}
					}
				}
			}
		}
	}
}

@Composable
@Preview
private fun PreviewRRErrorView() {
	RRComposeContextTest {
		RRErrorView(
			error = RRError(
				title = "Error title",
				message = "Error message"
			)
		)
	}
}

@Composable
@Preview(widthDp = 64, heightDp = 64)
private fun PreviewRRErrorViewSmall() {
	RRComposeContextTest {
		RRErrorView(
			error = RRError(
				title = "Error title",
				message = "Error message"
			)
		)
	}
}
