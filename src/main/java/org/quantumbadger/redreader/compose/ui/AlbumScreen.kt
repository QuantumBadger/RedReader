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

import android.util.Log
import androidx.compose.animation.AnimatedVisibility
import androidx.compose.animation.animateContentSize
import androidx.compose.animation.fadeIn
import androidx.compose.animation.fadeOut
import androidx.compose.animation.slideInVertically
import androidx.compose.animation.slideOutVertically
import androidx.compose.foundation.background
import androidx.compose.foundation.focusable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.add
import androidx.compose.foundation.layout.asPaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.layout.systemBarsPadding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.GridItemSpan
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.HorizontalDivider
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.Stable
import androidx.compose.runtime.derivedStateOf
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.res.pluralStringResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.role
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.delay
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.ctx.Dest
import org.quantumbadger.redreader.compose.ctx.LocalLauncher
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchAlbum
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import org.quantumbadger.redreader.compose.theme.combinedClickableWithHaptics
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.image.ImageInfo
import org.quantumbadger.redreader.settings.types.AlbumViewMode
import kotlin.math.min

@Composable
fun AlbumScreen(
	onBackPressed: () -> Unit,
	albumUrl: UriString
) {
	val album by fetchAlbum(albumUrl)

	when (val it = album) {
		NetRequestStatus.Connecting, is NetRequestStatus.Downloading -> {
			InitialContainer {
				CircularProgressIndicator()
			}
		}

		is NetRequestStatus.Failed -> {
			InitialContainer {
				RRErrorView(error = it.error)
			}
		}

		is NetRequestStatus.Success -> {
			AlbumScreen(
				onBackPressed = onBackPressed,
				album = it.result
			)
		}
	}
}

@Composable
private fun InitialContainer(
	modifier: Modifier = Modifier,
	content: @Composable () -> Unit
) {
	val theme = LocalComposeTheme.current

	Box(
        modifier
            .fillMaxSize()
            .background(theme.postCard.listBackgroundColor)
            .systemBarsPadding(),
		contentAlignment = Alignment.Center
	) {
		content()
	}
}

@Composable
private fun DoOnce(input: AlbumInfo, action: () -> Unit) {

	// Only do this once, even if the activity gets relaunched
	var alreadyDone by rememberSaveable(input) {
		mutableStateOf(false)
	}

	LaunchedEffect(input) {
		if (!alreadyDone) {
			alreadyDone = true
			action()
		}
	}
}

@Composable
fun AlbumScreen(
	onBackPressed: () -> Unit,
	album: AlbumInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current
	val launch = LocalLauncher.current

	val accessibilityFocusRequester = remember { FocusRequester() }
	val focusManager = LocalFocusManager.current

	// Workaround for potential Compose accessibility issue. Someone else reported this here:
	// https://stackoverflow.com/questions/78705279/talkback-does-not-focus-at-the-top-of-the-screen-on-navigation-with-compose
	// Without this code, the screenreader focus jumps to one of the "share" buttons halfway down
	// the screen, rather than the header at the top.
	// Only the `requestFocus()` call actually seems to have any effect here.
	LaunchedEffect(album) {
		try {
			focusManager.clearFocus()
			delay(1500)
			accessibilityFocusRequester.freeFocus()
			accessibilityFocusRequester.requestFocus()
		} catch (e: Exception) {
			Log.e("AlbumScreen", "requestFocus exception", e)
		}
	}

	DoOnce(album) {
		if (PrefsUtility.pref_album_skip_to_first()) {
			album.images.firstOrNull()?.let {
				launch(Dest.Link(
					url = it.original.url,
					albumInfo = album,
					albumImageIndex = 0
				))
			}
		}
	}

	val topBarHeight = 48.dp

	val contentPadding: PaddingValues = WindowInsets(bottom = 24.dp)
		.add(WindowInsets.systemBars)
		.asPaddingValues()

	val itemClickHandler: (Int) -> Unit = { index ->
		album.images[index].original.apply {
			launch(
				Dest.Link(
					url = url,
					albumInfo = album,
					albumImageIndex = index
				)
			)
		}
	}

	val itemLongClickListener: (Int) -> Unit = { index ->
		album.images[index].original.apply {
			launch(Dest.LinkLongClick(url))
		}
	}

	val head = @Composable {
		Column(
            Modifier
                .padding(horizontal = 16.dp)
                .fillMaxWidth()
				.animateContentSize()
		) {

			// Space for the top bar
			Spacer(Modifier.height(topBarHeight))

			AnimatedVisibility(
				modifier = Modifier.fillMaxWidth(),
				visible = !prefs.albumCompactTitle.value,
				enter = fadeIn() + slideInVertically { -it },
				exit = fadeOut() + slideOutVertically { -it }
			) {

				Column {
					Spacer(Modifier.height(10.dp))

					theme.album.title.StyledText(
						modifier = Modifier
							.fillMaxWidth()
							.focusRequester(accessibilityFocusRequester)
							.focusable(true)
							.semantics {
								heading()
							},
						text = album.title ?: stringResource(R.string.image_gallery),
						overflow = TextOverflow.Ellipsis,
						maxLines = 2
					)

					Spacer(Modifier.height(6.dp))

					theme.album.subtitle.StyledText(
						modifier = Modifier.fillMaxWidth(),
						text = album.description ?: pluralStringResource(
							R.plurals.album_image_count,
							album.images.size,
							album.images.size
						),
						overflow = TextOverflow.Ellipsis,
						maxLines = 3
					)

					Spacer(Modifier.height(16.dp))
				}
			}
		}
	}

	Box {

		// Content

		val scrollState = when (prefs.albumViewMode.value) {
			AlbumViewMode.Cards -> {

				val state = rememberLazyListState()

				LazyColumn(
                    Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
					contentPadding = contentPadding,
					state = state
				) {
					item {
						head()
					}

					items(count = album.images.size) {
						AlbumCard(
							index = it,
							image = album.images[it],
							onClick = itemClickHandler,
							onLongClick = itemLongClickListener
						)
					}
				}

				object : TopBarScrollObservable {
					override val firstVisibleItemIndex: Int
						get() = state.firstVisibleItemIndex
					override val firstVisibleItemOffset: Int
						get() = state.firstVisibleItemScrollOffset
				}
			}

			AlbumViewMode.List -> {
				val state = rememberLazyListState()

				LazyColumn(
                    Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
					contentPadding = contentPadding,
					state = state
				) {
					item {
						head()
						HorizontalDivider(thickness = 0.5.dp)
					}

					items(count = album.images.size) {
						AlbumListItem(
							index = it,
							image = album.images[it],
							onClick = itemClickHandler,
							onLongClick = itemLongClickListener,
						)
						HorizontalDivider(thickness = 0.5.dp)
					}
				}

				object : TopBarScrollObservable {
					override val firstVisibleItemIndex: Int
						get() = state.firstVisibleItemIndex
					override val firstVisibleItemOffset: Int
						get() = state.firstVisibleItemScrollOffset
				}
			}

			AlbumViewMode.Grid -> {
				val state = rememberLazyGridState()
				val colCount = prefs.albumGridColumns.value

				LazyVerticalGrid(
					state = state,
					columns = GridCells.Fixed(colCount),
					modifier = Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
					contentPadding = contentPadding,
					verticalArrangement = Arrangement.spacedBy(2.dp),
					horizontalArrangement = Arrangement.spacedBy(2.dp),
				) {
					item(span = { GridItemSpan(colCount) }) {
						head()
					}

					items(count = album.images.size, key = { it }) {

						val image = album.images[it]

						val description = stringResource(R.string.album_image_default_text, it + 1)

						NetImage(
							modifier = Modifier
                                .combinedClickableWithHaptics(
                                    onClick = { itemClickHandler(it) },
                                    onLongClick = { itemLongClickListener(it) }
                                )
                                .semantics {
                                    role = Role.Image
                                    contentDescription = description
                                },
							image = image.run {
								bigSquare ?: preview ?: original
							},
							cropToAspect = 1f.takeIf { prefs.albumGridCropToSquare.value },
							showVideoPlayOverlay = (image.mediaType == ImageInfo.MediaType.VIDEO
									|| image.mediaType == ImageInfo.MediaType.GIF
									|| image.isAnimated == true)
						)
					}
				}

				object : TopBarScrollObservable {
					override val firstVisibleItemIndex: Int
						get() = state.firstVisibleItemIndex
					override val firstVisibleItemOffset: Int
						get() = state.firstVisibleItemScrollOffset
				}
			}
		}

		val insets = WindowInsets.systemBars.asPaddingValues().override(bottom = 0.dp)

		val topBarShadow by with(LocalDensity.current) {
			remember(scrollState) {
				derivedStateOf {
					if (scrollState.firstVisibleItemIndex > 0) {
						10.dp
					} else {
						min(10f, scrollState.firstVisibleItemOffset.toDp().value / 10f).dp
					}
				}
			}
		}

		// Top bar
		Row(
			modifier = Modifier
                .shadow(topBarShadow)
                .background(theme.postCard.listBackgroundColor)
                .padding(insets)
                .padding(horizontal = 6.dp)
                .height(topBarHeight)
                .fillMaxWidth(),
			verticalAlignment = Alignment.CenterVertically,
		) {

			RRIconButton(
				onClick = { onBackPressed() },
				icon = R.drawable.ic_action_back_dark,
				contentDescription = R.string.action_back,
				tint = theme.album.toolbarIconColor
			)

			Box(
				modifier = Modifier
                    .weight(1f)
                    .padding(start = 6.dp),
			) {
				androidx.compose.animation.AnimatedVisibility(
					modifier = Modifier.fillMaxWidth(),
					visible = prefs.albumCompactTitle.value,
					enter = fadeIn() + slideInVertically { -it },
					exit = fadeOut() + slideOutVertically { -it }
				) {
					theme.album.titleCompact.StyledText(
						modifier = Modifier
                            .focusRequester(accessibilityFocusRequester)
                            .focusable(true)
                            .semantics {
                                heading()
                            },
						text = album.title ?: stringResource(R.string.image_gallery),
						overflow = TextOverflow.Ellipsis,
						maxLines = 1
					)
				}
			}

			AlbumSettingsButton()
		}
	}
}

@Composable
fun AlbumSettingsButton(
	modifier: Modifier = Modifier
) {
	val prefs = LocalComposePrefs.current
	val launch = LocalLauncher.current

	RRDropdownMenuIconButton(
		modifier = modifier,
		icon = R.drawable.ic_settings_dark,
		contentDescription = R.string.options_settings,
	) {
		ItemPrefRadio(pref = prefs.albumViewMode) {
			Option(
				value = AlbumViewMode.Cards,
				text = R.string.album_card_view,
				icon = R.drawable.cards_variant,
			)
			Option(
				value = AlbumViewMode.List,
				text = R.string.album_list_view,
				icon = R.drawable.view_list,
			)
			Option(
				value = AlbumViewMode.Grid,
				text = R.string.album_grid_view,
				icon = R.drawable.view_grid,
			)
		}

		ItemDivider()

		when (prefs.albumViewMode.value) {
			AlbumViewMode.Cards -> {
				ItemPrefBool(
					text = R.string.album_card_pref_buttons,
					pref = prefs.albumCardShowButtons
				)
			}

			AlbumViewMode.List -> {
				ItemPrefBool(
					text = R.string.album_list_pref_thumbnails,
					pref = prefs.albumListShowThumbnails
				)

				if (prefs.albumListShowThumbnails.value) {
					ItemDivider()
					ItemPrefIntSlider(
						text = R.string.album_list_pref_thumbnail_size,
						pref = prefs.albumListThumbnailSize,
						min = 64,
						max = 256,
						continuous = true
					)
				}

				ItemDivider()
				ItemPrefBool(
					text = R.string.album_list_pref_buttons,
					pref = prefs.albumListShowButtons
				)
			}

			AlbumViewMode.Grid -> {
				ItemPrefBool(
					text = R.string.album_grid_pref_crop_to_square,
					pref = prefs.albumGridCropToSquare
				)

				ItemDivider()

				ItemPrefIntSlider(
					text = R.string.album_grid_pref_column_count,
					pref = prefs.albumGridColumns,
					min = 2,
					max = 5,
					continuous = false
				)
			}
		}

		ItemDivider()

		ItemGroupCollapsible(text = R.string.album_more_options) {

			ItemPrefBool(
				text = R.string.album_pref_compact_title,
				pref = prefs.albumCompactTitle
			)

			if (prefs.albumViewMode.value == AlbumViewMode.Cards) {
				ItemPrefBool(
					text = R.string.album_grid_pref_rounded_corners,
					pref = prefs.albumGridRoundedCorners
				)

				ItemPrefBool(
					text = R.string.album_grid_pref_horizontal_padding,
					pref = prefs.albumGridHorizontalPadding
				)
			}
		}

		ItemDivider()

		Item(
			text = R.string.album_pref_all_settings,
			icon = R.drawable.ic_settings_dark,
			onClick = { launch(Dest.Settings) },
		)
	}
}

class OverridePaddingValues(
	private val parent: PaddingValues,
	private val top: Dp? = null,
	private val bottom: Dp? = null,
	private val left: Dp? = null,
	private val right: Dp? = null,
) : PaddingValues {
	override fun calculateBottomPadding() = bottom ?: parent.calculateBottomPadding()

	override fun calculateLeftPadding(layoutDirection: LayoutDirection) =
		left ?: parent.calculateLeftPadding(layoutDirection)

	override fun calculateRightPadding(layoutDirection: LayoutDirection) =
		right ?: parent.calculateRightPadding(layoutDirection)

	override fun calculateTopPadding() = top ?: parent.calculateTopPadding()
}

fun PaddingValues.override(
	top: Dp? = null,
	bottom: Dp? = null,
	left: Dp? = null,
	right: Dp? = null,
) = OverridePaddingValues(
	parent = this,
	top = top,
	bottom = bottom,
	left = left,
	right = right
)

@Stable
interface TopBarScrollObservable {
	val firstVisibleItemIndex: Int
	val firstVisibleItemOffset: Int
}
