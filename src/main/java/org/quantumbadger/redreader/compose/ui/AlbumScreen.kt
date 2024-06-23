package org.quantumbadger.redreader.compose.ui

import androidx.compose.animation.AnimatedContent
import androidx.compose.animation.fadeIn
import androidx.compose.animation.fadeOut
import androidx.compose.animation.togetherWith
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.add
import androidx.compose.foundation.layout.asPaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.staggeredgrid.LazyVerticalStaggeredGrid
import androidx.compose.foundation.lazy.staggeredgrid.StaggeredGridCells
import androidx.compose.foundation.lazy.staggeredgrid.StaggeredGridItemSpan
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.settings.types.AlbumViewMode

// TODO check for unstable composables
// TODO dark themes
// TODO theme progress spinner
// TODO make cards clickable
// TODO compact view/"list view"
// TODO grid view?
// TODO handle long-click
// TODO settings menu
// TODO pref to hide card buttons
// TODO find best reddit preview
// TODO resize image on another thread
// TODO actually hook up the buttons on the card
// TODO go through all todos on this branch
// TODO strings
// TODO screen reader testing
// TODO move the initial loading screen inside Compose
// TODO tidy up AlbumListingActivity2
// TODO gradient at top for status bar/nav bar?
// TODO error view
// TODO set RedditAccountId when fetching
// TODO handle videos in all three views
// TODO action bar on scroll, move head() out of the list
@Composable
fun AlbumScreen(
    album: AlbumInfo
) {
    val prefs = LocalComposePrefs.current
    val theme = LocalComposeTheme.current

    val contentPadding: PaddingValues = WindowInsets(top = 8.dp, bottom = 24.dp)
        .add(WindowInsets.systemBars)
        .asPaddingValues()

    val head: @Composable () -> Unit = {
        Column(
            Modifier
                .padding(horizontal = 14.dp)
                .fillMaxWidth()
        ) {

            Spacer(Modifier.height(6.dp))

            AlbumSettingsButton(Modifier.align(Alignment.End))

            Spacer(Modifier.height(6.dp))

            Text(
                modifier = Modifier
                    .fillMaxWidth()
                    .semantics {
                        contentDescription = album.title?.let { title ->
                            "Image gallery: $title"
                        } ?: "Image gallery" // TODO strings
                    },
                text = album.title ?: "Gallery",  // TODO string
                style = theme.album.title,
            )
            Spacer(Modifier.height(6.dp))

            val s = "s".takeIf { album.images.size != 1 } ?: ""

            Text(
                modifier = Modifier.fillMaxWidth(),
                text = album.description ?: "${album.images.size} image$s", // TODO string
                style = theme.album.subtitle
            )

            Spacer(Modifier.height(16.dp))
        }
    }

    AnimatedContent(
        targetState = prefs.albumViewMode.value,
        transitionSpec = { fadeIn() togetherWith fadeOut() }
    ) { mode ->
        when (mode) {
            AlbumViewMode.Cards -> {
                LazyColumn(
                    Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
                    contentPadding = contentPadding
                ) {
                    item {
                        head()
                    }

                    items(count = album.images.size) {
                        AlbumCard(image = album.images[it])
                    }
                }
            }

            AlbumViewMode.List -> {
                LazyColumn(
                    Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
                    contentPadding = contentPadding
                ) {
                    item {
                        head()
                    }

                    items(count = album.images.size) {
                        AlbumCard(image = album.images[it])
                    }
                }
            }

            AlbumViewMode.Grid -> {
                LazyVerticalStaggeredGrid(
                    // TODO slider
                    columns = StaggeredGridCells.Fixed(2),
                    modifier = Modifier
                        .fillMaxSize()
                        .background(theme.postCard.listBackgroundColor),
                    contentPadding = contentPadding,
                    verticalItemSpacing = 8.dp,
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    item(span = StaggeredGridItemSpan.FullLine) {
                        head()
                    }

                    items(count = album.images.size, key = { it }) {
                        Box() {
                            NetImage(
                                image = album.images[it].run { bigSquare ?: preview ?: original },
                                cropToAspect = 1f.takeUnless { prefs.albumGridStagger.value }
                            )
                        }
                    }
                }
            }
        }
    }
}

@Composable
fun AlbumSettingsButton(
    modifier: Modifier = Modifier
) {
    val prefs = LocalComposePrefs.current

    Box(modifier = modifier) {

        val settingsMenuState = rememberRRDropdownMenuState()

        RRIconButton(
            onClick = { settingsMenuState.expanded = true },
            icon = R.drawable.ic_settings_dark,
            contentDescription = R.string.options_settings,
        )

        RRDropdownMenu(state = settingsMenuState) {

            ItemPrefRadio(pref = prefs.albumViewMode) {
                Option(
                    value = AlbumViewMode.Cards,
                    text = "Card view",
                    icon = R.drawable.cards_variant,
                )
                Option(
                    value = AlbumViewMode.List,
                    text = "List view",
                    icon = R.drawable.view_list,
                )
                Option(
                    value = AlbumViewMode.Grid,
                    text = "Grid view",
                    icon = R.drawable.view_grid,
                )
            }

            ItemDivider()

            when (prefs.albumViewMode.value) {
                AlbumViewMode.Cards -> {
                    ItemPrefBool(
                        text = "Show buttons on cards",
                        pref = prefs.albumCardShowButtons
                    )
                }

                AlbumViewMode.List -> {
                    ItemPrefBool(
                        text = "Show thumbnails",
                        pref = prefs.albumListShowThumbnails
                    )
                }

                AlbumViewMode.Grid -> {
                    ItemPrefBool(
                        text = "Stagger items",
                        pref = prefs.albumGridStagger
                    )
                }
            }


            ItemDivider()

            Item(
                text = "All settings...",
                icon = R.drawable.ic_settings_dark,
                onClick = { /*TODO*/ },
            )
        }
    }
}
