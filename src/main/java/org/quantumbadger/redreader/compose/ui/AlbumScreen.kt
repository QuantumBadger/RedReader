package org.quantumbadger.redreader.compose.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.image.AlbumInfo

@Composable
fun AlbumScreen(
	album: AlbumInfo
) {
	val prefs = LocalComposePrefs.current
	val theme = LocalComposeTheme.current

	LazyColumn(
		Modifier
			.fillMaxSize()
			.background(theme.postCard.listBackgroundColor)
	) {

		item {
			Column {
				Text(text = album.title ?: "Album") // TODO string, style
				Text(text = album.description ?: "${album.images.size} image(s)") // TODO string, style, "s"
			}
		}

		items(count = album.images.size) {
			AlbumCard(index = it, image = album.images[it])
		}
	}
}
