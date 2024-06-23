package org.quantumbadger.redreader.compose.prefs

import android.content.Context
import androidx.annotation.StringRes
import androidx.compose.runtime.Stable
import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.staticCompositionLocalOf
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.settings.types.AlbumViewMode
import org.quantumbadger.redreader.settings.types.SettingSerializer

@Stable
interface Preference<T> {
    var value: T
}

interface ComposePrefs {
    val appearanceFontScaleGlobal: Float
    val appearanceFontScaleBodyText: Float
    val appearanceFontScalePosts: Float
    val appearanceFontScalePostSubtitles: Float

    val albumViewMode: Preference<AlbumViewMode>
    val albumCardShowButtons: Preference<Boolean>
    val albumListShowThumbnails: Preference<Boolean>
    val albumGridStagger: Preference<Boolean>
    val albumGridColumns: Preference<Float>
}

object ComposePrefsSingleton {
    private lateinit var SINGLETON: ComposePrefs

    fun init(context: Context) {
        General.checkThisIsUIThread()
        SINGLETON = ComposePrefsImpl(context)
    }

    val instance: ComposePrefs
        get() = SINGLETON
}

@Suppress("PrivatePropertyName")
private class ComposePrefsImpl(private val context: Context) : ComposePrefs {

    private val sharedPrefs = General.getSharedPrefs(context)

    private val changeObservers = HashMap<String, () -> Unit>()

    private val appearance_fontscale_global =
        FloatPref(R.string.pref_appearance_fontscale_global_key, 1f)
    private val appearance_fontscale_bodytext =
        FloatPref(R.string.pref_appearance_fontscale_bodytext_key, -1f)
    private val appearance_fontscale_posts =
        FloatPref(R.string.pref_appearance_fontscale_posts_key, -1f)
    private val appearance_fontscale_post_subtitles =
        FloatPref(R.string.pref_appearance_fontscale_post_subtitles_key, -1f)

    init {
        General.checkThisIsUIThread()
        sharedPrefs.registerOnSharedPreferenceChangeListener { prefs, key ->
            changeObservers[key]?.invoke()
        }
    }

    private inner class FloatPref(
        private val key: String,
        private val default: Float
    ) : Preference<Float> {

        constructor(@StringRes key: Int, default: Float) : this(context.getString(key), default)

        private val mutableState = mutableFloatStateOf(loadPref())

        init {
            changeObservers[key] = {
                mutableState.floatValue = loadPref()
            }
        }

        private fun loadPref() =
            sharedPrefs.getString(key, default.toString())?.toFloatOrNull() ?: default

        override var value: Float
            get() = mutableState.floatValue
            set(value) {
                sharedPrefs.edit().putFloat(key, value).apply()
                mutableState.floatValue = value
            }
    }

    private inner class BoolPref(
        private val key: String,
        private val default: Boolean
    ) : Preference<Boolean> {

        constructor(@StringRes key: Int, default: Boolean) : this(context.getString(key), default)

        private val mutableState = mutableStateOf(loadPref())

        init {
            changeObservers[key] = {
                mutableState.value = loadPref()
            }
        }

        private fun loadPref() = sharedPrefs.getBoolean(key, default)

        override var value: Boolean
            get() = mutableState.value
            set(value) {
                sharedPrefs.edit().putBoolean(key, value).apply()
                mutableState.value = value
            }
    }

    private inner class EnumPref<T>(
        private val key: String,
        private val default: T,
        private val serializer: SettingSerializer<T>
    ) : Preference<T> {

        private val mutableState = mutableStateOf(loadPref())

        init {
            changeObservers[key] = {
                mutableState.value = loadPref()
            }
        }

        private fun loadPref() =
            sharedPrefs.getString(key, serializer.serialize(default))?.let(serializer::deserialize)
                ?: default

        override var value: T
            get() = mutableState.value
            set(value) {
                sharedPrefs.edit().putString(key, serializer.serialize(value)).apply()
                mutableState.value = value
            }
    }

    private fun fontScale(pref: FloatPref) = pref.value.takeUnless { it == -1f }
        ?: appearance_fontscale_global.value

    override val appearanceFontScaleGlobal: Float
        get() = appearance_fontscale_global.value

    override val appearanceFontScaleBodyText: Float
        get() = fontScale(appearance_fontscale_bodytext)

    override val appearanceFontScalePosts: Float
        get() = fontScale(appearance_fontscale_posts)

    override val appearanceFontScalePostSubtitles: Float
        get() = fontScale(appearance_fontscale_post_subtitles)

    override val albumViewMode: Preference<AlbumViewMode> = EnumPref(
        "album_view_mode",
        AlbumViewMode.Cards,
        AlbumViewMode.settingSerializer
    )

    override val albumCardShowButtons: Preference<Boolean> = BoolPref(
        "album_card_show_buttons",
        true
    )

    override val albumListShowThumbnails: Preference<Boolean> = BoolPref(
        "album_list_show_thumbnails",
        true
    )

    override val albumGridStagger: Preference<Boolean> = BoolPref(
        "album_grid_stagger",
        false
    )

    override val albumGridColumns: Preference<Float> = FloatPref(
        "album_grid_columns",
        2f
    )
}

val LocalComposePrefs = staticCompositionLocalOf { ComposePrefsSingleton.instance }
