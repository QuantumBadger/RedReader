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
package org.quantumbadger.redreader.image

import org.xmlpull.v1.XmlPullParser
import org.xmlpull.v1.XmlPullParserFactory

data class MediaInfo(
    val filename: String,
    val width: Int?,
    val height: Int?
)

data class MPDReadResult(
    val video: MediaInfo? = null,
    val audio: MediaInfo? = null
)

private data class MPD(
    val periods: List<Period>
)

private data class Period(
    val adaptationSets: List<AdaptationSet>
)

private data class AdaptationSet(
    val contentType: String?,
    val representations: List<Representation>
)

private data class Representation(
    val id: String?,
    val bandwidth: Int,
    val codecs: String?,
    val width: Int?,
    val height: Int?,
    val audioSamplingRate: String?,
    val baseURL: String?
)

fun parseMPD(mpdContent: String): MPDReadResult {
    val factory = XmlPullParserFactory.newInstance()
    factory.isNamespaceAware = true
    val parser = factory.newPullParser()
    parser.setInput(mpdContent.reader())
    
    val mpd = parseMPDDocument(parser)
    
    // Get first period
    val firstPeriod = mpd.periods.firstOrNull() ?: return MPDReadResult()
    
    // Find video and audio adaptation sets
    val videoAdaptationSet = firstPeriod.adaptationSets.firstOrNull { it.contentType == "video" }
    val audioAdaptationSet = firstPeriod.adaptationSets.firstOrNull { it.contentType == "audio" }
    
    val selectedVideo = videoAdaptationSet?.let { selectVideoRepresentation(it) }
    val selectedAudio = audioAdaptationSet?.let { selectAudioRepresentation(it) }
    
    return MPDReadResult(video = selectedVideo, audio = selectedAudio)
}

private fun parseMPDDocument(parser: XmlPullParser): MPD {
    val periods = mutableListOf<Period>()
    
    var eventType = parser.eventType
    while (eventType != XmlPullParser.END_DOCUMENT) {
        if (eventType == XmlPullParser.START_TAG && parser.name == "Period") {
            periods.add(parsePeriod(parser))
        }
        eventType = parser.next()
    }
    
    return MPD(periods)
}

private fun parsePeriod(parser: XmlPullParser): Period {
    val adaptationSets = mutableListOf<AdaptationSet>()
    val periodDepth = parser.depth
    
    var eventType = parser.next()
    while (eventType != XmlPullParser.END_DOCUMENT) {
        when (eventType) {
            XmlPullParser.START_TAG -> {
                if (parser.name == "AdaptationSet") {
                    adaptationSets.add(parseAdaptationSet(parser))
                }
            }
            XmlPullParser.END_TAG -> {
                if (parser.name == "Period" && parser.depth == periodDepth) {
                    break
                }
            }
        }
        eventType = parser.next()
    }
    
    return Period(adaptationSets)
}

private fun parseAdaptationSet(parser: XmlPullParser): AdaptationSet {
    val contentType = parser.getAttributeValue(null, "contentType")
    val representations = mutableListOf<Representation>()
    val adaptationSetDepth = parser.depth
    
    var eventType = parser.next()
    while (eventType != XmlPullParser.END_DOCUMENT) {
        when (eventType) {
            XmlPullParser.START_TAG -> {
                if (parser.name == "Representation") {
                    representations.add(parseRepresentation(parser))
                }
            }
            XmlPullParser.END_TAG -> {
                if (parser.name == "AdaptationSet" && parser.depth == adaptationSetDepth) {
                    break
                }
            }
        }
        eventType = parser.next()
    }
    
    return AdaptationSet(contentType, representations)
}

private fun parseRepresentation(parser: XmlPullParser): Representation {
    val id = parser.getAttributeValue(null, "id")
    val bandwidth = parser.getAttributeValue(null, "bandwidth")?.toIntOrNull() ?: 0
    val codecs = parser.getAttributeValue(null, "codecs")
    val width = parser.getAttributeValue(null, "width")?.toIntOrNull()
    val height = parser.getAttributeValue(null, "height")?.toIntOrNull()
    val audioSamplingRate = parser.getAttributeValue(null, "audioSamplingRate")
    
    var baseURL: String? = null
    val representationDepth = parser.depth
    
    var eventType = parser.next()
    while (eventType != XmlPullParser.END_DOCUMENT) {
        when (eventType) {
            XmlPullParser.START_TAG -> {
                if (parser.name == "BaseURL") {
                    baseURL = parser.nextText().trim()
                }
            }
            XmlPullParser.END_TAG -> {
                if (parser.name == "Representation" && parser.depth == representationDepth) {
                    break
                }
            }
        }
        eventType = parser.next()
    }
    
    return Representation(id, bandwidth, codecs, width, height, audioSamplingRate, baseURL)
}

private fun selectVideoRepresentation(adaptationSet: AdaptationSet): MediaInfo? {
    val validReps = adaptationSet.representations.filter { it.baseURL != null }
    if (validReps.isEmpty()) return null
    
    val comparator = compareBy<Representation> { rep ->
        // Smallest dimension priority: 480 = 0, 720 = 1, lowest above 480 = 2+, highest below 480 = higher values
        val smallestDim = minOf(rep.width ?: 0, rep.height ?: 0)
        when {
            smallestDim == 480 -> 0
            smallestDim == 720 -> 1
            smallestDim > 480 -> 2 + (smallestDim - 481)  // Prefer lower dimensions above 480
            else -> 10000 - smallestDim  // For dimensions below 480, prefer higher ones
        }
    }.thenByDescending { rep ->
        // Prefer avc1.* codecs
        if (rep.codecs?.startsWith("avc1.") == true) 1 else 0
    }.thenByDescending { rep ->
        // Highest bandwidth wins
        rep.bandwidth
    }
    
    val selected = validReps.sortedWith(comparator).firstOrNull() ?: return null
    
    return MediaInfo(
        filename = selected.baseURL!!,
        width = selected.width,
        height = selected.height
    )
}

private fun selectAudioRepresentation(adaptationSet: AdaptationSet): MediaInfo? {
    val validReps = adaptationSet.representations.filter { it.baseURL != null }
    if (validReps.isEmpty()) return null
    
    // Select highest bandwidth
    val selected = validReps.maxByOrNull { it.bandwidth } ?: return null
    
    return MediaInfo(
        filename = selected.baseURL!!,
        width = null,
        height = null
    )
}
