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
package org.quantumbadger.redreader.test.reddit

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.quantumbadger.redreader.image.parseMPD
import org.robolectric.RobolectricTestRunner
import java.io.File

@RunWith(RobolectricTestRunner::class)
class MPDParserTest {
    
    private fun loadMPD(filename: String): String {
        // Assumes test resources are in src/test/resources
        return File("src/test/resources/mpd/$filename").readText()
    }
    
    @Test
    fun testMPD_6201fb9c() {
        val mpdContent = loadMPD("6201fb9c-b427-4540-901e-07abfa502742.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("CMAF_360.mp4", result.video?.filename)
        assertEquals(360, result.video?.width)
        assertEquals(640, result.video?.height)
        
        assertNotNull(result.audio)
        assertEquals("CMAF_AUDIO_128.mp4", result.audio?.filename)
        assertNull(result.audio?.width)
        assertNull(result.audio?.height)
    }
    
    @Test
    fun testMPD_80960438() {
        val mpdContent = loadMPD("80960438-5cf4-4d91-8d42-7537e5c211a9.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("DASH_360.mp4", result.video?.filename)
        assertEquals(352, result.video?.width)
        assertEquals(640, result.video?.height)
        
        assertNotNull(result.audio)
        assertEquals("DASH_AUDIO_128.mp4", result.audio?.filename)
        assertNull(result.audio?.width)
        assertNull(result.audio?.height)
    }
    
    @Test
    fun testMPD_b4505bfe() {
        val mpdContent = loadMPD("b4505bfe-f9ce-445d-abe8-8abda224b119.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("DASH_480.mp4", result.video?.filename)
        assertEquals(480, result.video?.width)
        assertEquals(716, result.video?.height)
        
        assertNull(result.audio)
    }
    
    @Test
    fun testMPD_b870922e() {
        val mpdContent = loadMPD("b870922e-63b5-4481-a76f-04974bc0ae88.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("CMAF_360.mp4", result.video?.filename)
        assertEquals(360, result.video?.width)
        assertEquals(640, result.video?.height)
        
        assertNull(result.audio)
    }
    
    @Test
    fun testMPD_d2c843f0() {
        val mpdContent = loadMPD("d2c843f0-4358-44bb-a6f2-35692af71d71.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("CMAF_480.mp4", result.video?.filename)
        assertEquals(480, result.video?.width)
        assertEquals(854, result.video?.height)
        
        assertNotNull(result.audio)
        assertEquals("CMAF_AUDIO_128.mp4", result.audio?.filename)
        assertNull(result.audio?.width)
        assertNull(result.audio?.height)
    }
    
    @Test
    fun testMPD_e53e0303() {
        val mpdContent = loadMPD("e53e0303-d3c6-4d1b-b042-2a2065e1c94e.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("CMAF_480.mp4", result.video?.filename)
        assertEquals(496, result.video?.width)
        assertEquals(480, result.video?.height)
        
        assertNull(result.audio)
    }
    
    @Test
    fun testMPD_ee9d77e9() {
        val mpdContent = loadMPD("ee9d77e9-bba4-46aa-b473-f6ea1235abee.mpd")
        val result = parseMPD(mpdContent)
        
        assertNotNull(result.video)
        assertEquals("DASH_480.mp4", result.video?.filename)
        assertEquals(480, result.video?.width)
        assertEquals(480, result.video?.height)
        
        assertNotNull(result.audio)
        assertEquals("DASH_AUDIO_128.mp4", result.audio?.filename)
        assertNull(result.audio?.width)
        assertNull(result.audio?.height)
    }

	@Test
	fun testMPD_6c000f2a() {
		val mpdContent = loadMPD("6c000f2a-17ab-4210-9e4d-b2c2ed4b09c6.mpd")
		val result = parseMPD(mpdContent)

		assertNotNull(result.video)
		assertEquals("DASH_480.mp4", result.video?.filename)
		assertEquals(854, result.video?.width)
		assertEquals(480, result.video?.height)

		assertNotNull(result.audio)
		assertEquals("DASH_AUDIO_128.mp4", result.audio?.filename)
		assertNull(result.audio?.width)
		assertNull(result.audio?.height)
	}
}
