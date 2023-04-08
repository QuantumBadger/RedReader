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

package org.quantumbadger.redreader.test.announcements;

import org.junit.Assert;
import org.junit.Test;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.receivers.announcements.Announcement;
import org.quantumbadger.redreader.receivers.announcements.Payload;

import java.io.IOException;

public class AnnouncementTests {

	@Test
	public void announcementTest() throws IOException {

		final byte[] payload = Announcement.create(
				"test_id",
				"myTitle",
				"my message",
				"https://my_url",
				TimeDuration.ms(100000)).toPayload().toBytes();

		final TimestampUTC estUntil = TimestampUTC.now().add(TimeDuration.ms(100000));

		final Announcement reinflated = Announcement.fromPayload(Payload.fromBytes(payload));

		Assert.assertEquals("test_id", reinflated.id);
		Assert.assertEquals("myTitle", reinflated.title);
		Assert.assertEquals("my message", reinflated.message);
		Assert.assertEquals("https://my_url", reinflated.url);

		Assert.assertFalse(estUntil.isLessThan(reinflated.showUntil));
		Assert.assertTrue(estUntil.subtract(TimeDuration.secs(1)).isLessThan(reinflated.showUntil));
		Assert.assertFalse(reinflated.isExpired());
	}

	@Test
	public void announcementTestNullMessage() throws IOException {

		final byte[] payload = Announcement.create(
				"test_id",
				"myTitle",
				null,
				"https://my_url",
				TimeDuration.ms(100000)).toPayload().toBytes();

		final TimestampUTC estUntil = TimestampUTC.now().add(TimeDuration.ms(100000));

		final Announcement reinflated = Announcement.fromPayload(Payload.fromBytes(payload));

		Assert.assertEquals("test_id", reinflated.id);
		Assert.assertEquals("myTitle", reinflated.title);
		Assert.assertNull(reinflated.message);
		Assert.assertEquals("https://my_url", reinflated.url);

		Assert.assertFalse(estUntil.isLessThan(reinflated.showUntil));
		Assert.assertTrue(estUntil.subtract(TimeDuration.secs(1)).isLessThan(reinflated.showUntil));
		Assert.assertFalse(reinflated.isExpired());
	}
}
