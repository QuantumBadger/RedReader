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
import org.quantumbadger.redreader.receivers.announcements.Announcement;
import org.quantumbadger.redreader.receivers.announcements.Payload;
import org.quantumbadger.redreader.receivers.announcements.SignatureHandler;
import org.quantumbadger.redreader.receivers.announcements.SignedDataSerializer;

public class AnnouncementRealKeyTest {

	private static final String PUBLIC_KEY = "3059301306072A8648CE3D020106082A8648CE3D0301070342000"
			+ "4F74D436746282E6080F0EE9FB80DCDCA06667F701A0266F2F14C15C204B6E48414444BD9D0C1170E6B0"
			+ "C257B3DE1AE23F4BA965D8CEB055A3C374DA927415C5D";

	@Test
	public void realKeyTest() throws Exception {

		final String examplePayload = "START0000004901000174000A54657374207469746C65010001750010687"
				+ "47470733A2F2F746573745F75726C0100016D000C54657374206D657373616765020005756E74696"
				+ "C00000176A4FBF7A70000000047304502200A121C81AF17D82C4ED139F6FDA260B14F8C11ACC5648"
				+ "908690C73496837D7AE022100E4343A79003D7B9A5D54B85F98CBD1E2201D1C53676387B141060D7"
				+ "33C47D682END";

		final byte[] payloadBytes = SignedDataSerializer.deserialize(
				SignatureHandler.stringToPublicKey(PUBLIC_KEY),
				examplePayload);

		final Announcement announcement = Announcement.fromPayload(Payload.fromBytes(payloadBytes));

		Assert.assertEquals("Test title", announcement.title);
		Assert.assertEquals("Test message", announcement.message);
		Assert.assertEquals("https://test_url", announcement.url);
		Assert.assertEquals(1609085745063L, announcement.showUntilUtcMillis);
	}
}
