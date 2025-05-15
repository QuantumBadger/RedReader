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

import androidx.annotation.NonNull;
import org.junit.Assert;
import org.junit.Test;
import org.quantumbadger.redreader.receivers.announcements.SignatureHandler;

import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.ECGenParameterSpec;
import java.util.Arrays;

public class SignatureHandlerTests {

	@NonNull
	public static KeyPair generateKeyPair()
			throws NoSuchAlgorithmException, InvalidAlgorithmParameterException {

		final KeyPairGenerator keyGen = KeyPairGenerator.getInstance("EC");

		keyGen.initialize(new ECGenParameterSpec("secp256r1"), new SecureRandom());

		return keyGen.generateKeyPair();
	}

	@Test
	public void signTest1() throws Exception {

		final KeyPair keyPair = generateKeyPair();

		final byte[] msg = "Hello World".getBytes(StandardCharsets.UTF_8);

		final byte[] payload = SignatureHandler.generateSignedPayload(keyPair.getPrivate(), msg);

		Assert.assertArrayEquals(
				msg,
				SignatureHandler.readAndVerifySignedPayload(keyPair.getPublic(), payload));
	}

	@Test
	public void signTest2() throws Exception {

		final KeyPair keyPair = generateKeyPair();

		final byte[] msg = "Hello World".getBytes(StandardCharsets.UTF_8);

		final byte[] payload = SignatureHandler.generateSignedPayload(keyPair.getPrivate(), msg);

		// The message starts at payload[4]
		Assert.assertArrayEquals(msg, Arrays.copyOfRange(payload, 4, 4 + msg.length));

		// Corrupt the message
		payload[6] += 1;

		Assert.assertThrows(
				SignatureHandler.SignatureInvalidException.class,
				() -> SignatureHandler.readAndVerifySignedPayload(keyPair.getPublic(), payload));
	}

	@Test
	public void signTest3() throws Exception {

		final KeyPair keyPair = generateKeyPair();
		final KeyPair keyPair2 = generateKeyPair();

		final byte[] msg = "Hello World".getBytes(StandardCharsets.UTF_8);

		final byte[] payload = SignatureHandler.generateSignedPayload(keyPair.getPrivate(), msg);

		Assert.assertThrows(
				SignatureHandler.SignatureInvalidException.class,
				() -> SignatureHandler.readAndVerifySignedPayload(keyPair2.getPublic(), payload));
	}

	@Test
	public void keyTest() throws Exception {

		final KeyPair keyPair = generateKeyPair();

		final String pubKeyStr = SignatureHandler.keyToString(keyPair.getPublic());
		final String privKeyStr = SignatureHandler.keyToString(keyPair.getPrivate());

		final byte[] msg = "Testing 123".getBytes(StandardCharsets.UTF_8);

		Assert.assertArrayEquals(msg, SignatureHandler.readAndVerifySignedPayload(
				SignatureHandler.stringToPublicKey(pubKeyStr),
				SignatureHandler.generateSignedPayload(keyPair.getPrivate(), msg)));

		Assert.assertArrayEquals(msg, SignatureHandler.readAndVerifySignedPayload(
				keyPair.getPublic(),
				SignatureHandler.generateSignedPayload(
						SignatureHandler.stringToPrivateKey(privKeyStr),
						msg)));
	}
}
