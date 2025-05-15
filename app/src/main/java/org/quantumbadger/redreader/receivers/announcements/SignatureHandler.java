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

package org.quantumbadger.redreader.receivers.announcements;

import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.HexUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

public final class SignatureHandler {

	private static final String ALG = "EC";
	private static final String SIGNATURE_ALG = "SHA256withECDSA";

	public static class SignatureInvalidException extends Exception {}

	private SignatureHandler() {}

	@NonNull
	public static String keyToString(@NonNull final Key key) {
		return HexUtils.toHex(key.getEncoded());
	}

	@NonNull
	public static PrivateKey stringToPrivateKey(@NonNull final String input)
			throws NoSuchAlgorithmException, IOException, InvalidKeySpecException {

		return KeyFactory.getInstance(ALG)
				.generatePrivate(new PKCS8EncodedKeySpec(HexUtils.fromHex(input)));
	}

	@NonNull
	public static PublicKey stringToPublicKey(@NonNull final String input)
			throws NoSuchAlgorithmException, IOException, InvalidKeySpecException {

		return KeyFactory.getInstance(ALG)
				.generatePublic(new X509EncodedKeySpec(HexUtils.fromHex(input)));
	}

	@NonNull
	private static byte[] sign(@NonNull final PrivateKey privateKey, @NonNull final byte[] message)
			throws NoSuchAlgorithmException, InvalidKeyException, SignatureException {

		final Signature signer = Signature.getInstance(SIGNATURE_ALG);

		signer.initSign(privateKey);
		signer.update(message);

		return signer.sign();
	}

	private static void verify(
			@NonNull final PublicKey publicKey,
			@NonNull final byte[] message,
			@NonNull final byte[] signature) throws
					NoSuchAlgorithmException,
					InvalidKeyException,
					SignatureException,
					SignatureInvalidException {

		final Signature signer = Signature.getInstance(SIGNATURE_ALG);

		signer.initVerify(publicKey);
		signer.update(message);

		if(!signer.verify(signature)) {
			throw new SignatureInvalidException();
		}
	}

	@NonNull
	public static byte[] generateSignedPayload(
			@NonNull final PrivateKey privateKey,
			@NonNull final byte[] message)
					throws NoSuchAlgorithmException, InvalidKeyException, SignatureException {

		final byte[] signature = sign(privateKey, message);

		final ByteArrayOutputStream result = new ByteArrayOutputStream();

		final DataOutputStream dos = new DataOutputStream(result);

		try {
			dos.writeInt(message.length);
			dos.write(message);

			dos.writeInt(signature.length);
			dos.write(signature);

			dos.flush();
			dos.close();

		} catch(final IOException e) {
			throw new RuntimeException(e);
		}

		return result.toByteArray();
	}

	@NonNull
	public static byte[] readAndVerifySignedPayload(
			@NonNull final PublicKey publicKey,
			@NonNull final byte[] payload) throws
					NoSuchAlgorithmException,
					InvalidKeyException,
					SignatureException,
					IOException,
					SignatureInvalidException {

		try(DataInputStream payloadStream
					= new DataInputStream(new ByteArrayInputStream(payload))) {

			final int msgLength = payloadStream.readInt();

			final byte[] msg = new byte[msgLength];
			payloadStream.readFully(msg);

			final int sigLength = payloadStream.readInt();

			final byte[] sig = new byte[sigLength];
			payloadStream.readFully(sig);

			// (any trailing bytes in payloadStream are safely ignored)

			verify(publicKey, msg, sig);

			return msg;
		}
	}
}
