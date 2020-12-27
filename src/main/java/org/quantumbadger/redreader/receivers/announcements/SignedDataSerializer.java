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

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;

public final class SignedDataSerializer {

	private static final String MARKER_START = "START";
	private static final String MARKER_END = "END";

	private SignedDataSerializer() {}

	@NonNull
	public static String serialize(
			@NonNull final PrivateKey privateKey,
			@NonNull final byte[] data) throws
					NoSuchAlgorithmException,
					InvalidKeyException,
					SignatureException {

		return MARKER_START
				+ HexUtils.toHex(SignatureHandler.generateSignedPayload(privateKey, data))
				+ MARKER_END;
	}

	@NonNull
	public static byte[] deserialize(
			@NonNull final PublicKey publicKey,
			@NonNull final String data) throws
					NoSuchAlgorithmException,
					InvalidKeyException,
					SignatureException,
					IOException,
					SignatureHandler.SignatureInvalidException {

		final int startMarkerIndex = data.indexOf(MARKER_START);
		final int endMarkerIndex = data.indexOf(MARKER_END);

		if(startMarkerIndex == -1) {
			throw new IOException("Start marker not found");
		}

		if(endMarkerIndex == -1) {
			throw new IOException("End marker not found");
		}

		final int start = startMarkerIndex + MARKER_START.length();
		final int length = endMarkerIndex - start;

		if(length < 0) {
			throw new IOException("Negative length");
		}

		final String hexData = data.substring(start, endMarkerIndex);

		final byte[] signedPayload = HexUtils.fromHex(hexData);

		return SignatureHandler.readAndVerifySignedPayload(publicKey, signedPayload);
	}
}
