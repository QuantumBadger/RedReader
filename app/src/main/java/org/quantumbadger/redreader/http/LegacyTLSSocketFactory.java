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

package org.quantumbadger.redreader.http;

import androidx.annotation.NonNull;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

public class LegacyTLSSocketFactory extends SSLSocketFactory {

	private static final String[] TLS_V1_2_ONLY = {"TLSv1.2"};

	private final SSLSocketFactory delegate;

	public LegacyTLSSocketFactory(@NonNull final SSLSocketFactory base) {
		this.delegate = base;
	}

	@Override
	public String[] getDefaultCipherSuites() {
		return delegate.getDefaultCipherSuites();
	}

	@Override
	public String[] getSupportedCipherSuites() {
		return delegate.getSupportedCipherSuites();
	}

	@Override
	public Socket createSocket(
			final Socket s,
			final String host,
			final int port,
			final boolean autoClose) throws IOException {
		return enableTLS1_2(delegate.createSocket(s, host, port, autoClose));
	}

	@Override
	public Socket createSocket(final String host, final int port) throws IOException {
		return enableTLS1_2(delegate.createSocket(host, port));
	}

	@Override
	public Socket createSocket(
			final String host,
			final int port,
			final InetAddress localHost,
			final int localPort) throws IOException {
		return enableTLS1_2(delegate.createSocket(host, port, localHost, localPort));
	}

	@Override
	public Socket createSocket(
			final InetAddress host,
			final int port) throws IOException {
		return enableTLS1_2(delegate.createSocket(host, port));
	}

	@Override
	public Socket createSocket(
			final InetAddress address,
			final int port,
			final InetAddress localAddress,
			final int localPort) throws IOException {
		return enableTLS1_2(delegate.createSocket(address, port, localAddress, localPort));
	}

	private Socket enableTLS1_2(final Socket s) {
		if (s instanceof SSLSocket) {
			((SSLSocket)s).setEnabledProtocols(TLS_V1_2_ONLY);
		}
		return s;
	}
}
