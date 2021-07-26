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

package org.quantumbadger.redreader.reddit.url;

import android.net.Uri;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class ComposeMessageURL extends RedditURLParser.RedditURL {

	public final String recipient;
	public final String subject;
	public final String message;

	public ComposeMessageURL(final String recipient, final String subject, final String message) {
		this.recipient = recipient;
		this.subject = subject;
		this.message = message;
	}

	public static ComposeMessageURL parse(final Uri uri) {

		final String[] pathSegments;
		{
			final List<String> pathSegmentsList = uri.getPathSegments();

			final ArrayList<String> pathSegmentsFiltered = new ArrayList<>(
					pathSegmentsList.size());
			for(String segment : pathSegmentsList) {

				while(StringUtils.asciiLowercase(segment).endsWith(".json")
						|| StringUtils.asciiLowercase(segment).endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				if(!segment.isEmpty()) {
					pathSegmentsFiltered.add(segment);
				}
			}

			pathSegments
					= pathSegmentsFiltered.toArray(new String[0]);
		}

		if(pathSegments.length != 2) {
			return null;
		}

		if(!pathSegments[0].equalsIgnoreCase("message")
				|| !pathSegments[1].equalsIgnoreCase("compose")) {
			return null;
		}

		String recipient = null;
		String subject = null;
		String message = null;
		for(final String parameterKey : General.getUriQueryParameterNames(uri)) {
			if(parameterKey.equalsIgnoreCase("to")) {
				// TODO validate username with regex
				recipient = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("subject")) {
				subject = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("message")) {
				message = uri.getQueryParameter(parameterKey);
			}
		}

		return new ComposeMessageURL(recipient, subject, message);
	}

	@Override
	public Uri generateJsonUri() {
		final Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getDomain());

		builder.appendEncodedPath("message");
		builder.appendEncodedPath("compose");

		if(recipient != null) {
			builder.appendQueryParameter("to", recipient);
		}

		if(subject != null) {
			builder.appendQueryParameter("subject", subject);
		}

		if(message != null) {
			builder.appendQueryParameter("message", message);
		}

		builder.appendEncodedPath(".json");

		return builder.build();
	}

	@Override
	public @RedditURLParser.PathType
	int pathType() {
		return RedditURLParser.COMPOSE_MESSAGE_URL;
	}
}
