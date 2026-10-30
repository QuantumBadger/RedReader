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

package org.quantumbadger.redreader.reddit;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// Detects comments that are just a reaction GIF, optionally with a few words
public final class ReactionGifFilter {

	public static final String DEFAULT_DOMAINS
			= "giphy.com, gph.is, tenor.com";

	// Comments with fewer than this many words (outside of links) can be hidden
	private static final int MAX_EXTRA_WORDS = 4;

	// ![label](target), as used by Reddit's GIF picker: ![gif](giphy|abc123|downsized)
	private static final Pattern MARKDOWN_IMAGE
			= Pattern.compile("!\\[([^\\]]*)\\]\\(([^)\\s]*)\\)");

	// [text](target)
	private static final Pattern MARKDOWN_LINK
			= Pattern.compile("\\[([^\\]]*)\\]\\(([^)\\s]*)\\)");

	private static final Pattern BARE_URL
			= Pattern.compile("(?i)\\b(?:https?://|www\\.)[^\\s<>()\\[\\]]+");

	private static final Pattern HOST
			= Pattern.compile("(?i)^(?:https?://)?([^/?#:]+)");

	private static final Pattern WORD
			= Pattern.compile("[\\p{L}\\p{N}]+(?:['’][\\p{L}\\p{N}]+)*");

	@NonNull private final List<String> mDomains;

	public ReactionGifFilter(@Nullable final String domainList) {

		if(domainList == null) {
			mDomains = Collections.emptyList();
			return;
		}

		final ArrayList<String> domains = new ArrayList<>();

		for(final String raw : domainList.split("[,\\s]+")) {
			String domain = raw.trim().toLowerCase(Locale.ROOT);

			if(domain.startsWith("*.")) {
				domain = domain.substring(2);
			} else if(domain.startsWith(".")) {
				domain = domain.substring(1);
			}

			if(!domain.isEmpty()) {
				domains.add(domain);
			}
		}

		mDomains = domains;
	}

	public boolean isReactionGif(@Nullable final String markdownBody) {

		if(markdownBody == null) {
			return false;
		}

		final int[] gifLinks = {0};
		final boolean[] otherLinks = {false};

		final LinkCallback onLink = target -> {
			if(isGifTarget(target)) {
				gifLinks[0]++;
			} else {
				otherLinks[0] = true;
			}
		};

		String text = markdownBody;

		// Images: drop the label too, it's never real text ("gif", "img")
		text = replaceAll(MARKDOWN_IMAGE, text, m -> {
			onLink.found(m.group(2));
			return " ";
		});

		// Links: keep the link text, it counts towards the word limit
		text = replaceAll(MARKDOWN_LINK, text, m -> {
			onLink.found(m.group(2));
			return " " + m.group(1) + " ";
		});

		text = replaceAll(BARE_URL, text, m -> {
			onLink.found(m.group());
			return " ";
		});

		if(gifLinks[0] == 0 || otherLinks[0]) {
			return false;
		}

		return countWords(text) <= MAX_EXTRA_WORDS;
	}

	private boolean isGifTarget(@NonNull final String rawTarget) {

		final String target = rawTarget.trim().toLowerCase(Locale.ROOT);

		// Reddit's GIF picker
		if(target.startsWith("giphy|")) {
			return true;
		}

		final String path = target.replaceFirst("[?#].*$", "");

		if(path.endsWith(".gif") || path.endsWith(".gifv")) {
			return true;
		}

		final Matcher hostMatcher = HOST.matcher(target);

		if(!hostMatcher.find()) {
			return false;
		}

		final String host = hostMatcher.group(1);

		for(final String domain : mDomains) {
			if(host.equals(domain) || host.endsWith("." + domain)) {
				return true;
			}
		}

		return false;
	}

	private static int countWords(@NonNull final String text) {

		final Matcher matcher = WORD.matcher(text);
		int count = 0;

		while(matcher.find()) {
			count++;
		}

		return count;
	}

	private interface LinkCallback {
		void found(String target);
	}

	private interface Replacement {
		String replace(Matcher matcher);
	}

	@NonNull
	private static String replaceAll(
			@NonNull final Pattern pattern,
			@NonNull final String input,
			@NonNull final Replacement replacement) {

		final Matcher matcher = pattern.matcher(input);
		final StringBuilder result = new StringBuilder(input.length());
		int last = 0;

		while(matcher.find()) {
			result.append(input, last, matcher.start());
			result.append(replacement.replace(matcher));
			last = matcher.end();
		}

		result.append(input, last, input.length());
		return result.toString();
	}
}
