package org.quantumbadger.redreader.reddit.prepared;

import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class MarkdownParser {

	private static final Pattern
			newlinePattern = Pattern.compile("\\n\\n+"),
			lineStartNumPattern = Pattern.compile("^(([0-9]+)(?:\\. |\\) ?)).*$"),
			lineStartHeaderPattern = Pattern.compile("^((#+) *).*");

	public static MarkdownParagraph[] splitIntoParagraphs(final String rawMarkdown) {

		final String[] rawParagraphs = newlinePattern.split(rawMarkdown);

		// TODO use array
		final LinkedList<MarkdownParagraph> paragraphs = new LinkedList<MarkdownParagraph>();

		for(final String rawParagraph : rawParagraphs) {
			final MarkdownParagraph parent = paragraphs.isEmpty() ? null : paragraphs.getLast();
			final MarkdownParagraph paragraph = MarkdownParagraph.create(rawParagraph, parent);
			if(paragraph != null) paragraphs.addLast(paragraph);
		}

		return paragraphs.toArray(new MarkdownParagraph[paragraphs.size()]);

		// TODO one pass through each raw paragraph, adding raw text to a string builder
		// Record the position of opening */_/**/etc, create span on close
		// Inline code
	}

	private static final class MarkdownParagraph {

		public static enum Type {
			TEXT, CODE, BULLET, NUMBERED, QUOTE, HEADER
		}

		final String raw;
		final MarkdownParagraph parent;
		final Type type;
		final int number;

		public static MarkdownParagraph create(final String rawWithPrefix, final MarkdownParagraph parent) {

			if(rawWithPrefix.trim().length() < 1) return null;

			final char firstChar = rawWithPrefix.charAt(0);

			switch(firstChar) {

				case ' ':
					if(rawWithPrefix.startsWith("    ")) {
						return new MarkdownParagraph(rawWithPrefix.substring(4), parent, Type.CODE);
					}
					break;

				case '>':
					if(rawWithPrefix.charAt(1) == ' ') {
						return new MarkdownParagraph(rawWithPrefix.substring(2), parent, Type.QUOTE);
					} else {
						return new MarkdownParagraph(rawWithPrefix.substring(1), parent, Type.QUOTE);
					}

				case '*':
				case '-':
				case '+':
					if(rawWithPrefix.charAt(1) == ' ') {
						return new MarkdownParagraph(rawWithPrefix.substring(2), parent, Type.BULLET);
					}
					break;

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					final Matcher numMatcher = lineStartNumPattern.matcher(rawWithPrefix);
					if(numMatcher.find()) {

						return new MarkdownParagraph(rawWithPrefix.substring(numMatcher.group(1).length()),
								parent, Type.NUMBERED, Integer.parseInt(numMatcher.group(2)));

					}
					break;

				case '#':
					final Matcher headerMatcher = lineStartHeaderPattern.matcher(rawWithPrefix);
					if(headerMatcher.find()) {

						final String hashes = headerMatcher.group(2);
						final int level = hashes.length();
						final String raw;

						if(rawWithPrefix.endsWith(" " + hashes)) {
							raw = rawWithPrefix.substring(headerMatcher.group(1).length(), rawWithPrefix.length() - level - 1);
						} else {
							raw = rawWithPrefix.substring(headerMatcher.group(1).length());
						}

						return new MarkdownParagraph(raw, parent, Type.HEADER, level);
					}
					break;
			}

			return new MarkdownParagraph(rawWithPrefix, parent, Type.TEXT);
		}


		private MarkdownParagraph(String raw, MarkdownParagraph parent, Type type, int number) {
			this.raw = raw.replace('\n', ' ');
			this.parent = parent;
			this.type = type;
			this.number = number;
		}

		private MarkdownParagraph(String raw, MarkdownParagraph parent, Type type) {
			this(raw, parent, type, 0);
		}
	}

}
