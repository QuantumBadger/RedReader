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

package org.saiditnet.redreader.reddit.prepared.markdown;

import java.util.ArrayList;

public final class MarkdownParser {

	public enum MarkdownParagraphType {
		TEXT, CODE, BULLET, NUMBERED, QUOTE, HEADER, HLINE, EMPTY
	}

	public static MarkdownParagraphGroup parse(final char[] raw) {

		final CharArrSubstring[] rawLines = CharArrSubstring.generateFromLines(raw);

		final MarkdownLine[] lines = new MarkdownLine[rawLines.length];

		for(int i = 0; i < rawLines.length; i++) {
			lines[i] = MarkdownLine.generate(rawLines[i]);
		}

		final ArrayList<MarkdownLine> mergedLines = new ArrayList<>(rawLines.length);
		MarkdownLine currentLine = null;

		for(int i = 0; i < lines.length; i++) {

			if(currentLine != null) {

				switch(lines[i].type) {
					case BULLET:
					case NUMBERED:
					case HEADER:
					case CODE:
					case HLINE:
					case QUOTE:

						mergedLines.add(currentLine);
						currentLine = lines[i];
						break;

					case EMPTY:
						mergedLines.add(currentLine);
						currentLine = null;
						break;

					case TEXT:

						if(i < 1) {
							throw new RuntimeException("Internal error: invalid paragrapher state");
						}

						switch(lines[i - 1].type) {
							case QUOTE:
							case BULLET:
							case NUMBERED:
							case TEXT:

								if(lines[i - 1].spacesAtEnd >= 2) {
									mergedLines.add(currentLine);
									currentLine = lines[i];

								} else {
									currentLine = currentLine.rejoin(lines[i]);
								}
								break;

							case CODE:
							case HEADER:
							case HLINE:
								mergedLines.add(currentLine);
								currentLine = lines[i];
								break;
						}

						break;
				}
			} else if(lines[i].type != MarkdownParagraphType.EMPTY) {
				currentLine = lines[i];
			}
		}

		if(currentLine != null) {
			mergedLines.add(currentLine);
		}

		final ArrayList<MarkdownParagraph> outputParagraphs = new ArrayList<>(mergedLines.size());

		for(final MarkdownLine line : mergedLines) {
			final MarkdownParagraph lastParagraph = outputParagraphs.isEmpty() ? null : outputParagraphs.get(outputParagraphs.size() - 1);
			final MarkdownParagraph paragraph = line.tokenize(lastParagraph);
			if(!paragraph.isEmpty()) outputParagraphs.add(paragraph);
		}

		return new MarkdownParagraphGroup(outputParagraphs.toArray(new MarkdownParagraph[outputParagraphs.size()]));
	}
}
