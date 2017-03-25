package org.quantumbadger.redreader.test.markdown;

import org.junit.Test;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParagraph;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownTokenizer;

/**
 * Created by Marco on 25.03.2017.
 */
public class MarkdownParagraphTest {
	@Test
	public void markdownParagraph(){
		String src = "^^^All ^^^of ^^^this ^^^should ^^^be ^^^superscripted";
		MarkdownParagraph mp = new MarkdownParagraph(MarkdownTokenizerTest.toCAS(src), null, null, MarkdownTokenizer.tokenize(MarkdownTokenizerTest.toCAS(src)).substringAsArray(0), 0, 0);

	}
}
