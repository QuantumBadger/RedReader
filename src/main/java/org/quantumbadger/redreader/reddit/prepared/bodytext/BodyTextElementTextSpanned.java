package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.text.Spanned;

public class BodyTextElementTextSpanned extends BodyTextElement {

	@NonNull private final Spanned mSpanned;

	public BodyTextElementTextSpanned(@NonNull final Spanned spanned) {
		mSpanned = spanned;
	}
}
