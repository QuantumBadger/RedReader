package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

public class MalformedHtmlException extends Exception {

	@NonNull public final String html;
	@Nullable public final Integer charPosition;

	public MalformedHtmlException(
			@NonNull final String message,
			@NonNull final String html,
			@Nullable final Integer charPosition) {

		super(message);
		this.html = html;
		this.charPosition = charPosition;
	}
}
