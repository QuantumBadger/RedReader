package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

public class BodyElementRRError extends BodyElement {

	@NonNull private final RRError mError;

	public BodyElementRRError(@NonNull final RRError error) {

		super(BlockType.ERROR);
		mError = error;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return new ErrorView(activity, mError);
	}
}
