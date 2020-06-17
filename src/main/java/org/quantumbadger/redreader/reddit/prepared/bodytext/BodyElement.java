package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;

public abstract class BodyElement {

	@NonNull private final BlockType mType;

	protected BodyElement(@NonNull final BlockType type) {
		mType = type;
	}

	@NonNull
	public final BlockType getType() {
		return mType;
	}

	public abstract View generateView(
			@NonNull AppCompatActivity activity,
			@Nullable Integer textColor,
			@Nullable Float textSize,
			boolean showLinkButtons);
}
