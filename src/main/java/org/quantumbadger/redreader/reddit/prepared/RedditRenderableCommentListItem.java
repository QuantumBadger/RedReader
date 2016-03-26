package org.quantumbadger.redreader.reddit.prepared;

import android.content.Context;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import org.quantumbadger.redreader.common.RRThemeAttributes;

public interface RedditRenderableCommentListItem {

	CharSequence getHeader(
			final RRThemeAttributes theme,
			final RedditChangeDataManagerVolatile changeDataManager,
			final Context context);

	View getBody(
			final AppCompatActivity activity,
			final Integer textColor,
			final Float textSize,
			final boolean showLinkButtons);
}
