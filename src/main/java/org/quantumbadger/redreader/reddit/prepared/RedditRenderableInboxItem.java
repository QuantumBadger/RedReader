package org.quantumbadger.redreader.reddit.prepared;

import android.support.v7.app.AppCompatActivity;

public interface RedditRenderableInboxItem extends RedditRenderableCommentListItem {

	void handleInboxClick(AppCompatActivity activity);
	void handleInboxLongClick(AppCompatActivity activity);
}
