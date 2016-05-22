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

package org.quantumbadger.redreader.activities;

import android.content.Context;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.InboxListingAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMessage;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditMessage;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.net.URI;
import java.util.UUID;

public final class InboxListingActivity extends BaseActivity {

	private static final int OPTIONS_MENU_MARK_ALL_AS_READ = 0;
	private static final int OPTIONS_MENU_SHOW_UNREAD_ONLY = 1;

	private InboxListingAdapter adapter;
	private SharedPreferences.Editor editor;

	private LoadingView loadingView;
	private LinearLayout notifications;

	private CacheRequest request;

	private boolean isModmail = false;
	private boolean onlyUnread;

	private final Handler itemHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {
			adapter.addItem((RedditRenderableInboxItem)msg.obj);
		}
	};

	// TODO load more on scroll to bottom?

	@Override
	public void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		final RRThemeAttributes theme = new RRThemeAttributes(this);

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		editor = sharedPreferences.edit();
		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences)
				&& PrefsUtility.appearance_theme(this, sharedPreferences) == PrefsUtility.AppearanceTheme.NIGHT;

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

		final String title;

		isModmail = getIntent() != null && getIntent().getBooleanExtra("modmail", false);
		onlyUnread = sharedPreferences.getBoolean("onlyUnread", false);

		if(!isModmail) {
			title = getString(R.string.mainmenu_inbox);
		} else {
			title = getString(R.string.mainmenu_modmail);
		}

		OptionsMenuUtility.fixActionBar(this, title);

		final LinearLayout outer = new LinearLayout(this);
		outer.setOrientation(android.widget.LinearLayout.VERTICAL);

		if(solidblack) {
			outer.setBackgroundColor(Color.BLACK);
		}

		loadingView = new LoadingView(this, getString(R.string.download_waiting), true, true);

		notifications = new LinearLayout(this);
		notifications.setOrientation(android.widget.LinearLayout.VERTICAL);
		notifications.addView(loadingView);

		final ListView lv = new ListView(this);

		lv.setSmoothScrollbarEnabled(false);
		lv.setVerticalFadingEdgeEnabled(false);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				final Object item = lv.getAdapter().getItem(position);

				if(item != null && item instanceof RedditRenderableInboxItem) {
					((RedditRenderableInboxItem)item).handleInboxClick(InboxListingActivity.this);
				}
			}
		});

		adapter = new InboxListingAdapter(this, theme);
		lv.setAdapter(adapter);

		registerForContextMenu(lv);

		outer.addView(notifications);
		outer.addView(lv);

		makeFirstRequest(this);

		setContentView(outer);
	}

	public void cancel() {
		if(request != null) request.cancel();
	}

	private void makeFirstRequest(final Context context) {

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		final CacheManager cm = CacheManager.getInstance(context);

		final URI url;

		if(!isModmail) {
			if(onlyUnread) {
				url = Constants.Reddit.getUri("/message/unread.json?mark=true&limit=100");
			}else{
				url = Constants.Reddit.getUri("/message/inbox.json?mark=true&limit=100");
			}
		} else {
			url = Constants.Reddit.getUri("/message/moderator.json?limit=100");
		}

		// TODO parameterise limit
		request = new CacheRequest(url, user, null, Constants.Priority.API_INBOX_LIST, 0,
				CacheRequest.DOWNLOAD_FORCE, Constants.FileType.INBOX_LIST,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API, true, true, context) {

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onCallbackException(final Throwable t) {
				request = null;
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

				request = null;

				if(loadingView != null) loadingView.setDone(R.string.download_failed);

				final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					public void run() {
						notifications.addView(new ErrorView(InboxListingActivity.this, error));
					}
				});

				if(t != null) t.printStackTrace();
			}

			@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
				request = null;
			}

			@Override
			public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

				if(loadingView != null) loadingView.setIndeterminate(R.string.download_downloading);

				// TODO pref (currently 10 mins)
				// TODO xml
				if(fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						public void run() {
							final TextView cacheNotif = new TextView(context);
							cacheNotif.setText(context.getString(R.string.listing_cached, RRTime.formatDateTime(timestamp, context)));
							final int paddingPx = General.dpToPixels(context, 6);
							final int sidePaddingPx = General.dpToPixels(context, 10);
							cacheNotif.setPadding(sidePaddingPx, paddingPx, sidePaddingPx, paddingPx);
							cacheNotif.setTextSize(13f);
							notifications.addView(cacheNotif);
							adapter.notifyDataSetChanged();
						}
					});
				}

				// TODO {"error": 403} is received for unauthorized subreddits

				try {
					final JsonBufferedObject root = value.asObject();
					final JsonBufferedObject data = root.getObject("data");
					final JsonBufferedArray children = data.getArray("children");

					for(JsonValue child : children) {

						final RedditThing thing = child.asObject(RedditThing.class);

						switch(thing.getKind()) {
							case COMMENT:
								final RedditComment comment = thing.asComment();
								final RedditParsedComment parsedComment = new RedditParsedComment(comment);
								final RedditRenderableComment renderableComment = new RedditRenderableComment(parsedComment, null, -100000, false);
								itemHandler.sendMessage(General.handlerMessage(0, renderableComment));

								break;

							case MESSAGE:
								final RedditPreparedMessage message = new RedditPreparedMessage(
										InboxListingActivity.this, thing.asMessage(), timestamp);
								itemHandler.sendMessage(General.handlerMessage(0, message));

								if(message.src.replies != null && message.src.replies.getType() == JsonValue.TYPE_OBJECT) {

									final JsonBufferedArray replies = message.src.replies.asObject().getObject("data").getArray("children");

									for(JsonValue childMsgValue : replies) {
										final RedditMessage childMsgRaw = childMsgValue.asObject(RedditThing.class).asMessage();
										final RedditPreparedMessage childMsg = new RedditPreparedMessage(InboxListingActivity.this, childMsgRaw, timestamp);
										itemHandler.sendMessage(General.handlerMessage(0, childMsg));
									}
								}

								break;

							default:
								throw new RuntimeException("Unknown item in list.");
						}
					}

				} catch (Throwable t) {
					notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Parse failure");
					return;
				}

				if(loadingView != null) loadingView.setDone(R.string.download_done);
			}
		};

		cm.makeRequest(request);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		menu.add(0, OPTIONS_MENU_MARK_ALL_AS_READ, 0, R.string.mark_all_as_read);
		menu.add(0, OPTIONS_MENU_SHOW_UNREAD_ONLY, 1, "Unread Messages Only");
		menu.getItem(1).setCheckable(true);
		if(onlyUnread){
			menu.getItem(1).setChecked(true);
		}
		return super.onCreateOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case OPTIONS_MENU_MARK_ALL_AS_READ:
				RedditAPI.markAllAsRead(
						CacheManager.getInstance(this),
						new APIResponseHandler.ActionResponseHandler(this) {
							@Override
							protected void onSuccess() {
								General.quickToast(context, R.string.mark_all_as_read_success);
							}

							@Override
							protected void onCallbackException(final Throwable t) {
								BugReportActivity.addGlobalError(new RRError("Mark all as Read failed", "Callback exception", t));
							}

							@Override
							protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
								final RRError error = General.getGeneralErrorForFailure(context, type, t, status,
										"Reddit API action: Mark all as Read");
								AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
									public void run() {
										General.showResultDialog(InboxListingActivity.this, error);
									}
								});
							}

							@Override
							protected void onFailure(final APIFailureType type) {

								final RRError error = General.getGeneralErrorForFailure(context, type);
								AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
									public void run() {
										General.showResultDialog(InboxListingActivity.this, error);
									}
								});
							}
						},
						RedditAccountManager.getInstance(this).getDefaultAccount(),
						this);

				return true;
			case OPTIONS_MENU_SHOW_UNREAD_ONLY:
				if (!item.isChecked()) {
					item.setChecked(true);
					editor.putBoolean("onlyUnread", true);
					onlyUnread = true;
				} else {
					item.setChecked(false);
					editor.putBoolean("onlyUnread", false);
					onlyUnread = false;
				}
				editor.apply();
				makeFirstRequest(this);
				return true;
			case android.R.id.home:
				finish();
				return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}
}
