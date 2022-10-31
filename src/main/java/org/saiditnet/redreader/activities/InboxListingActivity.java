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

package org.saiditnet.redreader.activities;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.annotation.Nullable;
import android.support.v7.widget.RecyclerView;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.adapters.GroupedRecyclerViewAdapter;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.common.RRThemeAttributes;
import org.saiditnet.redreader.common.RRTime;
import org.saiditnet.redreader.jsonwrap.JsonBufferedArray;
import org.saiditnet.redreader.jsonwrap.JsonBufferedObject;
import org.saiditnet.redreader.jsonwrap.JsonValue;
import org.saiditnet.redreader.reddit.APIResponseHandler;
import org.saiditnet.redreader.reddit.RedditAPI;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;
import org.saiditnet.redreader.reddit.prepared.RedditParsedComment;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedMessage;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableComment;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.saiditnet.redreader.reddit.things.RedditComment;
import org.saiditnet.redreader.reddit.things.RedditMessage;
import org.saiditnet.redreader.reddit.things.RedditThing;
import org.saiditnet.redreader.views.RedditInboxItemView;
import org.saiditnet.redreader.views.ScrollbarRecyclerViewManager;
import org.saiditnet.redreader.views.liststatus.ErrorView;
import org.saiditnet.redreader.views.liststatus.LoadingView;

import java.net.URI;
import java.util.UUID;

public final class InboxListingActivity extends BaseActivity {

	private static final int OPTIONS_MENU_MARK_ALL_AS_READ = 0;
	private static final int OPTIONS_MENU_SHOW_UNREAD_ONLY = 1;

	private static final String PREF_ONLY_UNREAD = "inbox_only_show_unread";

	private GroupedRecyclerViewAdapter adapter;

	private LoadingView loadingView;
	private LinearLayout notifications;

	private CacheRequest request;

	private boolean isModmail = false;
	private boolean mOnlyShowUnread;

	private RRThemeAttributes mTheme;
	private RedditChangeDataManager mChangeDataManager;

	private final Handler itemHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {
			adapter.appendToGroup(0, (GroupedRecyclerViewAdapter.Item)msg.obj);
		}
	};

	private final class InboxItem extends GroupedRecyclerViewAdapter.Item {

		private final int mListPosition;
		private final RedditRenderableInboxItem mItem;

		private InboxItem(int listPosition, RedditRenderableInboxItem item) {
			this.mListPosition = listPosition;
			this.mItem = item;
		}

		@Override
		public Class getViewType() {
			return RedditInboxItemView.class;
		}

		@Override
		public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup viewGroup) {

			final RedditInboxItemView view = new RedditInboxItemView(InboxListingActivity.this, mTheme);

			final RecyclerView.LayoutParams layoutParams
					= new RecyclerView.LayoutParams(
							ViewGroup.LayoutParams.MATCH_PARENT,
							ViewGroup.LayoutParams.WRAP_CONTENT);
			view.setLayoutParams(layoutParams);

			return new RecyclerView.ViewHolder(view) {};
		}

		@Override
		public void onBindViewHolder(RecyclerView.ViewHolder viewHolder) {
			((RedditInboxItemView)viewHolder.itemView).reset(
					InboxListingActivity.this,
					mChangeDataManager,
					mTheme,
					mItem,
					mListPosition != 0);
		}

		@Override
		public boolean isHidden() {
			return false;
		}
	}

	// TODO load more on scroll to bottom?

	@Override
	public void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		mTheme = new RRThemeAttributes(this);
		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(this).getDefaultAccount());

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		final String title;

		isModmail = getIntent() != null && getIntent().getBooleanExtra("modmail", false);
		mOnlyShowUnread = sharedPreferences.getBoolean(PREF_ONLY_UNREAD, false);

		if(!isModmail) {
			title = getString(R.string.mainmenu_inbox);
		} else {
			title = getString(R.string.mainmenu_modmail);
		}

		setTitle(title);

		final LinearLayout outer = new LinearLayout(this);
		outer.setOrientation(LinearLayout.VERTICAL);

		loadingView = new LoadingView(this, getString(R.string.download_waiting), true, true);

		notifications = new LinearLayout(this);
		notifications.setOrientation(LinearLayout.VERTICAL);
		notifications.addView(loadingView);

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(this, null, false);

		adapter = new GroupedRecyclerViewAdapter(1);
		recyclerViewManager.getRecyclerView().setAdapter(adapter);

		outer.addView(notifications);
		outer.addView(recyclerViewManager.getOuterView());

		makeFirstRequest(this);

		setBaseActivityContentView(outer);
	}

	public void cancel() {
		if(request != null) request.cancel();
	}

	private void makeFirstRequest(final Context context) {

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		final CacheManager cm = CacheManager.getInstance(context);

		final URI url;

		if(!isModmail) {
			if(mOnlyShowUnread) {
				url = Constants.Reddit.getUri("/message/unread.json?mark=true&limit=100");
			}else{
				url = Constants.Reddit.getUri("/message/inbox.json?mark=true&limit=100");
			}
		} else {
			url = Constants.Reddit.getUri("/message/moderator.json?limit=100");
		}

		// TODO parameterise limit
		request = new CacheRequest(url, user, null, Constants.Priority.API_INBOX_LIST, 0,
				DownloadStrategyAlways.INSTANCE, Constants.FileType.INBOX_LIST,
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
				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
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
					AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
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

					int listPosition = 0;

					for(JsonValue child : children) {

						final RedditThing thing = child.asObject(RedditThing.class);

						switch(thing.getKind()) {
							case COMMENT:
								final RedditComment comment = thing.asComment();
								final RedditParsedComment parsedComment = new RedditParsedComment(comment);
								final RedditRenderableComment renderableComment = new RedditRenderableComment(parsedComment, null, -100000, false);
								itemHandler.sendMessage(General.handlerMessage(0, new InboxItem(listPosition, renderableComment)));
								listPosition++;

								break;

							case MESSAGE:
								final RedditPreparedMessage message = new RedditPreparedMessage(
										InboxListingActivity.this, thing.asMessage(), timestamp);
								itemHandler.sendMessage(General.handlerMessage(0, new InboxItem(listPosition, message)));
								listPosition++;

								if(message.src.replies != null && message.src.replies.getType() == JsonValue.TYPE_OBJECT) {

									final JsonBufferedArray replies = message.src.replies.asObject().getObject("data").getArray("children");

									for(JsonValue childMsgValue : replies) {
										final RedditMessage childMsgRaw = childMsgValue.asObject(RedditThing.class).asMessage();
										final RedditPreparedMessage childMsg = new RedditPreparedMessage(InboxListingActivity.this, childMsgRaw, timestamp);
										itemHandler.sendMessage(General.handlerMessage(0, new InboxItem(listPosition, childMsg)));
										listPosition++;
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
		menu.add(0, OPTIONS_MENU_SHOW_UNREAD_ONLY, 1, R.string.inbox_unread_only);
		menu.getItem(1).setCheckable(true);
		if(mOnlyShowUnread){
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
							protected void onSuccess(@Nullable final String redirectUrl) {
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
								AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										General.showResultDialog(InboxListingActivity.this, error);
									}
								});
							}

							@Override
							protected void onFailure(final APIFailureType type) {

								final RRError error = General.getGeneralErrorForFailure(context, type);
								AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										General.showResultDialog(InboxListingActivity.this, error);
									}
								});
							}
						},
						RedditAccountManager.getInstance(this).getDefaultAccount(),
						this);

				return true;

			case OPTIONS_MENU_SHOW_UNREAD_ONLY: {

				final boolean enabled = !item.isChecked();

				item.setChecked(enabled);
				mOnlyShowUnread = enabled;

				PreferenceManager
						.getDefaultSharedPreferences(this)
						.edit()
						.putBoolean(PREF_ONLY_UNREAD, enabled)
						.apply();

				General.recreateActivityNoAnimation(this);
				return true;
			}

			default:
				return super.onOptionsItemSelected(item);
		}
	}
}
