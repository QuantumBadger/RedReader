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
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMessage;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditMessage;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.views.RedditInboxItemView;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.net.URI;
import java.util.UUID;

public final class InboxListingActivity extends BaseActivity {

	private static final int OPTIONS_MENU_MARK_ALL_AS_READ = 0;
	private static final int OPTIONS_MENU_SHOW_UNREAD_ONLY = 1;

	public enum InboxType {
		INBOX, SENT, MODMAIL
	}

	private static final String PREF_ONLY_UNREAD = "inbox_only_show_unread";

	private GroupedRecyclerViewAdapter adapter;

	private LoadingView loadingView;
	private LinearLayout notifications;

	private CacheRequest request;

	private InboxType inboxType;
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

		private InboxItem(final int listPosition, final RedditRenderableInboxItem item) {
			this.mListPosition = listPosition;
			this.mItem = item;
		}

		@Override
		public Class getViewType() {
			return RedditInboxItemView.class;
		}

		@Override
		public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {

			final RedditInboxItemView view
					= new RedditInboxItemView(InboxListingActivity.this, mTheme);

			final RecyclerView.LayoutParams layoutParams
					= new RecyclerView.LayoutParams(
							ViewGroup.LayoutParams.MATCH_PARENT,
							ViewGroup.LayoutParams.WRAP_CONTENT);
			view.setLayoutParams(layoutParams);

			return new RecyclerView.ViewHolder(view) {
			};
		}

		@Override
		public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {
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
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		mTheme = new RRThemeAttributes(this);
		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(this).getDefaultAccount());

		final SharedPrefsWrapper sharedPreferences
				= General.getSharedPrefs(this);
		final String title;

		if(getIntent() != null) {
			final String inboxTypeString = getIntent().getStringExtra("inboxType");

			if(inboxTypeString != null) {
				inboxType = InboxType.valueOf(StringUtils.asciiUppercase(inboxTypeString));
			} else {
				inboxType = InboxType.INBOX;
			}
		} else {
			inboxType = InboxType.INBOX;
		}

		mOnlyShowUnread = sharedPreferences.getBoolean(PREF_ONLY_UNREAD, false);

		switch(inboxType) {
			case SENT:
				title = getString(R.string.mainmenu_sent_messages);
				break;
			case MODMAIL:
				title = getString(R.string.mainmenu_modmail);
				break;
			default:
				title = getString(R.string.mainmenu_inbox);
				break;
		}

		setTitle(title);

		final LinearLayout outer = new LinearLayout(this);
		outer.setOrientation(LinearLayout.VERTICAL);

		loadingView = new LoadingView(
				this,
				getString(R.string.download_waiting),
				true,
				true);

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

		setBaseActivityListing(outer);
	}

	public void cancel() {
		if(request != null) {
			request.cancel();
		}
	}

	private void makeFirstRequest(final Context context) {

		final RedditAccount user = RedditAccountManager.getInstance(context)
				.getDefaultAccount();
		final CacheManager cm = CacheManager.getInstance(context);

		final URI url;

		if(inboxType == InboxType.SENT) {
			url = Constants.Reddit.getUri("/message/sent.json?limit=100");
		} else if(inboxType == InboxType.MODMAIL) {
			url = Constants.Reddit.getUri("/message/moderator.json?limit=100");
		} else {
			if(mOnlyShowUnread) {
				url = Constants.Reddit.getUri("/message/unread.json?mark=true&limit=100");
			} else {
				url = Constants.Reddit.getUri("/message/inbox.json?mark=true&limit=100");
			}
		}

		// TODO parameterise limit
		request = new CacheRequest(
				url,
				user,
				null,
				new Priority(Constants.Priority.API_INBOX_LIST),
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.INBOX_LIST,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				false,
				context,
				new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						if(loadingView != null) {
							loadingView.setIndeterminate(R.string.download_downloading);
						}

						// TODO pref (currently 10 mins)
						// TODO xml
						if(fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
							AndroidCommon.UI_THREAD_HANDLER.post(() -> {
								final TextView cacheNotif = new TextView(context);
								cacheNotif.setText(context.getString(
										R.string.listing_cached,
										RRTime.formatDateTime(timestamp, context)));
								final int paddingPx = General.dpToPixels(context, 6);
								final int sidePaddingPx = General.dpToPixels(context, 10);
								cacheNotif.setPadding(
										sidePaddingPx,
										paddingPx,
										sidePaddingPx,
										paddingPx);
								cacheNotif.setTextSize(13f);
								notifications.addView(cacheNotif);
							});
						}

						// TODO {"error": 403} is received for unauthorized subreddits

						try {
							final JsonObject root = result.asObject();
							final JsonObject data = root.getObject("data");
							final JsonArray children = data.getArray("children");

							int listPosition = 0;

							for(final JsonValue child : children) {

								final RedditThing thing = child.asObject(RedditThing.class);

								switch(thing.getKind()) {
									case COMMENT:
										final RedditComment comment = thing.asComment();

										final RedditParsedComment parsedComment
												= new RedditParsedComment(
												comment,
												InboxListingActivity.this);

										final RedditRenderableComment renderableComment
												= new RedditRenderableComment(
												parsedComment,
												null,
												-100_000,
												null,
												false,
												true,
												true);

										itemHandler.sendMessage(General.handlerMessage(
												0,
												new InboxItem(listPosition, renderableComment)));

										listPosition++;

										break;

									case MESSAGE:
										final RedditPreparedMessage message
												= new RedditPreparedMessage(
												InboxListingActivity.this,
												thing.asMessage(),
												timestamp,
												inboxType);
										itemHandler.sendMessage(General.handlerMessage(
												0,
												new InboxItem(listPosition, message)));
										listPosition++;

										if(message.src.replies != null
												&& message.src.replies.asObject() != null) {

											final JsonArray replies
													= message.src.replies.asObject()
													.getObject("data")
													.getArray("children");

											for(final JsonValue childMsgValue : replies) {
												final RedditMessage childMsgRaw
														= childMsgValue.asObject(RedditThing.class)
														.asMessage();
												final RedditPreparedMessage childMsg
														= new RedditPreparedMessage(
														InboxListingActivity.this,
														childMsgRaw,
														timestamp,
														inboxType);
												itemHandler.sendMessage(General.handlerMessage(
														0,
														new InboxItem(listPosition, childMsg)));
												listPosition++;
											}
										}

										break;

									default:
										throw new RuntimeException("Unknown item in list.");
								}
							}

						} catch(final Throwable t) {
							onFailure(
									CacheRequest.REQUEST_FAILURE_PARSE,
									t,
									null,
									"Parse failure",
									Optional.of(new FailedRequestBody(result)));
							return;
						}

						if(loadingView != null) {
							loadingView.setDone(R.string.download_done);
						}
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						request = null;

						if(loadingView != null) {
							loadingView.setDone(R.string.download_failed);
						}

						final RRError error = General.getGeneralErrorForFailure(
								context,
								type,
								t,
								httpStatus,
								url.toString(),
								body);
						AndroidCommon.runOnUiThread(() -> notifications.addView(
								new ErrorView(InboxListingActivity.this, error)));
					}
				}));

		cm.makeRequest(request);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		if(inboxType == InboxType.SENT) {
			return false;
		}

		menu.add(0, OPTIONS_MENU_MARK_ALL_AS_READ, 0, R.string.mark_all_as_read);
		menu.add(0, OPTIONS_MENU_SHOW_UNREAD_ONLY, 1, R.string.inbox_unread_only);
		menu.getItem(1).setCheckable(true);
		if(mOnlyShowUnread) {
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
								General.quickToast(
										context,
										R.string.mark_all_as_read_success);
							}

							@Override
							protected void onCallbackException(final Throwable t) {
								BugReportActivity.addGlobalError(new RRError(
										"Mark all as Read failed",
										"Callback exception",
										true,
										t));
							}

							@Override
							protected void onFailure(
									final @CacheRequest.RequestFailureType int type,
									final Throwable t,
									final Integer status,
									final String readableMessage,
									@NonNull final Optional<FailedRequestBody> response) {

								final RRError error = General.getGeneralErrorForFailure(
										context,
										type,
										t,
										status,
										"Reddit API action: Mark all as Read",
										response);
								General.showResultDialog(
										InboxListingActivity.this,
										error);
							}

							@Override
							protected void onFailure(
									@NonNull final APIFailureType type,
									@Nullable final String debuggingContext,
									@NonNull final Optional<FailedRequestBody> response) {

								final RRError error = General.getGeneralErrorForFailure(
										context,
										type,
										debuggingContext,
										response);
								General.showResultDialog(InboxListingActivity.this, error);
							}
						},
						RedditAccountManager.getInstance(this).getDefaultAccount(),
						this);

				return true;

			case OPTIONS_MENU_SHOW_UNREAD_ONLY: {

				final boolean enabled = !item.isChecked();

				item.setChecked(enabled);
				mOnlyShowUnread = enabled;

				General.getSharedPrefs(this)
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
