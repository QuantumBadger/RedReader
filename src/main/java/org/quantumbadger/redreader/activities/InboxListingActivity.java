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
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.kthings.JsonUtils;
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError;
import org.quantumbadger.redreader.reddit.kthings.RedditComment;
import org.quantumbadger.redreader.reddit.kthings.RedditFieldReplies;
import org.quantumbadger.redreader.reddit.kthings.RedditListing;
import org.quantumbadger.redreader.reddit.kthings.RedditMessage;
import org.quantumbadger.redreader.reddit.kthings.RedditThing;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMessage;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableInboxItem;
import org.quantumbadger.redreader.views.RedditInboxItemView;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.io.IOException;
import java.util.ArrayList;
import java.util.UUID;

public final class InboxListingActivity extends ViewsBaseActivity {

	private static final int OPTIONS_MENU_MARK_ALL_AS_READ = 0;
	private static final int OPTIONS_MENU_SHOW_UNREAD_ONLY = 1;
	private static final int OPTIONS_MENU_MARK_INBOX_AS_READ_WHEN_BACK = 2;

	public enum InboxType {
		INBOX, SENT, MODMAIL
	}

	private static final String PREF_ONLY_UNREAD = "inbox_only_show_unread";

	private static final String PREF_MARK_INBOX_READ_BACK = "inbox_mark_as_read_when_back";

	private GroupedRecyclerViewAdapter adapter;

	private LoadingView loadingView;
	private LinearLayout notifications;

	private CacheRequest request;

	private InboxType inboxType;
	private boolean mOnlyShowUnread;

	private boolean mMarkInboxAsReadWhenBack;

	private boolean mUserWantsBack = false;

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

		mMarkInboxAsReadWhenBack = sharedPreferences.getBoolean(PREF_MARK_INBOX_READ_BACK, false);

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

		final UriString url;

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
				new CacheRequestCallbacks() {

					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException>
									streamFactory,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						try {
							final RedditThing rootThing = JsonUtils.INSTANCE
									.decodeRedditThingFromStream(streamFactory.create());

							final RedditListing listing
									= ((RedditThing.Listing)rootThing).getData();

							if(loadingView != null) {
								loadingView.setIndeterminate(R.string.download_downloading);
							}

							// TODO pref (currently 10 mins)
							// TODO xml
							if(fromCache) {
								if (timestamp.elapsed().isGreaterThan(TimeDuration.minutes(10))) {
									AndroidCommon.UI_THREAD_HANDLER.post(() -> {
										final TextView cacheNotif = new TextView(context);
										cacheNotif.setText(context.getString(
												R.string.listing_cached,
												timestamp.format()));
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
							}

							// TODO {"error": 403} is received for unauthorized subreddits

							int listPosition = 0;

							for(final MaybeParseError<RedditThing> maybeThing
									: listing.getChildren()) {

								// TODO show error instead
								final RedditThing thing = maybeThing.ok();

								if(thing instanceof RedditThing.Comment) {
									final RedditComment comment
											= ((RedditThing.Comment) thing).getData();

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

								} else if(thing instanceof RedditThing.Message) {

									final RedditPreparedMessage message
											= new RedditPreparedMessage(
													InboxListingActivity.this,
													((RedditThing.Message) thing).getData(),
											inboxType);

									itemHandler.sendMessage(General.handlerMessage(
											0,
											new InboxItem(listPosition, message)));
									listPosition++;

									if(message.src.getReplies()
											instanceof RedditFieldReplies.Some) {

										// TODO make RedditThing generic (and override data)?

										final ArrayList<MaybeParseError<RedditThing>> replies
												= ((RedditThing.Listing)((RedditFieldReplies.Some)
														message.src.getReplies()).getValue())
																.getData().getChildren();

										for(final MaybeParseError<RedditThing> childMsgValue
												: replies) {

											final RedditMessage childMsgRaw
													= ((RedditThing.Message)childMsgValue.ok())
															.getData();

											final RedditPreparedMessage childMsg
													= new RedditPreparedMessage(
															InboxListingActivity.this,
															childMsgRaw,
													inboxType);

											itemHandler.sendMessage(General.handlerMessage(
													0,
													new InboxItem(listPosition, childMsg)));

											listPosition++;
										}
									}
								} else {
									throw new RuntimeException("Unknown item in list.");
								}
							}

							if(loadingView != null) {
								loadingView.setDone(R.string.download_done);
							}


						} catch(final Exception e) {
							onFailure(General.getGeneralErrorForFailure(
									context,
									CacheRequest.REQUEST_FAILURE_PARSE,
									e,
									null,
									url,
									FailedRequestBody.from(streamFactory)));
						}
					}

					@Override
					public void onFailure(@NonNull final RRError error) {

						request = null;

						if(loadingView != null) {
							loadingView.setDone(R.string.download_failed);
						}

						AndroidCommon.runOnUiThread(() -> notifications.addView(
								new ErrorView(InboxListingActivity.this, error)));
					}
				});

		cm.makeRequest(request);
	}

	private void markInboxAsRead() {
		RedditAPI.markAllAsRead(
				CacheManager.getInstance(this),
				new APIResponseHandler.ActionResponseHandler(this) {
					@Override
					protected void onSuccess() {
						if (!mUserWantsBack) { //Don't annoy the user
							General.quickToast(
									context,
									R.string.mark_all_as_read_success);
						}
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
					protected void onFailure(@NonNull final RRError error) {
						if (!mUserWantsBack) { //Don't annoy the user
							General.showResultDialog(
									InboxListingActivity.this,
									error);
						}
					}
				},
				RedditAccountManager.getInstance(this).getDefaultAccount(),
				this);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) {
			mUserWantsBack = true;
			markInboxAsRead();
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
		menu.add(0, OPTIONS_MENU_MARK_INBOX_AS_READ_WHEN_BACK,
				2, R.string.mark_inbox_as_read_when_back);
		menu.getItem(1).setCheckable(true);
		if(mOnlyShowUnread) {
			menu.getItem(1).setChecked(true);
		}
		menu.getItem(2).setCheckable(true);
		if(mMarkInboxAsReadWhenBack) {
			menu.getItem(2).setChecked(true);
		}
		return super.onCreateOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case OPTIONS_MENU_MARK_ALL_AS_READ:
				markInboxAsRead();
				return true;

			case OPTIONS_MENU_MARK_INBOX_AS_READ_WHEN_BACK: {

				final boolean enabled = !item.isChecked();

				item.setChecked(enabled);
				mMarkInboxAsReadWhenBack = enabled;

				General.getSharedPrefs(this)
						.edit()
						.putBoolean(PREF_MARK_INBOX_READ_BACK, enabled)
						.apply();

				return true;
			}
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
