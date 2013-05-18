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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.view.View;
import android.widget.AdapterView;
import com.laurencedawson.activetextview.ActiveTextView;
import org.apache.http.StatusLine;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.Dialog;
import org.holoeverywhere.app.DialogFragment;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.ListView;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.CommentListingActivity;
import org.quantumbadger.redreader.adapters.InboxListingAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.RedditPreparedInboxItem;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMessage;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.net.URI;
import java.util.EnumSet;
import java.util.UUID;

public final class InboxListingFragment extends DialogFragment implements ActiveTextView.OnLinkClickedListener {

	private InboxListingAdapter adapter;

	private LoadingView loadingView;
	private LinearLayout notifications;

	private CacheRequest request;

	private EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems;

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	private final Handler itemHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {
			if(isAdded()) adapter.addItem((RedditPreparedInboxItem)msg.obj);
		}
	};

	// TODO load more on scroll to bottom?

	public static InboxListingFragment newInstance() {
		return new InboxListingFragment();
	}

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {

		if(alreadyCreated) return getDialog();
		alreadyCreated = true;

		super.onCreateDialog(savedInstanceState);

		final Context context = getSupportActivity();

		headerItems = PrefsUtility.appearance_comment_header_items(context, PreferenceManager.getDefaultSharedPreferences(context));
		headerItems.remove(PrefsUtility.AppearanceCommentHeaderItems.SCORE);
		headerItems.remove(PrefsUtility.AppearanceCommentHeaderItems.UPS_DOWNS);

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(android.widget.LinearLayout.VERTICAL);

		loadingView = new LoadingView(context, getString(R.string.download_waiting), true, true);

		notifications = new LinearLayout(context);
		notifications.setOrientation(android.widget.LinearLayout.VERTICAL);
		notifications.addView(loadingView);

		final ListView lv = new ListView(context);

		lv.setSmoothScrollbarEnabled(false);
		lv.setVerticalFadingEdgeEnabled(false);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				final Object item = lv.getAdapter().getItem(position);

				if(item != null && item instanceof RedditPreparedInboxItem) {
					handleClick((RedditPreparedInboxItem)item);
				}
			}
		});

		adapter = new InboxListingAdapter(context, this);
		lv.setAdapter(adapter);

		registerForContextMenu(lv);

		outer.addView(notifications);
		outer.addView(lv);

		makeFirstRequest(context);

		final AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
		builder.setTitle(R.string.mainmenu_inbox);

		builder.setView(outer);

		return builder.create();
	}

	public void cancel() {
		if(request != null) request.cancel();
	}

	private void makeFirstRequest(final Context context) {

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		final CacheManager cm = CacheManager.getInstance(context);

		final URI url = Constants.Reddit.getUri("/message/inbox.json?mark=true&limit=100");

		// TODO parameterise limit
		request = new CacheRequest(url, user, null, Constants.Priority.API_INBOX_LIST, 0, CacheRequest.DownloadType.FORCE, Constants.FileType.INBOX_LIST, true, true, true, context) {

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
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {

				request = null;

				if(!isAdded()) return;

				if(loadingView != null) loadingView.setDone(R.string.download_failed);

				final RRError error = General.getGeneralErrorForFailure(context, type, t, status);
				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						if(isAdded()) notifications.addView(new ErrorView(getSupportActivity(), error));
					}
				});

				if(t != null) t.printStackTrace();
			}

			@Override protected void onProgress(final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
				request = null;
			}

			@Override
			public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

				if(isAdded() && loadingView != null) loadingView.setIndeterminate(R.string.download_downloading);

				// TODO pref (currently 10 mins)
				// TODO xml
				if(fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
					new Handler(Looper.getMainLooper()).post(new Runnable() {
						public void run() {
							final TextView cacheNotif = new TextView(context);
							cacheNotif.setText(context.getString(R.string.listing_cached) + RRTime.formatDateTime(timestamp, context));
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
								final RedditPreparedComment comment = new RedditPreparedComment(
										getSupportActivity(), thing.asComment(), null, timestamp, false, null, user, headerItems);
								itemHandler.sendMessage(General.handlerMessage(0, comment));

								break;

							case MESSAGE:
								final RedditPreparedMessage message = new RedditPreparedMessage(
										getSupportActivity(), thing.asMessage(), timestamp);
								itemHandler.sendMessage(General.handlerMessage(0, message));
								break;

							default:
								throw new RuntimeException("Unknown item in list.");
						}
					}

				} catch (Throwable t) {
					notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
					return;
				}

				if(isAdded() && loadingView != null) loadingView.setDone(R.string.download_done);
			}
		};

		cm.makeRequest(request);
	}

	public void onClickUrl(String url) {
		if(url != null) LinkHandler.onLinkClicked(getSupportActivity(), url, false);
	}

	public void onClickText(Object attachment) {
		if(attachment != null && attachment instanceof RedditPreparedInboxItem) {
			handleClick((RedditPreparedInboxItem)attachment);
		}
	}

	private void handleClick(RedditPreparedInboxItem item) {
		if(item instanceof RedditPreparedComment) {
			final URI commentContext = Constants.Reddit.getUri(((RedditPreparedComment)item).src.context);

			final Intent intent = new Intent(getSupportActivity(), CommentListingActivity.class);
			intent.setData(Uri.parse(commentContext.toString()));
			startActivity(intent);
		}
	}
}
