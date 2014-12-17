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
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.text.ClipboardManager;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ListAdapter;
import com.actionbarsherlock.view.ContextMenu;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.laurencedawson.activetextview.ActiveTextView;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.holoeverywhere.LayoutInflater;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.Fragment;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.ListView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.CommentEditActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.adapters.CommentListingAdapter;
import org.quantumbadger.redreader.adapters.HeaderAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMoreComments;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.prepared.markdown.MarkdownParser;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.things.RedditMoreComments;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.url.CommentListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.*;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;
import org.quantumbadger.redreader.views.liststatus.SpecificCommentThreadView;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.UUID;

public class CommentListingFragment extends Fragment
		implements ActiveTextView.OnLinkClickedListener,
		RedditPostView.PostSelectionListener,
		RedditCommentView.CommentClickListener {

	private RedditURLParser.RedditURL mUrl;
	private UUID session = null;
	private CacheRequest.DownloadType downloadType;
	private CommentListingAdapter commentListAdapter;
	private BaseAdapter outerAdapter;

	private LoadingView loadingView;
	private LinearLayout notifications, listHeaderNotifications, listHeaderPost, listHeaderSelftext, listFooter;
	private ListView lv;

	private CacheRequest request;

	private RedditPreparedPost mPost;

	private float commentFontScale = 1.0f;
	private EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems;

	private Context context;

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		context = activity.getApplicationContext();
	}

	private final Handler commentHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {
			if(isAdded()) {
				final ArrayList<RedditCommentListItem> items = (ArrayList<RedditCommentListItem>) msg.obj;
				commentListAdapter.addItems(items);
			}
		}
	};

	// TODO load more on scroll to bottom?
	public static CommentListingFragment newInstance(final CommentListingURL url, final UUID session, final CacheRequest.DownloadType downloadType) {

		final CommentListingFragment f = new CommentListingFragment();

		final Bundle bundle = new Bundle(3);

		bundle.putString("url", url.generateJsonUri().toString());
		if(session != null) bundle.putString("session", session.toString());
		bundle.putString("downloadType", downloadType.name());

		f.setArguments(bundle);

		return f;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		// TODO load position/etc?
		super.onCreate(savedInstanceState);

		setHasOptionsMenu(true);

		final Bundle arguments = getArguments();

		mUrl = RedditURLParser.parseProbableCommentListing(Uri.parse(arguments.getString("url")));
		// TODO url may be null...

		if(arguments.containsKey("session")) {
			session = UUID.fromString(arguments.getString("session"));
		}

		downloadType = CacheRequest.DownloadType.valueOf(arguments.getString("downloadType"));
	}

	public void handleCommentVisibilityToggle(RedditCommentView view) {

		final boolean isCollapsed = view.handleVisibilityToggle();
		outerAdapter.notifyDataSetChanged();

		if(isCollapsed) {

			final RedditPreparedComment comment = view.getComment();
			final ListAdapter adapter = lv.getAdapter();
			final int count = adapter.getCount();

			for(int i = 0; i < count; i++) {
				if(adapter.getItem(i) == comment) {

					if(i == lv.getFirstVisiblePosition()) {
						lv.smoothScrollToPosition(i);
					}

					break;
				}
			}
		}
	}

	private LinearLayout createVerticalLinearLayout(Context context) {
		final LinearLayout result = new LinearLayout(context);
		result.setOrientation(LinearLayout.VERTICAL);
		return result;
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, final ViewGroup container, final Bundle savedInstanceState) {

		super.onCreateView(inflater, container, savedInstanceState);
		final Context context = container.getContext();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		commentFontScale = PrefsUtility.appearance_fontscale_comments(context, prefs);

		headerItems = PrefsUtility.appearance_comment_header_items(context, prefs);

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(android.widget.LinearLayout.VERTICAL);

		loadingView = new LoadingView(context, context.getString(R.string.download_waiting), true, true);

		notifications = new LinearLayout(context);
		notifications.setOrientation(android.widget.LinearLayout.VERTICAL);

		lv = new ListView(context);

		lv.setSmoothScrollbarEnabled(false);
		lv.setVerticalFadingEdgeEnabled(false);

		final LinearLayout listHeader = createVerticalLinearLayout(context);
		this.listHeaderPost = createVerticalLinearLayout(context);
		this.listHeaderNotifications = createVerticalLinearLayout(context);
		this.listHeaderSelftext = createVerticalLinearLayout(context);

		listHeader.addView(listHeaderPost);
		listHeader.addView(listHeaderNotifications);
		listHeader.addView(listHeaderSelftext);

		listFooter = createVerticalLinearLayout(context);

		lv.addHeaderView(listHeader);
		lv.addFooterView(listFooter, null, false);

		commentListAdapter = new CommentListingAdapter(context, this);
		outerAdapter = commentListAdapter;

		if(mUrl.pathType() == RedditURLParser.PathType.PostCommentListingURL
			&& mUrl.asPostCommentListURL().commentId != null) {

			final SpecificCommentThreadView specificCommentThreadView = new SpecificCommentThreadView(
					getSupportActivity(),
					mUrl.asPostCommentListURL());

			outerAdapter = new HeaderAdapter(specificCommentThreadView, outerAdapter);
		}


		lv.setAdapter(outerAdapter);

		registerForContextMenu(lv);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				if(view instanceof RedditCommentView) {
					onCommentClicked((RedditCommentView) view);

				} else if(view instanceof LoadMoreCommentsView) {
					LinkHandler.onLinkClicked(getSupportActivity(), ((LoadMoreCommentsView)view).getUrl().toString());

				} else if(position == 0 && mPost != null && !mPost.src.is_self) {
					LinkHandler.onLinkClicked(getSupportActivity(), mPost.url, false, mPost.src);
				}
			}
		});

		outer.addView(notifications);
		outer.addView(lv);

		final FrameLayout outerFrame = new FrameLayout(context);
		outerFrame.addView(outer);

		final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(context);

		final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(context, new BezelSwipeOverlay.BezelSwipeListener() {

			public boolean onSwipe(BezelSwipeOverlay.SwipeEdge edge) {

				if(mPost == null) return false;

				toolbarOverlay.setContents(mPost.generateToolbar(getSupportActivity(), true, toolbarOverlay));
				toolbarOverlay.show(edge == BezelSwipeOverlay.SwipeEdge.LEFT ?
						SideToolbarOverlay.SideToolbarPosition.LEFT : SideToolbarOverlay.SideToolbarPosition.RIGHT);
				return true;
			}

			public boolean onTap() {

				if(toolbarOverlay.isShown()) {
					toolbarOverlay.hide();
					return true;
				}

				return false;
			}
		});

		outerFrame.addView(bezelOverlay);
		outerFrame.addView(toolbarOverlay);

		bezelOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
		bezelOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

		toolbarOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
		toolbarOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

		makeFirstRequest(context);

		return outerFrame;
	}

	public void cancel() {
		if(request != null) request.cancel();
	}

	private void makeFirstRequest(final Context context) {

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();
		final CacheManager cm = CacheManager.getInstance(context);

		// TODO parameterise limit
		request = new CacheRequest(General.uriFromString(mUrl.generateJsonUri().toString()), user, session, Constants.Priority.API_COMMENT_LIST, 0, downloadType, Constants.FileType.COMMENT_LIST, true, true, false, context) {

			@Override
			protected void onDownloadNecessary() {
				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						listFooter.addView(loadingView);
						outerAdapter.notifyDataSetChanged();
					}
				});
			}

			@Override
			protected void onDownloadStarted() {
				loadingView.setIndeterminate(context.getString(R.string.download_connecting));
			}

			@Override
			protected void onCallbackException(final Throwable t) {
				request = null;
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {

				request = null;

				if(!isAdded()) return;

				if(loadingView != null) loadingView.setDoneNoAnim(R.string.download_failed);
				final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());

				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						notifications.addView(new ErrorView(getSupportActivity(), error));
					}
				});
			}

			@Override protected void onProgress(final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
				request = null;
			}

			@Override
			public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

				if(isAdded() && loadingView != null) loadingView.setIndeterminate("Downloading...");

				// TODO pref (currently 10 mins)
				if(fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
					General.UI_THREAD_HANDLER.post(new Runnable() {
						public void run() {
							if(isDetached()) return;
							final CachedHeaderView cacheNotif = new CachedHeaderView(
									context,
									context.getString(R.string.listing_cached) + " " + RRTime.formatDateTime(timestamp, context),
									null
							);
							listHeaderNotifications.addView(cacheNotif);
							outerAdapter.notifyDataSetChanged();
						}
					});
				}

				((SessionChangeListener)getSupportActivity()).onSessionChanged(session, SessionChangeListener.SessionChangeType.COMMENTS, timestamp);

				// TODO {"error": 403} is received for unauthorized subreddits

				final boolean showLinkButtons = PrefsUtility.pref_appearance_linkbuttons(
						context, PreferenceManager.getDefaultSharedPreferences(context));

				try {

					// Download main post
					if(value.getType() == JsonValue.Type.ARRAY) {
						// lol, reddit api
						final JsonBufferedArray root = value.asArray();
						final JsonBufferedObject thing = root.get(0).asObject();
						final JsonBufferedObject listing = thing.getObject("data");
						final JsonBufferedArray postContainer = listing.getArray("children");
						final RedditThing postThing = postContainer.getObject(0, RedditThing.class);
						final RedditPost post = postThing.asPost();

						// TODO show upvote/downvote/etc buttons

						CommentListingFragment.this.mPost = new RedditPreparedPost(context, cm, 0, post, timestamp, true, false, false, false, user);

						final ViewGroup selfText;

						if(post.is_self && post.selftext != null && post.selftext.trim().length() > 0) {

							selfText = MarkdownParser.parse(StringEscapeUtils.unescapeHtml4(post.selftext).toCharArray())
									.buildView(getSupportActivity(), null, 14f * commentFontScale, showLinkButtons);

						} else {
							selfText = null;
						}

						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {

								final RedditPostHeaderView postHeader = new RedditPostHeaderView(getSupportActivity(), CommentListingFragment.this.mPost);
								listHeaderPost.addView(postHeader);

								if(selfText != null) {
									selfText.setFocusable(false);
									selfText.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);

									final int paddingPx = General.dpToPixels(context, 10);
									listHeaderSelftext.addView(selfText);
									listHeaderSelftext.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
									listHeaderNotifications.setBackgroundColor(Color.argb(35, 128, 128, 128));
								}

								if(!General.isTablet(context, PreferenceManager.getDefaultSharedPreferences(context))) {
									getSupportActivity().getSupportActionBar().setTitle(StringEscapeUtils.unescapeHtml4(post.title));
								}
							}
						});
					}

					// Download comments

					final JsonBufferedObject thing;

					if(value.getType() == JsonValue.Type.ARRAY) {
						thing = value.asArray().get(1).asObject();
					} else {
						thing = value.asObject();
					}

					final JsonBufferedObject listing = thing.getObject("data");
					final JsonBufferedArray topLevelComments = listing.getArray("children");

					final String parentId;

					switch(CommentListingFragment.this.mUrl.pathType()) {
						case PostCommentListingURL:
							parentId = "t3_" + CommentListingFragment.this.mUrl.asPostCommentListURL().postId;
							break;
						case UserCommentListingURL:
							parentId = "/u/" + CommentListingFragment.this.mUrl.asUserCommentListURL().user + "/comments";
							break;
						default:
							throw new RuntimeException("Unknown url type");
					}

					final HashSet<String> needsChanging = RedditChangeDataManager
							.getInstance(context)
							.getChangedForParent(parentId, user);

					for(final JsonValue commentThingValue : topLevelComments) {
						buildComments(commentThingValue, null, timestamp, needsChanging);
					}

					commentHandler.sendMessage(General.handlerMessage(0, buffer));

				} catch (Throwable t) {
					notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
					return;
				}

				if(isAdded() && loadingView != null) loadingView.setDoneNoAnim(R.string.download_done);
			}

			private ArrayList<RedditCommentListItem> buffer = new ArrayList<RedditCommentListItem>();

			private void buildComments(final JsonValue value, final RedditCommentListItem parent, final long timestamp, final HashSet<String> needsChanging) throws IOException, InterruptedException, IllegalAccessException, java.lang.InstantiationException, NoSuchMethodException, InvocationTargetException {

				final RedditThing commentThing = value.asObject(RedditThing.class);

				final RedditCommentListItem item;
				boolean shouldRecurse = false;

				if(commentThing.getKind() == RedditThing.Kind.MORE_COMMENTS
						&& mUrl.pathType() == RedditURLParser.PathType.PostCommentListingURL) {

					final RedditMoreComments redditMoreComments = commentThing.asMoreComments();
					final RedditPreparedMoreComments preparedMoreComments = new RedditPreparedMoreComments(redditMoreComments, mUrl.asPostCommentListURL());
					item = new RedditCommentListItem(parent, preparedMoreComments);

				} else if(commentThing.getKind() == RedditThing.Kind.COMMENT) {

					final RedditComment comment = commentThing.asComment();
					final RedditPreparedComment preparedComment = new RedditPreparedComment(context, comment,
							timestamp, needsChanging.contains(comment.name), mPost, user, headerItems);
					item = new RedditCommentListItem(parent, preparedComment);

					if(comment.replies.getType() == JsonValue.Type.OBJECT) {
						shouldRecurse = true;
					}

				} else {
					return;
				}

				buffer.add(item);
				if(buffer.size() >= 40) {
					commentHandler.sendMessage(General.handlerMessage(0, buffer));
					buffer = new ArrayList<RedditCommentListItem>();
				}

				if(shouldRecurse) {
					final RedditComment comment = commentThing.asComment();
					final JsonBufferedObject replies = comment.replies.asObject();
					final JsonBufferedArray children = replies.getObject("data").getArray("children");

					for(final JsonValue v : children) {
						buildComments(v, item, timestamp, needsChanging);
					}
				}
			}
		};

		cm.makeRequest(request);
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, android.view.ContextMenu.ContextMenuInfo menuInfo) {

		if (v instanceof ListView) {

			final AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo)menuInfo;

			if(info.position == 0) {

				if(mPost == null) {
					General.quickToast(getSupportActivity(), R.string.error_toast_parent_post_not_downloaded);
					return;
				}

				RedditPreparedPost.showActionMenu(getSupportActivity(), mPost);

			} else {

				final Object item = lv.getAdapter().getItem(info.position);

				if(!(item instanceof RedditCommentListItem)) {
					return;
				}

				if(((RedditCommentListItem)item).isComment()) {

					final RedditPreparedComment comment = ((RedditCommentListItem)item).asComment();
					final RedditAccount user = RedditAccountManager.getInstance(getSupportActivity()).getDefaultAccount();

					if(!user.isAnonymous()) {

						if(!comment.isUpvoted()) {
							menu.add(Menu.NONE, Action.UPVOTE.ordinal(), 0, R.string.action_upvote);
						} else {
							menu.add(Menu.NONE, Action.UNVOTE.ordinal(), 0, R.string.action_upvote_remove);
						}

						if(!comment.isDownvoted()) {
							menu.add(Menu.NONE, Action.DOWNVOTE.ordinal(), 0, R.string.action_downvote);
						} else {
							menu.add(Menu.NONE, Action.UNVOTE.ordinal(), 0, R.string.action_downvote_remove);
						}

						if(comment.isSaved()) {
							menu.add(Menu.NONE, Action.UNSAVE.ordinal(), 0, R.string.action_unsave);
						} else {
							menu.add(Menu.NONE, Action.SAVE.ordinal(), 0, R.string.action_save);
						}

						menu.add(Menu.NONE, Action.REPORT.ordinal(), 0, R.string.action_report);
						menu.add(Menu.NONE, Action.REPLY.ordinal(), 0, R.string.action_reply);

						if(user.username.equalsIgnoreCase(comment.src.author))
							menu.add(Menu.NONE, Action.EDIT.ordinal(), 0, R.string.action_edit);
					}

					menu.add(Menu.NONE, Action.COMMENT_LINKS.ordinal(), 0, R.string.action_comment_links);
					menu.add(Menu.NONE, Action.COLLAPSE.ordinal(), 0, R.string.action_collapse);
					menu.add(Menu.NONE, Action.SHARE.ordinal(), 0, R.string.action_share);
					menu.add(Menu.NONE, Action.COPY.ordinal(), 0, R.string.action_copy);
					menu.add(Menu.NONE, Action.USER_PROFILE.ordinal(), 0, R.string.action_user_profile);
					menu.add(Menu.NONE, Action.PROPERTIES.ordinal(), 0, R.string.action_properties);
				}
			}
		}
	}

	public void onClickUrl(String url) {
		if(url != null) LinkHandler.onLinkClicked(getSupportActivity(), url, false, null);
	}

	public void onClickText(Object attachment) {}

	@Override
	public void onCommentClicked(final RedditCommentView view) {
		switch(PrefsUtility.pref_behaviour_actions_comment_tap(context, PreferenceManager.getDefaultSharedPreferences(context))) {
			case COLLAPSE:
				handleCommentVisibilityToggle(view);
				break;
			case ACTION_MENU:
				openContextMenu(view);
				break;
		}
	}

	private static enum Action {
		UPVOTE, UNVOTE, DOWNVOTE, SAVE, UNSAVE, REPORT, SHARE, COPY, REPLY, USER_PROFILE, COMMENT_LINKS, COLLAPSE, EDIT, PROPERTIES
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {

		final AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo)item.getMenuInfo();

		if(info.position <= 0) {
			return false;
		}

		final Object selectedObject = lv.getAdapter().getItem(info.position);

		if(!(selectedObject instanceof RedditCommentListItem)
				|| !((RedditCommentListItem)selectedObject).isComment()) {
			return false;
		}

		final Action action = Action.values()[item.getItemId()];
		final RedditPreparedComment comment = ((RedditCommentListItem)selectedObject).asComment();

		switch(action) {

			case UPVOTE:
				comment.action(getSupportActivity(), RedditAPI.RedditAction.UPVOTE);
				break;

			case DOWNVOTE:
				comment.action(getSupportActivity(), RedditAPI.RedditAction.DOWNVOTE);
				break;

			case UNVOTE:
				comment.action(getSupportActivity(), RedditAPI.RedditAction.UNVOTE);
				break;

			case SAVE:
				comment.action(getSupportActivity(), RedditAPI.RedditAction.SAVE);
				break;

			case UNSAVE:
				comment.action(getSupportActivity(), RedditAPI.RedditAction.UNSAVE);
				break;

			case REPORT:

				new AlertDialog.Builder(getSupportActivity())
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(R.string.action_report,
								new DialogInterface.OnClickListener() {
									public void onClick(final DialogInterface dialog, final int which) {
										comment.action(getSupportActivity(), RedditAPI.RedditAction.REPORT);
										// TODO update the view to show the result
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case REPLY: {
				final Intent intent = new Intent(getSupportActivity(), CommentReplyActivity.class);
				intent.putExtra("parentIdAndType", comment.idAndType);
				startActivity(intent);
				break;
			}

			case EDIT: {
				final Intent intent = new Intent(getSupportActivity(), CommentEditActivity.class);
				intent.putExtra("commentIdAndType", comment.idAndType);
				intent.putExtra("commentText", StringEscapeUtils.unescapeHtml4(comment.src.body));
				startActivity(intent);
				break;
			}

			case COMMENT_LINKS:
				final HashSet<String> linksInComment = comment.computeAllLinks();

				if(linksInComment.isEmpty()) {
					General.quickToast(getSupportActivity(), R.string.error_toast_no_urls_in_comment);

				} else {

					final String[] linksArr = linksInComment.toArray(new String[linksInComment.size()]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(getSupportActivity());
					builder.setItems(linksArr, new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface dialog, int which) {
							LinkHandler.onLinkClicked(getSupportActivity(), linksArr[which], false);
							dialog.dismiss();
						}
					});

					final AlertDialog alert = builder.create();
					alert.setTitle(R.string.action_comment_links);
					alert.setCanceledOnTouchOutside(true);
					alert.show();
				}

				break;

			case SHARE:

				final Intent mailer = new Intent(Intent.ACTION_SEND);
				mailer.setType("text/plain");
				mailer.putExtra(Intent.EXTRA_SUBJECT, "Comment by " + comment.src.author + " on Reddit");

				// TODO this currently just dumps the markdown
				mailer.putExtra(Intent.EXTRA_TEXT, StringEscapeUtils.unescapeHtml4(comment.src.body));
				startActivityForResult(Intent.createChooser(mailer, context.getString(R.string.action_share)), 1);

				break;

			case COPY:

				ClipboardManager manager = (ClipboardManager) getActivity().getSystemService(Context.CLIPBOARD_SERVICE);
				// TODO this currently just dumps the markdown
				manager.setText(StringEscapeUtils.unescapeHtml4(comment.src.body));
				break;

			case COLLAPSE:
				if(comment.getBoundView() != null) handleCommentVisibilityToggle(comment.getBoundView());
				break;

			case USER_PROFILE:
				LinkHandler.onLinkClicked(getSupportActivity(), new UserProfileURL(comment.src.author).toString());
				break;

			case PROPERTIES:
				CommentPropertiesDialog.newInstance(comment.src).show(getSupportActivity());
				break;

		}

		return true;
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		if(mUrl.pathType() == RedditURLParser.PathType.PostCommentListingURL) {
			menu.add(R.string.action_reply);
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {

		if(item.getTitle().equals(context.getString(R.string.action_reply))) {
			onParentReply();
			return true;
		}

		return false;
	}

	private void onParentReply() {

		if(mPost != null) {
			final Intent intent = new Intent(getSupportActivity(), CommentReplyActivity.class);
			intent.putExtra("parentIdAndType", mPost.idAndType);
			startActivity(intent);

		} else {
			General.quickToast(getSupportActivity(), R.string.error_toast_parent_post_not_downloaded);
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostSelected(post);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostCommentsSelected(post);
	}
}
