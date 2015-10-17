/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.fragments;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Fragment;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.text.ClipboardManager;
import android.view.*;
import android.widget.*;
import com.laurencedawson.activetextview.ActiveTextView;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.CommentEditActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.activities.MoreCommentsListingActivity;
import org.quantumbadger.redreader.adapters.CommentListingAdapter;
import org.quantumbadger.redreader.adapters.HeaderAdapter;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.reddit.CommentListingRequest;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.*;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;
import org.quantumbadger.redreader.views.liststatus.SpecificCommentThreadView;

import java.util.*;

public class CommentListingFragment extends RRFragment
		implements ActiveTextView.OnLinkClickedListener,
		RedditPostView.PostSelectionListener,
		RedditCommentView.CommentClickListener,
		CommentListingRequest.Listener {

	private RedditAccount mUser;
	private ArrayList<RedditURLParser.RedditURL> mAllUrls;
	private LinkedList<RedditURLParser.RedditURL> mUrlsToDownload;
	private UUID session = null;
	private CacheRequest.DownloadType downloadType;
	private CommentListingAdapter commentListAdapter;
	private BaseAdapter outerAdapter;

	private boolean isCachedNotifShown = false;

	private LoadingView loadingView;
	private boolean loadingViewIsAdded = false;
	private LinearLayout notifications, listHeaderNotifications, listHeaderPost, listHeaderSelftext, listFooter;
	private ListView lv;

	private RedditPreparedPost mPost;

	private float commentFontScale = 1.0f;
	private EnumSet<PrefsUtility.AppearanceCommentHeaderItems> headerItems;
	private boolean mShowLinkButtons;

	public CommentListingFragment(
			final Activity parent,
			final ArrayList<RedditURLParser.RedditURL> urls,
			final UUID session,
			final CacheRequest.DownloadType downloadType) {

		super(parent);

		mAllUrls = urls;

		mUrlsToDownload = new LinkedList<RedditURLParser.RedditURL>(mAllUrls);

		this.session = session;
		this.downloadType = downloadType;

		mUser = RedditAccountManager.getInstance(getActivity()).getDefaultAccount();

		parent.invalidateOptionsMenu();
	}

	public void handleCommentVisibilityToggle(RedditCommentView view) {

		final boolean isCollapsed = view.handleVisibilityToggle();
		outerAdapter.notifyDataSetChanged();

		if(isCollapsed) {

			final RedditPreparedComment comment = view.getComment();

			final ListAdapter adapter = lv.getAdapter();
			final int count = adapter.getCount();

			for(int i = 0; i < count; i++) {

				final Object item = adapter.getItem(i);

				if(item == null || !(item instanceof RedditCommentListItem)) {
					continue;
				}

				RedditCommentListItem cmt = (RedditCommentListItem)item;
				if(cmt.isComment() && cmt.asComment() == comment) {

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
	public View onCreateView() {

		final Context context = getActivity();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		commentFontScale = PrefsUtility.appearance_fontscale_comments(context, prefs);
		headerItems = PrefsUtility.appearance_comment_header_items(context, prefs);
		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, prefs);

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

		commentListAdapter = new CommentListingAdapter(getActivity(), this);
		outerAdapter = commentListAdapter;

		if(!mAllUrls.isEmpty()
			&& mAllUrls.get(0).pathType() == RedditURLParser.PathType.PostCommentListingURL
			&& mAllUrls.get(0).asPostCommentListURL().commentId != null) {

			final SpecificCommentThreadView specificCommentThreadView = new SpecificCommentThreadView(
					getActivity(),
					mAllUrls.get(0).asPostCommentListURL());

			outerAdapter = new HeaderAdapter(specificCommentThreadView, outerAdapter);
		}


		lv.setAdapter(outerAdapter);

		registerForContextMenu(lv);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				if(view instanceof RedditCommentView) {
					onCommentClicked((RedditCommentView) view);

				} else if(view instanceof LoadMoreCommentsView) {

					final ArrayList<String> urls = new ArrayList<String>(16);
					for(PostCommentListingURL url : ((LoadMoreCommentsView) view).getUrls()) {
						urls.add(url.toString());
					}

					final Intent intent = new Intent(context, MoreCommentsListingActivity.class);
					intent.putStringArrayListExtra("urls", urls);
					getActivity().startActivity(intent);

				} else if(position == 0 && mPost != null && !mPost.src.is_self) {
					LinkHandler.onLinkClicked(getActivity(), mPost.url, false, mPost.src);

				} else if(view instanceof SpecificCommentThreadView) {
					final PostCommentListingURL allComments = ((SpecificCommentThreadView)view).getUrl().commentId(null);
					LinkHandler.onLinkClicked(getActivity(), allComments.toString());
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

				toolbarOverlay.setContents(mPost.generateToolbar(getActivity(), true, toolbarOverlay));
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

		makeNextRequest(context);

		return outerFrame;
	}

	private void makeNextRequest(final Context context) {

		if(!mUrlsToDownload.isEmpty()) {
			new CommentListingRequest(
					context,
					headerItems,
					mAllUrls.size() == 1,
					mUrlsToDownload.removeFirst(),
					mUser,
					session,
					downloadType,
					this
			);
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, android.view.ContextMenu.ContextMenuInfo menuInfo) {

		if (v instanceof ListView) {

			final AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo)menuInfo;

			if(info.position == 0) {

				if(mPost == null) {
					General.quickToast(getActivity(), R.string.error_toast_parent_post_not_downloaded);
					return;
				}

				RedditPreparedPost.showActionMenu(getActivity(), mPost);

			} else {

				final Object item = lv.getAdapter().getItem(info.position);

				if(!(item instanceof RedditCommentListItem)) {
					return;
				}

				if(((RedditCommentListItem)item).isComment()) {

					final RedditPreparedComment comment = ((RedditCommentListItem)item).asComment();
					final RedditAccount user = RedditAccountManager.getInstance(getActivity()).getDefaultAccount();

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

						if(user.username.equalsIgnoreCase(comment.src.author)) {
							menu.add(Menu.NONE, Action.EDIT.ordinal(), 0, R.string.action_edit);
							menu.add(Menu.NONE, Action.DELETE.ordinal(), 0, R.string.action_delete);
						}
					}

					menu.add(Menu.NONE, Action.CONTEXT.ordinal(), 0, R.string.action_comment_context);
					menu.add(Menu.NONE, Action.GO_TO_COMMENT.ordinal(), 0, R.string.action_comment_go_to);

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
		if(url != null) LinkHandler.onLinkClicked(getActivity(), url, false, null);
	}

	public void onClickText(Object attachment) {}

	@Override
	public void onCommentClicked(final RedditCommentView view) {
		switch(PrefsUtility.pref_behaviour_actions_comment_tap(
				getActivity(),
				PreferenceManager.getDefaultSharedPreferences(getActivity()))) {

			case COLLAPSE:
				handleCommentVisibilityToggle(view);
				break;
			case ACTION_MENU:
				getActivity().openContextMenu(view);
				break;
		}
	}

	@Override
	public void onCommentListingRequestDownloadNecessary() {
		if(!loadingViewIsAdded) {
			listFooter.addView(loadingView);
			outerAdapter.notifyDataSetChanged();
			loadingViewIsAdded = true;
			loadingView.setIndeterminate(R.string.download_waiting);
		}
	}

	@Override
	public void onCommentListingRequestDownloadStarted() {
		if(mAllUrls.size() > 1) {
			loadingView.setIndeterminate(R.string.download_downloading);
		} else {
			loadingView.setIndeterminate(R.string.download_connecting);
		}
	}

	@Override
	public void onCommentListingRequestException(final Throwable t) {
		BugReportActivity.handleGlobalError(getActivity(), t);
	}

	@Override
	public void onCommentListingRequestFailure(final RRError error) {
		loadingView.setDone(R.string.download_failed);
		listFooter.addView(new ErrorView(getActivity(), error));
	}

	@Override
	public void onCommentListingRequestCachedCopy(final long timestamp) {

		if(!isCachedNotifShown) {

			// TODO pref (currently 10 mins)
			if(RRTime.since(timestamp) > 10 * 60 * 1000) {

				final CachedHeaderView cacheNotif = new CachedHeaderView(
						getActivity(),
						getActivity().getString(R.string.listing_cached)
								+ " "
								+ RRTime.formatDateTime(timestamp, getActivity()),
						null
				);
				listHeaderNotifications.addView(cacheNotif);
				outerAdapter.notifyDataSetChanged();
			}

			isCachedNotifShown = true;
		}
	}

	@Override
	public void onCommentListingRequestParseStart() {
		loadingView.setIndeterminate(R.string.download_loading);
	}


	@Override
	public void onCommentListingRequestAuthorizing() {
		loadingView.setIndeterminate(R.string.download_authorizing);
	}

	@Override
	public void onCommentListingRequestPostDownloaded(final RedditPreparedPost post) {

		final Context context = getActivity();

		if(mPost == null) {

			mPost = post;

			final RedditPostHeaderView postHeader = new RedditPostHeaderView(getActivity(), CommentListingFragment.this.mPost);
			listHeaderPost.addView(postHeader);

			if(post.parsedSelfText != null) {
				final ViewGroup selfText = post.parsedSelfText.buildView(
						getActivity(), null, 14f * commentFontScale, mShowLinkButtons);
				selfText.setFocusable(false);
				selfText.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);

				final int paddingPx = General.dpToPixels(context, 10);
				listHeaderSelftext.addView(selfText);
				listHeaderSelftext.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
				listHeaderNotifications.setBackgroundColor(Color.argb(35, 128, 128, 128));
			}

			if(!General.isTablet(context, PreferenceManager.getDefaultSharedPreferences(context))) {
				getActivity().getActionBar().setTitle(StringEscapeUtils.unescapeHtml4(post.title));
			}
		}
	}

	private final ArrayList<RedditCommentListItem> mItemBuffer = new ArrayList<RedditCommentListItem>(64);

	@Override
	public void onCommentListingRequestItemDownloaded(final RedditCommentListItem item) {

		mItemBuffer.add(item);

		if(mItemBuffer.size() >= 20) {
			commentListAdapter.addItems(mItemBuffer);
			outerAdapter.notifyDataSetChanged();
			mItemBuffer.clear();
		}
	}

	@Override
	public void onCommentListingRequestComplete() {
		commentListAdapter.addItems(mItemBuffer);
		mItemBuffer.clear();

		if(mUrlsToDownload.isEmpty()) {

			if(loadingViewIsAdded) {
				loadingView.setDone(R.string.download_done);
				listFooter.removeView(loadingView);
			}

		} else {
			makeNextRequest(getActivity());
		}
	}

	private enum Action {
		UPVOTE,
		UNVOTE,
		DOWNVOTE,
		SAVE,
		UNSAVE,
		REPORT,
		SHARE,
		COPY,
		REPLY,
		USER_PROFILE,
		COMMENT_LINKS,
		COLLAPSE,
		EDIT,
		DELETE,
		PROPERTIES,
		CONTEXT,
		GO_TO_COMMENT
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
				comment.action(getActivity(), RedditAPI.RedditAction.UPVOTE);
				break;

			case DOWNVOTE:
				comment.action(getActivity(), RedditAPI.RedditAction.DOWNVOTE);
				break;

			case UNVOTE:
				comment.action(getActivity(), RedditAPI.RedditAction.UNVOTE);
				break;

			case SAVE:
				comment.action(getActivity(), RedditAPI.RedditAction.SAVE);
				break;

			case UNSAVE:
				comment.action(getActivity(), RedditAPI.RedditAction.UNSAVE);
				break;

			case REPORT:

				new AlertDialog.Builder(getActivity())
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(R.string.action_report,
								new DialogInterface.OnClickListener() {
									public void onClick(final DialogInterface dialog, final int which) {
										comment.action(getActivity(), RedditAPI.RedditAction.REPORT);
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case REPLY: {
				final Intent intent = new Intent(getActivity(), CommentReplyActivity.class);
				intent.putExtra("parentIdAndType", comment.idAndType);
				startActivity(intent);
				break;
			}

			case EDIT: {
				final Intent intent = new Intent(getActivity(), CommentEditActivity.class);
				intent.putExtra("commentIdAndType", comment.idAndType);
				intent.putExtra("commentText", StringEscapeUtils.unescapeHtml4(comment.src.body));
				startActivity(intent);
				break;
			}

			case DELETE: {
				new AlertDialog.Builder(getActivity())
						.setTitle(R.string.accounts_delete)
						.setMessage(R.string.delete_confirm)
						.setPositiveButton(R.string.action_delete,
								new DialogInterface.OnClickListener() {
									public void onClick(final DialogInterface dialog, final int which) {
										comment.action(getActivity(), RedditAPI.RedditAction.DELETE);
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();
				break;
			}

			case COMMENT_LINKS:
				final HashSet<String> linksInComment = comment.computeAllLinks();

				if(linksInComment.isEmpty()) {
					General.quickToast(getActivity(), R.string.error_toast_no_urls_in_comment);

				} else {

					final String[] linksArr = linksInComment.toArray(new String[linksInComment.size()]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
					builder.setItems(linksArr, new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface dialog, int which) {
							LinkHandler.onLinkClicked(getActivity(), linksArr[which], false);
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
				startActivityForResult(Intent.createChooser(mailer, getActivity().getString(R.string.action_share)), 1);

				break;

			case COPY:

				ClipboardManager manager = (ClipboardManager) getActivity().getSystemService(Context.CLIPBOARD_SERVICE);
				// TODO this currently just dumps the markdown
				manager.setText(StringEscapeUtils.unescapeHtml4(comment.src.body));
				break;

			case COLLAPSE:
				if(comment.getBoundView() != null) {
					handleCommentVisibilityToggle(comment.getBoundView());
				} else {
					General.quickToast(getActivity(), "Error: Comment is no longer visible.");
				}
				break;

			case USER_PROFILE:
				LinkHandler.onLinkClicked(getActivity(), new UserProfileURL(comment.src.author).toString());
				break;

			case PROPERTIES:
				CommentPropertiesDialog.newInstance(comment.src).show(getActivity().getFragmentManager(), null);
				break;

			case GO_TO_COMMENT: {
				PostCommentListingURL url = new PostCommentListingURL(
						null,
						comment.src.link_id,
						comment.idAlone,
						null,
						null,
						null);
				LinkHandler.onLinkClicked(getActivity(), url.toString());
				break;
			}

			case CONTEXT: {
				PostCommentListingURL url = new PostCommentListingURL(
						null,
						comment.src.link_id,
						comment.idAlone,
						3,
						null,
						null);
				LinkHandler.onLinkClicked(getActivity(), url.toString());
				break;
			}
		}

		return true;
	}

	@Override
	public void onCreateOptionsMenu(Menu menu) {
		if(mAllUrls != null && mAllUrls.size() > 0 && mAllUrls.get(0).pathType() == RedditURLParser.PathType.PostCommentListingURL) {
			menu.add(R.string.action_reply);
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {

		if(item.getTitle().equals(getActivity().getString(R.string.action_reply))) {
			onParentReply();
			return true;
		}

		return false;
	}

	private void onParentReply() {

		if(mPost != null) {
			final Intent intent = new Intent(getActivity(), CommentReplyActivity.class);
			intent.putExtra("parentIdAndType", mPost.idAndType);
			startActivity(intent);

		} else {
			General.quickToast(getActivity(), R.string.error_toast_parent_post_not_downloaded);
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostCommentsSelected(post);
	}
}
