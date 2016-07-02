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

package org.quantumbadger.redreader.reddit.api;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.text.ClipboardManager;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.CommentEditActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.CommentPropertiesDialog;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.things.RedditComment;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.RedditCommentView;

import java.util.ArrayList;
import java.util.HashSet;

public class RedditAPICommentAction {

	public enum RedditCommentAction {
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

	private static class RCVMenuItem {
		public final String title;
		public final RedditCommentAction action;

		private RCVMenuItem(Context context, int titleRes, RedditCommentAction action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public static void showActionMenu(
			final AppCompatActivity activity,
			final CommentListingFragment commentListingFragment,
			final RedditRenderableComment comment,
			final RedditCommentView commentView,
			final RedditChangeDataManager changeDataManager,
			final boolean isArchived) {

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		final ArrayList<RCVMenuItem> menu = new ArrayList<>();

		if(!user.isAnonymous()) {

			if (!isArchived) {
				if (!changeDataManager.isUpvoted(comment)) {
					menu.add(new RCVMenuItem(activity, R.string.action_upvote, RedditCommentAction.UPVOTE));
				} else {
					menu.add(new RCVMenuItem(activity, R.string.action_upvote_remove, RedditCommentAction.UNVOTE));
				}

				if (!changeDataManager.isDownvoted(comment)) {
					menu.add(new RCVMenuItem(activity, R.string.action_downvote, RedditCommentAction.DOWNVOTE));
				} else {
					menu.add(new RCVMenuItem(activity, R.string.action_downvote_remove, RedditCommentAction.UNVOTE));
				}
			}

			if(changeDataManager.isSaved(comment)) {
				menu.add(new RCVMenuItem(activity, R.string.action_unsave, RedditCommentAction.UNSAVE));
			} else {
				menu.add(new RCVMenuItem(activity, R.string.action_save, RedditCommentAction.SAVE));
			}

			menu.add(new RCVMenuItem(activity, R.string.action_report, RedditCommentAction.REPORT));

			if (!isArchived)
				menu.add(new RCVMenuItem(activity, R.string.action_reply, RedditCommentAction.REPLY));

			if(user.username.equalsIgnoreCase(comment.getParsedComment().getRawComment().author)) {
				if (!isArchived)
					menu.add(new RCVMenuItem(activity, R.string.action_edit, RedditCommentAction.EDIT));
				menu.add(new RCVMenuItem(activity, R.string.action_delete, RedditCommentAction.DELETE));
			}
		}

		menu.add(new RCVMenuItem(activity, R.string.action_comment_context, RedditCommentAction.CONTEXT));
		menu.add(new RCVMenuItem(activity, R.string.action_comment_go_to, RedditCommentAction.GO_TO_COMMENT));

		menu.add(new RCVMenuItem(activity, R.string.action_comment_links, RedditCommentAction.COMMENT_LINKS));

		if(commentListingFragment != null) {
			menu.add(new RCVMenuItem(activity, R.string.action_collapse, RedditCommentAction.COLLAPSE));
		}

		menu.add(new RCVMenuItem(activity, R.string.action_share, RedditCommentAction.SHARE));
		menu.add(new RCVMenuItem(activity, R.string.action_copy, RedditCommentAction.COPY));
		menu.add(new RCVMenuItem(activity, R.string.action_user_profile, RedditCommentAction.USER_PROFILE));
		menu.add(new RCVMenuItem(activity, R.string.action_properties, RedditCommentAction.PROPERTIES));

		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		builder.setItems(menuText, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				onActionMenuItemSelected(
						comment,
						commentView,
						activity,
						commentListingFragment,
						menu.get(which).action,
						changeDataManager);
			}
		});

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	private static void onActionMenuItemSelected(
			final RedditRenderableComment renderableComment,
			final RedditCommentView commentView,
			final AppCompatActivity activity,
			final CommentListingFragment commentListingFragment,
			final RedditCommentAction action,
			final RedditChangeDataManager changeDataManager) {

		final RedditComment comment = renderableComment.getParsedComment().getRawComment();

		switch(action) {

			case UPVOTE:
				action(activity, comment, RedditAPI.ACTION_UPVOTE, changeDataManager);
				break;

			case DOWNVOTE:
				action(activity, comment, RedditAPI.ACTION_DOWNVOTE, changeDataManager);
				break;

			case UNVOTE:
				action(activity, comment, RedditAPI.ACTION_UNVOTE, changeDataManager);
				break;

			case SAVE:
				action(activity, comment, RedditAPI.ACTION_SAVE, changeDataManager);
				break;

			case UNSAVE:
				action(activity, comment, RedditAPI.ACTION_UNSAVE, changeDataManager);
				break;

			case REPORT:

				new AlertDialog.Builder(activity)
						.setTitle(R.string.action_report)
						.setMessage(R.string.action_report_sure)
						.setPositiveButton(R.string.action_report,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(final DialogInterface dialog, final int which) {
										action(activity, comment, RedditAPI.ACTION_REPORT, changeDataManager);
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();

				break;

			case REPLY: {
				final Intent intent = new Intent(activity, CommentReplyActivity.class);
				intent.putExtra("parentIdAndType", comment.getIdAndType());
				activity.startActivity(intent);
				break;
			}

			case EDIT: {
				final Intent intent = new Intent(activity, CommentEditActivity.class);
				intent.putExtra("commentIdAndType", comment.getIdAndType());
				intent.putExtra("commentText", StringEscapeUtils.unescapeHtml4(comment.body));
				activity.startActivity(intent);
				break;
			}

			case DELETE: {
				new AlertDialog.Builder(activity)
						.setTitle(R.string.accounts_delete)
						.setMessage(R.string.delete_confirm)
						.setPositiveButton(R.string.action_delete,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(final DialogInterface dialog, final int which) {
										action(activity, comment, RedditAPI.ACTION_DELETE, changeDataManager);
									}
								})
						.setNegativeButton(R.string.dialog_cancel, null)
						.show();
				break;
			}

			case COMMENT_LINKS:
				final HashSet<String> linksInComment = comment.computeAllLinks();

				if(linksInComment.isEmpty()) {
					General.quickToast(activity, R.string.error_toast_no_urls_in_comment);

				} else {

					final String[] linksArr = linksInComment.toArray(new String[linksInComment.size()]);

					final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
					builder.setItems(linksArr, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							LinkHandler.onLinkClicked(activity, linksArr[which], false);
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
				mailer.putExtra(Intent.EXTRA_SUBJECT, "Comment by " + comment.author + " on Reddit");

				// TODO this currently just dumps the markdown
				mailer.putExtra(Intent.EXTRA_TEXT, StringEscapeUtils.unescapeHtml4(comment.body));
				activity.startActivityForResult(Intent.createChooser(mailer, activity.getString(R.string.action_share)), 1);

				break;

			case COPY:
				ClipboardManager manager = (ClipboardManager)activity.getSystemService(Context.CLIPBOARD_SERVICE);
				// TODO this currently just dumps the markdown
				manager.setText(StringEscapeUtils.unescapeHtml4(comment.body));
				break;

			case COLLAPSE: {
				commentListingFragment.handleCommentVisibilityToggle(commentView);
				break;
			}

			case USER_PROFILE:
				LinkHandler.onLinkClicked(activity, new UserProfileURL(comment.author).toString());
				break;

			case PROPERTIES:
				CommentPropertiesDialog.newInstance(comment).show(activity.getSupportFragmentManager(), null);
				break;

			case GO_TO_COMMENT: {
				LinkHandler.onLinkClicked(activity, comment.getContextUrl().context(null).toString());
				break;
			}

			case CONTEXT: {
				LinkHandler.onLinkClicked(activity, comment.getContextUrl().toString());
				break;
			}
		}
	}

	public static void action(
			final AppCompatActivity activity,
			final RedditComment comment,
			final @RedditAPI.RedditAction int action,
			final RedditChangeDataManager changeDataManager) {

		final RedditAccount user = RedditAccountManager.getInstance(activity).getDefaultAccount();

		if(user.isAnonymous()) {
			General.quickToast(activity, "You must be logged in to do that.");
			return;
		}

		final boolean wasUpvoted = changeDataManager.isUpvoted(comment);
		final boolean wasDownvoted = changeDataManager.isUpvoted(comment);

		switch(action) {
			case RedditAPI.ACTION_DOWNVOTE:
				if(!comment.isArchived()) {
					changeDataManager.markDownvoted(RRTime.utcCurrentTimeMillis(), comment);
				}
				break;
			case RedditAPI.ACTION_UNVOTE:
				if(!comment.isArchived()) {
					changeDataManager.markUnvoted(RRTime.utcCurrentTimeMillis(), comment);
				}
				break;
			case RedditAPI.ACTION_UPVOTE:
				if(!comment.isArchived()) {
					changeDataManager.markUpvoted(RRTime.utcCurrentTimeMillis(), comment);
				}
				break;
			case RedditAPI.ACTION_SAVE: changeDataManager.markSaved(RRTime.utcCurrentTimeMillis(), comment, true); break;
			case RedditAPI.ACTION_UNSAVE: changeDataManager.markSaved(RRTime.utcCurrentTimeMillis(), comment, false); break;
		}

		boolean vote = (action == RedditAPI.ACTION_DOWNVOTE
				| action == RedditAPI.ACTION_UPVOTE
				| action == RedditAPI.ACTION_UNVOTE);

		if(comment.isArchived() && vote){
			Toast.makeText(activity, R.string.error_archived_vote, Toast.LENGTH_SHORT)
					.show();
			return;
		}

		RedditAPI.action(CacheManager.getInstance(activity),
				new APIResponseHandler.ActionResponseHandler(activity) {
					@Override
					protected void onCallbackException(final Throwable t) {
						throw new RuntimeException(t);
					}

					@Override
					protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
						revertOnFailure();
						if(t != null) t.printStackTrace();

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status, null);
						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onFailure(final APIFailureType type) {
						revertOnFailure();

						final RRError error = General.getGeneralErrorForFailure(context, type);
						AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.showResultDialog(activity, error);
							}
						});
					}

					@Override
					protected void onSuccess() {
						if(action == RedditAPI.ACTION_DELETE) {
							General.quickToast(context, R.string.delete_success);
						}
					}

					private void revertOnFailure() {

						switch(action) {
							case RedditAPI.ACTION_DOWNVOTE:
							case RedditAPI.ACTION_UNVOTE:
							case RedditAPI.ACTION_UPVOTE:
								if(wasUpvoted) {
									changeDataManager.markUpvoted(RRTime.utcCurrentTimeMillis(), comment);
								} else if(wasDownvoted) {
									changeDataManager.markDownvoted(RRTime.utcCurrentTimeMillis(), comment);
								} else {
									changeDataManager.markUnvoted(RRTime.utcCurrentTimeMillis(), comment);
								}
							case RedditAPI.ACTION_SAVE:
								changeDataManager.markSaved(RRTime.utcCurrentTimeMillis(), comment, false);
								break;
							case RedditAPI.ACTION_UNSAVE:
								changeDataManager.markSaved(RRTime.utcCurrentTimeMillis(), comment, true);
								break;
						}
					}

				}, user, comment.getIdAndType(), action, activity);
	}
}
