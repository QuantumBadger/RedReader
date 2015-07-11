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

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.things.RedditUser;
import org.quantumbadger.redreader.reddit.url.UserPostListingURL;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

public class UserProfileDialog extends PropertiesDialog {

	private String username;
	private boolean active = true;

	public static UserProfileDialog newInstance(final String user) {

		final UserProfileDialog dialog = new UserProfileDialog();

		final Bundle args = new Bundle();
		args.putString("user", user);
		dialog.setArguments(args);

		return dialog;
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		active = false;
	}

	@Override
	protected String getTitle(Context context) {
		return username;
	}

	@Override
	public final void prepare(final Activity context, final LinearLayout items) {

		final LoadingView loadingView = new LoadingView(context, R.string.download_waiting, true, true);
		items.addView(loadingView);

		username = getArguments().getString("user");
		final CacheManager cm = CacheManager.getInstance(context);

		RedditAPI.getUser(cm, username, new APIResponseHandler.UserResponseHandler(context) {
			@Override
			protected void onDownloadStarted() {
				if(!active) return;
				loadingView.setIndeterminate(R.string.download_connecting);
			}

			@Override
			protected void onSuccess(final RedditUser user, long timestamp) {

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					public void run() {

						if(!active) return;

						loadingView.setDone(R.string.download_done);

						final LinearLayout karmaLayout = (LinearLayout) getActivity().getLayoutInflater().inflate(R.layout.karma, null);
						items.addView(karmaLayout);

						final TextView linkKarma = (TextView) karmaLayout.findViewById(R.id.layout_karma_text_link);
						final TextView commentKarma = (TextView) karmaLayout.findViewById(R.id.layout_karma_text_comment);

						linkKarma.setText(String.valueOf(user.link_karma));
						commentKarma.setText(String.valueOf(user.comment_karma));

						items.addView(propView(context, R.string.userprofile_created, RRTime.formatDateTime(user.created_utc * 1000, context), false));

						if(user.has_mail != null) {
							items.addView(propView(context, R.string.userprofile_hasmail, user.has_mail ? R.string.general_true : R.string.general_false, false));
						}

						if(user.has_mod_mail != null) {
							items.addView(propView(context, R.string.userprofile_hasmodmail, user.has_mod_mail ? R.string.general_true : R.string.general_false, false));
						}

						if(user.is_friend) {
							items.addView(propView(context, R.string.userprofile_isfriend, R.string.general_true, false));
						}

						if(user.is_gold) {
							items.addView(propView(context, R.string.userprofile_isgold, R.string.general_true, false));
						}

						if(user.is_mod) {
							items.addView(propView(context, R.string.userprofile_moderator, R.string.general_true, false));
						}

						final Button commentsButton = new Button(context);
						commentsButton.setText(R.string.userprofile_viewcomments);
						commentsButton.setOnClickListener(new View.OnClickListener() {
							public void onClick(View v) {
								LinkHandler.onLinkClicked(getActivity(), Constants.Reddit.getUri("/user/" + username + "/comments.json").toString(), false);
							}
						});
						items.addView(commentsButton);
						// TODO use margin? or framelayout? scale padding dp
						// TODO change button color
						commentsButton.setPadding(20, 20, 20, 20);

						final Button postsButton = new Button(context);
						postsButton.setText(R.string.userprofile_viewposts);
						postsButton.setOnClickListener(new View.OnClickListener() {
							public void onClick(View v) {
								LinkHandler.onLinkClicked(getActivity(), UserPostListingURL.getSubmitted(username).generateJsonUri().toString(), false);
							}
						});
						items.addView(postsButton);
						// TODO use margin? or framelayout? scale padding dp
						postsButton.setPadding(20, 20, 20, 20);

					}
				});
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					public void run() {

						if(!active) return;

						loadingView.setDone(R.string.download_failed);

						final RRError error = General.getGeneralErrorForFailure(context, type, t, status, null);
						items.addView(new ErrorView(getActivity(), error));
					}
				});
			}

			@Override
			protected void onFailure(final APIFailureType type) {

				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					public void run() {

						if(!active) return;

						loadingView.setDone(R.string.download_failed);

						final RRError error = General.getGeneralErrorForFailure(context, type);
						items.addView(new ErrorView(getActivity(), error));
					}
				});
			}

		}, RedditAccountManager.getInstance(context).getDefaultAccount(), CacheRequest.DownloadType.FORCE, true, context);
	}
}
