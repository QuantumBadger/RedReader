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

package org.quantumbadger.redreader.reddit;

import android.content.Context;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditUser;

import java.util.List;

public abstract class APIResponseHandler {

	protected final Context context;

	public static enum APIFailureType {
		INVALID_USER, BAD_CAPTCHA, NOTALLOWED, SUBREDDIT_REQUIRED, UNKNOWN
	}

	private APIResponseHandler(Context context) {
		this.context = context;
	}

	protected abstract void onCallbackException(Throwable t);

	protected abstract void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage);
	protected abstract void onFailure(APIFailureType type);

	public final void notifyFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
		try {
			onFailure(type, t, status, readableMessage);
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public final void notifyFailure(final APIFailureType type) {
		try {
			onFailure(type);
		} catch(Throwable t1) {
			try {
				onCallbackException(t1);
			} catch(Throwable t2) {
				BugReportActivity.addGlobalError(new RRError(null, null, t1));
				BugReportActivity.handleGlobalError(context, t2);
			}
		}
	}

	public static abstract class ActionResponseHandler extends APIResponseHandler {

		protected ActionResponseHandler(Context context) {
			super(context);
		}

		public final void notifySuccess() {
			try {
				onSuccess();
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		protected abstract void onSuccess();
	}

	public static abstract class NewCaptchaResponseHandler extends APIResponseHandler {

		protected NewCaptchaResponseHandler(Context context) {
			super(context);
		}

		public final void notifySuccess(final String captchaId) {
			try {
				onSuccess(captchaId);
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		protected abstract void onSuccess(String captchaId);
	}

	public static abstract class SubredditResponseHandler extends APIResponseHandler {

		protected SubredditResponseHandler(Context context) {
			super(context);
		}

		public final void notifySuccess(final List<RedditSubreddit> result, final long timestamp) {
			try {
				onSuccess(result, timestamp);
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		public final void notifyDownloadNecessary() {
			try {
				onDownloadNecessary();
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		public final void notifyDownloadStarted() {
			try {
				onDownloadStarted();
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		protected abstract void onDownloadNecessary();
		protected abstract void onDownloadStarted();
		protected abstract void onSuccess(List<RedditSubreddit> result, long timestamp);
	}

	public static abstract class UserResponseHandler extends APIResponseHandler {

		protected UserResponseHandler(Context context) {
			super(context);
		}

		public final void notifySuccess(final RedditUser result, final long timestamp) {
			try {
				onSuccess(result, timestamp);
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		public final void notifyDownloadStarted() {
			try {
				onDownloadStarted();
			} catch(Throwable t1) {
				try {
					onCallbackException(t1);
				} catch(Throwable t2) {
					BugReportActivity.addGlobalError(new RRError(null, null, t1));
					BugReportActivity.handleGlobalError(context, t2);
				}
			}
		}

		protected abstract void onDownloadStarted();

		protected abstract void onSuccess(RedditUser result, long timestamp);
	}
}
