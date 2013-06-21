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

package org.quantumbadger.redreader.common;

import android.content.Context;
import android.content.pm.PackageManager;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.account.RedditAccountManager;

import java.net.URI;

public final class Constants {

	public static String version(Context context) {
		try {
			return context.getPackageManager().getPackageInfo(context.getPackageName(), 0).versionName;
		} catch(PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e); // Internal error
		}
	}

	public static final class Mime {

		public static boolean isImage(String mimetype) {
			return mimetype.toLowerCase().startsWith("image/");
		}

		public static boolean isImageGif(String mimetype) {
			return mimetype.equalsIgnoreCase("image/gif");
		}
	}

	public static final class Reddit {

		public static final String
				SCHEME_HTTP = "http",
				SCHEME_HTTPS = "https",
				DOMAIN_HTTP = "www.reddit.com",
				DOMAIN_HTTPS = "pay.reddit.com",
				PATH_VOTE = "/api/vote",
				PATH_SAVE = "/api/save",
				PATH_HIDE = "/api/hide",
				PATH_UNSAVE = "/api/unsave",
				PATH_UNHIDE = "/api/unhide",
				PATH_REPORT = "/api/report",
				PATH_SUBREDDITS_MINE_SUBSCRIBER = "/subreddits/mine/subscriber.json?limit=100",
				PATH_SUBREDDITS_POPULAR = "/subreddits/popular.json",
				PATH_COMMENTS = "/comments/";

		public static String getScheme() {
			return SCHEME_HTTP;
		}

		public static String getDomain() {
			return DOMAIN_HTTP;
		}

		public static URI getUri(final String path) {
			return General.uriFromString(getScheme() + "://" +  getDomain() + path);
		}

		public static boolean isApiErrorUser(final String str) {
			return ".error.USER_REQUIRED".equals(str) || "please login to do that".equals(str);
		}

		public static boolean isApiErrorCaptcha(final String str) {
			return ".error.BAD_CAPTCHA.field-captcha".equals(str) || "care to try these again?".equals(str);
		}

		public static boolean isApiErrorNotAllowed(final String str) {
			return ".error.SUBREDDIT_NOTALLOWED.field-sr".equals(str) || "you aren't allowed to post there.".equals(str);
		}

		public static boolean isApiErrorSubredditRequired(final String str) {
			return ".error.SUBREDDIT_REQUIRED.field-sr".equals(str) || "you must specify a subreddit".equals(str);
		}

		private static String getUserPath(Context context) {
			return "/user/" + RedditAccountManager.getInstance(context).getDefaultAccount().username;
		}

		public static String getSavedPath(Context context) {
			return getUserPath(context) + "/saved";
		}

		public static String getHiddenPath(Context context) {
			return getUserPath(context) + "/hidden";
		}

		public static String getLikedPath(Context context) {
			return getUserPath(context) + "/liked";
		}
	}

	public static String ua(final Context context) {
		final String canonicalName = RedReader.class.getCanonicalName();
		return canonicalName.substring(0, canonicalName.lastIndexOf('.')) + "-" + version(context);
	}

	public static final class Priority {
		public static final int
				CAPTCHA = -600,
				API_ACTION = -500,
				API_SUBREDDIT_LIST = -100,
				API_POST_LIST = -200,
				API_COMMENT_LIST = -300,
				THUMBNAIL = 100,
				IMAGE_PRECACHE = 500,
				IMAGE_VIEW = -400,
				API_USER_ABOUT = -500,
				API_INBOX_LIST = -500;
	}

	public static final class FileType {
		public static final int NOCACHE = -1,
				SUBREDDIT_LIST = 100,
				POST_LIST = 110,
				COMMENT_LIST = 120,
				USER_ABOUT = 130,
				INBOX_LIST = 140,
				THUMBNAIL = 200,
				IMAGE = 201,
				CAPTCHA = 202;
	}
}
