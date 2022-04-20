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
import android.net.Uri;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.common.collections.CollectionStream;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;

public final class Constants {

	public static String version(final Context context) {
		try {
			return context.getPackageManager()
					.getPackageInfo(context.getPackageName(), 0).versionName;
		} catch(final PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e); // Internal error
		}
	}

	public static final class Mime {

		public static boolean isImage(final String mimetype) {
			return StringUtils.asciiLowercase(mimetype).startsWith("image/");
		}

		public static boolean isImageGif(final String mimetype) {
			return mimetype.equalsIgnoreCase("image/gif");
		}

		public static boolean isVideo(final String mimetype) {
			return mimetype.startsWith("video/");
		}

		public static boolean isOctetStream(final String mimetype) {
			return mimetype.equals("application/octet-stream");
		}
	}

	public static final class Reddit {

		public static final ArrayList<SubredditCanonicalId> DEFAULT_SUBREDDITS;

		public static final HashSet<String> BOT_USERNAMES_LOWERCASE;

		static {
			final String[] defaultSubredditStrings = {
					"/r/Art",
					"/r/AskReddit",
					"/r/askscience",
					"/r/aww",
					"/r/books",
					"/r/creepy",
					"/r/dataisbeautiful",
					"/r/DIY",
					"/r/Documentaries",
					"/r/EarthPorn",
					"/r/explainlikeimfive",
					"/r/Fitness",
					"/r/food",
					"/r/funny",
					"/r/Futurology",
					"/r/gadgets",
					"/r/gaming",
					"/r/GetMotivated",
					"/r/gifs",
					"/r/history",
					"/r/IAmA",
					"/r/InternetIsBeautiful",
					"/r/Jokes",
					"/r/LifeProTips",
					"/r/listentothis",
					"/r/mildlyinteresting",
					"/r/movies",
					"/r/Music",
					"/r/news",
					"/r/nosleep",
					"/r/nottheonion",
					"/r/oldschoolcool",
					"/r/personalfinance",
					"/r/philosophy",
					"/r/photoshopbattles",
					"/r/pics",
					"/r/reddit",
					"/r/science",
					"/r/Showerthoughts",
					"/r/space",
					"/r/sports",
					"/r/television",
					"/r/tifu",
					"/r/todayilearned",
					"/r/TwoXChromosomes",
					"/r/UpliftingNews",
					"/r/videos",
					"/r/worldnews",
					"/r/writingprompts"
			};

			DEFAULT_SUBREDDITS = new CollectionStream<>(defaultSubredditStrings)
					.mapRethrowExceptions(SubredditCanonicalId::new)
					.collect(new ArrayList<>(defaultSubredditStrings.length));

			BOT_USERNAMES_LOWERCASE = new HashSet<>();
			BOT_USERNAMES_LOWERCASE.add("automoderator");
			BOT_USERNAMES_LOWERCASE.add("qualityvote");
			BOT_USERNAMES_LOWERCASE.add("visualmod");
		}

		public static final String SCHEME_HTTPS = "https";
		public static final String DOMAIN_HTTPS = "oauth.reddit.com";
		public static final String DOMAIN_HTTPS_HUMAN = "reddit.com";
		public static final String PATH_VOTE = "/api/vote";
		public static final String PATH_SAVE = "/api/save";
		public static final String PATH_HIDE = "/api/hide";
		public static final String PATH_UNSAVE = "/api/unsave";
		public static final String PATH_UNHIDE = "/api/unhide";
		public static final String PATH_REPORT = "/api/report";
		public static final String PATH_DELETE = "/api/del";
		public static final String PATH_SUBSCRIBE = "/api/subscribe";
		public static final String PATH_SUBREDDITS_MINE_SUBSCRIBER
						= "/subreddits/mine/subscriber.json?limit=100";
		public static final String PATH_SUBREDDITS_MINE_MODERATOR
						= "/subreddits/mine/moderator.json?limit=100";
		public static final String PATH_SUBREDDITS_POPULAR = "/subreddits/popular.json";
		public static final String PATH_MULTIREDDITS_MINE = "/api/multi/mine.json";
		public static final String PATH_COMMENTS = "/comments/";
		public static final String PATH_ME = "/api/v1/me";

		public static String getScheme() {
			return SCHEME_HTTPS;
		}

		public static String getDomain() {
			return DOMAIN_HTTPS;
		}

		public static String getHumanReadableDomain() {
			return DOMAIN_HTTPS_HUMAN;
		}

		public static Uri.Builder getUriBuilder(final String path) {
			return Uri.parse(getUri(path).toString()).buildUpon();
		}

		public static URI getUri(final String path) {
			return General.uriFromString(getScheme() + "://" + getDomain() + path);
		}

		public static URI getNonAPIUri(final String path) {
			return General.uriFromString(getScheme() + "://reddit.com" + path);
		}

		public static boolean isApiErrorUser(final String str) {
			return ".error.USER_REQUIRED".equals(str) || "please login to do that".equals(
					str);
		}

		public static boolean isApiErrorCaptcha(final String str) {
			return ".error.BAD_CAPTCHA.field-captcha".equals(str)
					|| "care to try these again?".equals(str);
		}

		public static boolean isApiErrorNotAllowed(final String str) {
			return ".error.SUBREDDIT_NOTALLOWED.field-sr".equals(str)
					|| "you aren't allowed to post there.".equals(str);
		}

		public static boolean isApiErrorSubredditRequired(final String str) {
			return ".error.SUBREDDIT_REQUIRED.field-sr".equals(str)
					|| "you must specify a subreddit".equals(str);
		}

		public static boolean isApiErrorURLRequired(final String str) {
			return ".error.NO_URL.field-url".equals(str)
					|| "a url is required".equals(str);
		}

		public static boolean isApiTooFast(final String str) {
			return ".error.RATELIMIT.field-ratelimit".equals(str)
					|| (str != null && str.contains("you are doing that too much"));
		}

		public static boolean isApiTooLong(final String str) {
			return "TOO_LONG".equals(str)
					|| (str != null && str.contains("this is too long"));
		}

		public static boolean isApiAlreadySubmitted(final String str) {
			return ".error.ALREADY_SUB.field-url".equals(str)
					|| (str != null
							&& str.contains("that link has already been submitted"));
		}

		public static boolean isPostFlairRequired(final String str) {
			return ".error.SUBMIT_VALIDATION_FLAIR_REQUIRED.field-flair".equals(str)
					|| (str != null
							&& str.contains("Your post must contain post flair."));
		}

		public static boolean isApiError(final String str) {
			return str != null && str.startsWith(".error.");
		}
	}

	public static String ua(final Context context) {
		final String canonicalName = RedReader.class.getCanonicalName();
		return canonicalName.substring(0, canonicalName.lastIndexOf('.')) + "/" + version(
				context);
	}

	public static final class Priority {
		public static final int CAPTCHA = -600;
		public static final int API_ACTION = -500;
		public static final int API_MULTIREDDIT_LIST = -200;
		public static final int API_SUBREDDIT_LIST = -100;
		public static final int API_SUBREDDIT_SEARCH = -500;
		public static final int API_SUBREDDIT_INVIDIVUAL = -250;
		public static final int API_POST_LIST = -200;
		public static final int API_COMMENT_LIST = -300;
		public static final int THUMBNAIL = 100;
		public static final int INLINE_IMAGE_PREVIEW = 100;
		public static final int IMAGE_PRECACHE = 500;
		public static final int COMMENT_PRECACHE = 500;
		public static final int IMAGE_VIEW = -400;
		public static final int API_USER_ABOUT = -500;
		public static final int API_INBOX_LIST = -500;
		public static final int DEV_ANNOUNCEMENTS = 600;
	}

	public static final class FileType {
		public static final int NOCACHE = -1;
		public static final int SUBREDDIT_LIST = 100;
		public static final int SUBREDDIT_ABOUT = 101;
		public static final int MULTIREDDIT_LIST = 102;
		public static final int POST_LIST = 110;
		public static final int COMMENT_LIST = 120;
		public static final int USER_ABOUT = 130;
		public static final int INBOX_LIST = 140;
		public static final int THUMBNAIL = 200;
		public static final int IMAGE = 201;
		public static final int CAPTCHA = 202;
		public static final int INLINE_IMAGE_PREVIEW = 203;
		public static final int IMAGE_INFO = 300;
	}
}
