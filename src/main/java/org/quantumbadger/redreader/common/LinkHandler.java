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

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Handler;
import android.preference.PreferenceManager;
import org.quantumbadger.redreader.activities.*;
import org.quantumbadger.redreader.fragments.UserProfileDialog;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;

import java.util.LinkedHashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LinkHandler {

	public static final Pattern
			youtubeDotComPattern = Pattern.compile("^https?://[\\.\\w]*youtube\\.\\w+/.*"),
			youtuDotBePattern = Pattern.compile("^https?://[\\.\\w]*youtu\\.be/([A-Za-z0-9\\-_]+)(\\?.*|).*"),
			vimeoPattern = Pattern.compile("^https?://[\\.\\w]*vimeo\\.\\w+/.*"),
			googlePlayPattern = Pattern.compile("^https?://[\\.\\w]*play\\.google\\.\\w+/.*");

	public static void onLinkClicked(Activity activity, String url) {
		onLinkClicked(activity, url, false);
	}

	public static void onLinkClicked(Activity activity, String url, boolean forceNoImage) {
		onLinkClicked(activity, url, forceNoImage, null);
	}

	public static void onLinkClicked(
			final Activity activity,
			String url,
			final boolean forceNoImage,
			final RedditPost post) {

		onLinkClicked(activity, url, forceNoImage, post, null, 0);
	}

	public static void onLinkClicked(
			final Activity activity,
			String url,
			final boolean forceNoImage,
			final RedditPost post,
			final ImgurAPI.AlbumInfo albumInfo,
			final int albumImageIndex) {

		if(url.startsWith("rr://")) {

			final Uri rrUri = Uri.parse(url);

			if(rrUri.getAuthority().equals("msg")) {
				new Handler().post(new Runnable() {
					public void run() {
						final AlertDialog.Builder builder = new AlertDialog.Builder(activity);
						builder.setTitle(rrUri.getQueryParameter("title"));
						builder.setMessage(rrUri.getQueryParameter("message"));
						AlertDialog alert = builder.create();
						alert.show();
					}
				});

				return;
			}
		}

		if(url.startsWith("r/") || url.startsWith("u/")) {
			url = "/" + url;
		}

		if(url.startsWith("/")) {
			url = "https://reddit.com" + url;
		}

		if(!url.contains("://")) {
			url = "http://" + url;
		}

		if(!forceNoImage && isProbablyAnImage(url)) {
			final Intent intent = new Intent(activity, ImageViewActivity.class);
			intent.setData(Uri.parse(url));
			intent.putExtra("post", post);

			if(albumInfo != null) {
				intent.putExtra("album", albumInfo.id);
				intent.putExtra("albumImageIndex", albumImageIndex);
			}

			activity.startActivity(intent);
			return;
		}

		if(!forceNoImage && imgurAlbumPattern.matcher(url).matches()) {
			final Intent intent = new Intent(activity, AlbumListingActivity.class);
			intent.setData(Uri.parse(url));
			intent.putExtra("post", post);
			activity.startActivity(intent);
			return;
		}

		final RedditURLParser.RedditURL redditURL = RedditURLParser.parse(Uri.parse(url));
		if(redditURL != null) {

			switch(redditURL.pathType()) {

				case SubredditPostListingURL:
				case UserPostListingURL:
				case UnknownPostListingURL: {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case PostCommentListingURL:
				case UserCommentListingURL: {
					final Intent intent = new Intent(activity, CommentListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case UserProfileURL: {
					UserProfileDialog.newInstance(redditURL.asUserProfileURL().username).show(activity.getFragmentManager(), null);
					return;
				}
			}
		}

		// Use a browser

		if(!PrefsUtility.pref_behaviour_useinternalbrowser(activity, PreferenceManager.getDefaultSharedPreferences(activity))) {
			openWebBrowser(activity, Uri.parse(url));
			return;
		}

		if(youtubeDotComPattern.matcher(url).matches()
				|| vimeoPattern.matcher(url).matches()
				|| googlePlayPattern.matcher(url).matches()) {
			openWebBrowser(activity, Uri.parse(url));
			return;
		}

		final Matcher youtuDotBeMatcher = youtuDotBePattern.matcher(url);

		if(youtuDotBeMatcher.find() && youtuDotBeMatcher.group(1) != null) {
			final String youtuBeUrl = "http://youtube.com/watch?v=" + youtuDotBeMatcher.group(1)
					+ (youtuDotBeMatcher.group(2).length() > 0 ? "&" + youtuDotBeMatcher.group(2).substring(1) : "");
			openWebBrowser(activity, Uri.parse(youtuBeUrl));

		} else {
			final Intent intent = new Intent(activity, WebViewActivity.class);
			intent.putExtra("url", url);
			intent.putExtra("post", post);
			activity.startActivity(intent);
		}
	}

	public static void openWebBrowser(Activity activity, Uri uri) {
		try {
			final Intent intent = new Intent(Intent.ACTION_VIEW);
			intent.setData(uri);
			activity.startActivity(intent);
		} catch(Exception e) {
			General.quickToast(activity, "Failed to open url \"" + uri.toString() + "\" in external browser");
		}
	}

	public static final Pattern imgurPattern = Pattern.compile(".*imgur\\.com/(\\w+).*"),
			imgurAlbumPattern = Pattern.compile(".*imgur\\.com/(a|gallery)/(\\w+).*"),
			qkmePattern1 = Pattern.compile(".*qkme\\.me/(\\w+).*"),
			qkmePattern2 = Pattern.compile(".*quickmeme\\.com/meme/(\\w+).*"),
			lvmePattern = Pattern.compile(".*livememe\\.com/(\\w+).*");

	public static boolean isProbablyAnImage(final String url) {

		final Matcher matchImgur = imgurPattern.matcher(url);

		if(matchImgur.find()) {
			final String imgId = matchImgur.group(1);
			if(imgId.length() > 2 && !imgId.startsWith("gallery")) {
				return true;
			}
		}

		return getImageUrlPatternMatch(url) != null;
	}

	public static void getImageInfo(
			final Context context,
			final String url,
			final int priority,
			final int listId,
			final GetImageInfoListener listener) {

		final Matcher matchImgur = imgurPattern.matcher(url);

		if(matchImgur.find()) {
			final String imgId = matchImgur.group(1);
			if(imgId.length() > 2 && !imgId.startsWith("gallery")) {
				ImgurAPI.getImageInfo(context, imgId, priority, listId, listener);
				return;
			}
		}

		final String imageUrlPatternMatch = getImageUrlPatternMatch(url);

		if(imageUrlPatternMatch != null) {
			listener.onSuccess(new ImgurAPI.ImageInfo(imageUrlPatternMatch));
		} else {
			listener.onNotAnImage();
		}
	}

	private static String getImageUrlPatternMatch(final String url) {

		final String urlLower = url.toLowerCase();

		final String[] imageExtensions = {".jpg", ".jpeg", ".png", ".gif", ".webm", ".mp4", ".h264", ".gifv", ".mkv", ".3gp"};

		for(final String ext : imageExtensions) {
			if(urlLower.endsWith(ext)) {
				return url;
			}
		}

		if(url.contains("?")) {

			final String urlBeforeQ = urlLower.split("\\?")[0];

			for(final String ext : imageExtensions) {
				if(urlBeforeQ.endsWith(ext)) {
					return url;
				}
			}
		}

		final Matcher matchQkme1 = qkmePattern1.matcher(url);

		if(matchQkme1.find()) {
			final String imgId = matchQkme1.group(1);
			if(imgId.length() > 2)
				return String.format("http://i.qkme.me/%s.jpg", imgId);
		}

		final Matcher matchQkme2 = qkmePattern2.matcher(url);

		if(matchQkme2.find()) {
			final String imgId = matchQkme2.group(1);
			if(imgId.length() > 2)
				return String.format("http://i.qkme.me/%s.jpg", imgId);
		}

		final Matcher matchLvme = lvmePattern.matcher(url);

		if(matchLvme.find()) {
			final String imgId = matchLvme.group(1);
			if(imgId.length() > 2)
				return String.format("http://www.livememe.com/%s.jpg", imgId);
		}

		return null;

	}

	public static LinkedHashSet<String> computeAllLinks(final String text) {

		final LinkedHashSet<String> result = new LinkedHashSet<String>();

		// From http://stackoverflow.com/a/1806161/1526861
		// TODO may not handle .co.uk, similar (but should handle .co/.us/.it/etc fine)
		final Pattern urlPattern = Pattern.compile("\\b((((ht|f)tp(s?)\\:\\/\\/|~\\/|\\/)|www.)" +
				"(\\w+:\\w+@)?(([-\\w]+\\.)+(com|org|net|gov" +
				"|mil|biz|info|mobi|name|aero|jobs|museum" +
				"|travel|[a-z]{2}))(:[\\d]{1,5})?" +
				"(((\\/([-\\w~!$+|.,=]|%[a-f\\d]{2})+)+|\\/)+|\\?|#)?" +
				"((\\?([-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
				"([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)" +
				"(&(?:[-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
				"([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)*)*" +
				"(#([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)?)\\b");

		final Matcher urlMatcher = urlPattern.matcher(text);

		while(urlMatcher.find()) {
			result.add(urlMatcher.group(1));
		}

		final Matcher subredditMatcher = Pattern.compile("(?<!\\w)(/?[ru]/\\w+)\\b").matcher(text);

		while(subredditMatcher.find()) {
			result.add(subredditMatcher.group(1));
		}

		return result;
	}
}
