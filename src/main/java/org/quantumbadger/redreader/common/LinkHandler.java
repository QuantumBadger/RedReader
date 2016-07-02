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

import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Handler;
import android.os.Parcelable;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.AlbumListingActivity;
import org.quantumbadger.redreader.activities.CommentListingActivity;
import org.quantumbadger.redreader.activities.ImageViewActivity;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.activities.WebViewActivity;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.UserProfileDialog;
import org.quantumbadger.redreader.image.GetAlbumInfoListener;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.GfycatAPI;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.image.ImgurAPIV3;
import org.quantumbadger.redreader.image.StreamableAPI;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LinkHandler {

	public static final Pattern
			youtubeDotComPattern = Pattern.compile("^https?://[\\.\\w]*youtube\\.\\w+/.*"),
			youtuDotBePattern = Pattern.compile("^https?://[\\.\\w]*youtu\\.be/([A-Za-z0-9\\-_]+)(\\?.*|).*"),
			vimeoPattern = Pattern.compile("^https?://[\\.\\w]*vimeo\\.\\w+/.*"),
			googlePlayPattern = Pattern.compile("^https?://[\\.\\w]*play\\.google\\.\\w+/.*");

	public static void onLinkClicked(AppCompatActivity activity, String url) {
		onLinkClicked(activity, url, false);
	}

	public static void onLinkClicked(AppCompatActivity activity, String url, boolean forceNoImage) {
		onLinkClicked(activity, url, forceNoImage, null);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			String url,
			final boolean forceNoImage,
			final RedditPost post) {

		onLinkClicked(activity, url, forceNoImage, post, null, 0);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			String url,
			final boolean forceNoImage,
			final RedditPost post,
			final ImgurAPI.AlbumInfo albumInfo,
			final int albumImageIndex) {
		onLinkClicked(activity, url, forceNoImage, post, albumInfo, albumImageIndex, false);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			String url,
			final boolean forceNoImage,
			final RedditPost post,
			final ImgurAPI.AlbumInfo albumInfo,
			final int albumImageIndex,
			final boolean fromExternalIntent) {

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(activity);

		if(url.startsWith("rr://")) {

			final Uri rrUri = Uri.parse(url);

			if(rrUri.getAuthority().equals("msg")) {
				new Handler().post(new Runnable() {
					@Override
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

			final PrefsUtility.AlbumViewMode albumViewMode
					= PrefsUtility.pref_behaviour_albumview_mode(activity, sharedPreferences);

			switch(albumViewMode) {

				case INTERNAL_LIST: {
					final Intent intent = new Intent(activity, AlbumListingActivity.class);
					intent.setData(Uri.parse(url));
					intent.putExtra("post", post);
					activity.startActivity(intent);
					return;
				}

				case INTERNAL_BROWSER: {
					final Intent intent = new Intent(activity, WebViewActivity.class);
					intent.putExtra("url", url);
					intent.putExtra("post", post);
					activity.startActivity(intent);
					return;
				}

				case EXTERNAL_BROWSER: {
					openWebBrowser(activity, Uri.parse(url), fromExternalIntent);
					return;
				}
			}
		}

		final RedditURLParser.RedditURL redditURL = RedditURLParser.parse(Uri.parse(url));
		if(redditURL != null) {

			switch(redditURL.pathType()) {

				case RedditURLParser.SUBREDDIT_POST_LISTING_URL:
				case RedditURLParser.USER_POST_LISTING_URL:
				case RedditURLParser.UNKNOWN_POST_LISTING_URL: {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case RedditURLParser.POST_COMMENT_LISTING_URL:
				case RedditURLParser.USER_COMMENT_LISTING_URL: {
					final Intent intent = new Intent(activity, CommentListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case RedditURLParser.USER_PROFILE_URL: {
					UserProfileDialog.newInstance(redditURL.asUserProfileURL().username).show(activity.getSupportFragmentManager(), null);
					return;
				}
			}
		}

		// Use a browser

		if(!PrefsUtility.pref_behaviour_useinternalbrowser(activity, sharedPreferences)) {
			if(openWebBrowser(activity, Uri.parse(url), fromExternalIntent)) {
				return;
			}
		}

		if(youtubeDotComPattern.matcher(url).matches()
				|| vimeoPattern.matcher(url).matches()
				|| googlePlayPattern.matcher(url).matches()) {
			if(openWebBrowser(activity, Uri.parse(url), fromExternalIntent)) {
				return;
			}
		}

		final Matcher youtuDotBeMatcher = youtuDotBePattern.matcher(url);

		if(youtuDotBeMatcher.find() && youtuDotBeMatcher.group(1) != null) {
			final String youtuBeUrl = "http://youtube.com/watch?v=" + youtuDotBeMatcher.group(1)
					+ (youtuDotBeMatcher.group(2).length() > 0 ? "&" + youtuDotBeMatcher.group(2).substring(1) : "");
			if(openWebBrowser(activity, Uri.parse(youtuBeUrl), fromExternalIntent)) {
				return;
			}
		}

		final Intent intent = new Intent(activity, WebViewActivity.class);
		intent.putExtra("url", url);
		intent.putExtra("post", post);
		activity.startActivity(intent);
	}

	public static boolean openWebBrowser(AppCompatActivity activity, Uri uri, final boolean fromExternalIntent) {

		if(!fromExternalIntent) {
			try {
				final Intent intent = new Intent(Intent.ACTION_VIEW);
				intent.setData(uri);
				activity.startActivity(intent);
				return true;

			} catch(Exception e) {
				General.quickToast(activity, "Failed to open url \"" + uri.toString() + "\" in external browser");
			}

		} else {

			// We want to make sure we don't just pass this back to ourselves

			final Intent baseIntent = new Intent(Intent.ACTION_VIEW);
			baseIntent.setData(uri);

			final ArrayList<Intent> targetIntents = new ArrayList<>();

			for (final ResolveInfo info : activity.getPackageManager().queryIntentActivities(baseIntent, 0)) {

				final String packageName = info.activityInfo.packageName;
				Log.i("RRDEBUG", "Considering " + packageName);

				if (packageName != null && !packageName.startsWith("org.quantumbadger.redreader")) {
					final Intent intent = new Intent(Intent.ACTION_VIEW);
					intent.setData(uri);
					intent.setPackage(packageName);
					targetIntents.add(intent);
				}
			}

			if(!targetIntents.isEmpty()) {

				final Intent chooserIntent = Intent.createChooser(
						targetIntents.remove(0),
						activity.getString(R.string.open_with));

				if(!targetIntents.isEmpty()) {
					chooserIntent.putExtra(Intent.EXTRA_INITIAL_INTENTS, targetIntents.toArray(new Parcelable[]{}));
				}
				activity.startActivity(chooserIntent);

				return true;
			}
		}

		return false;
	}

	public static final Pattern imgurPattern = Pattern.compile(".*[^A-Za-z]imgur\\.com/(\\w+).*"),
			imgurAlbumPattern = Pattern.compile(".*[^A-Za-z]imgur\\.com/(a|gallery)/(\\w+).*"),
			qkmePattern1 = Pattern.compile(".*[^A-Za-z]qkme\\.me/(\\w+).*"),
			qkmePattern2 = Pattern.compile(".*[^A-Za-z]quickmeme\\.com/meme/(\\w+).*"),
			lvmePattern = Pattern.compile(".*[^A-Za-z]livememe\\.com/(\\w+).*"),
			gfycatPattern = Pattern.compile(".*[^A-Za-z]gfycat\\.com/(\\w+).*"),
			streamablePattern = Pattern.compile(".*[^A-Za-z]streamable\\.com/(\\w+).*"),
			reddituploadsPattern = Pattern.compile(".*[^A-Za-z]i\\.reddituploads\\.com/(\\w+).*");

	public static boolean isProbablyAnImage(final String url) {

		{
			final Matcher matchImgur = imgurPattern.matcher(url);

			if(matchImgur.find()) {
				final String imgId = matchImgur.group(1);
				if(imgId.length() > 2 && !imgId.startsWith("gallery")) {
					return true;
				}
			}
		}

		{
			final Matcher matchGfycat = gfycatPattern.matcher(url);

			if(matchGfycat.find()) {
				final String imgId = matchGfycat.group(1);
				if(imgId.length() > 5) {
					return true;
				}
			}
		}

		{
			final Matcher matchStreamable = streamablePattern.matcher(url);

			if(matchStreamable.find()) {
				final String imgId = matchStreamable.group(1);
				if(imgId.length() > 2) {
					return true;
				}
			}
		}

		{
			final Matcher matchRedditUploads = reddituploadsPattern.matcher(url);

			if(matchRedditUploads.find()) {
				final String imgId = matchRedditUploads.group(1);
				if(imgId.length() > 10) {
					return true;
				}
			}
		}

		return getImageUrlPatternMatch(url) != null;
	}

	private static abstract class ImageInfoRetryListener implements GetImageInfoListener {

		private final GetImageInfoListener mListener;

		private ImageInfoRetryListener(final GetImageInfoListener listener) {
			mListener = listener;
		}

		@Override
		public abstract void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage);

		@Override
		public void onSuccess(final ImageInfo info) {
			mListener.onSuccess(info);
		}

		@Override
		public void onNotAnImage() {
			mListener.onNotAnImage();
		}
	}

	public static void getImgurImageInfo(
			final Context context,
			final String imgId,
			final int priority,
			final int listId,
			final boolean returnUrlOnFailure,
			final GetImageInfoListener listener) {

		Log.i("getImgurImageInfo", "Image " + imgId + ": trying API v3 with auth");

		ImgurAPIV3.getImageInfo(context, imgId, priority, listId, true, new ImageInfoRetryListener(listener) {
			@Override
			public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

				Log.i("getImgurImageInfo", "Image " + imgId + ": trying API v3 without auth");

				ImgurAPIV3.getImageInfo(context, imgId, priority, listId, false, new ImageInfoRetryListener(listener) {
					@Override
					public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

						Log.i("getImgurImageInfo", "Image " + imgId + ": trying API v2");

						ImgurAPI.getImageInfo(context, imgId, priority, listId, new ImageInfoRetryListener(listener) {
							@Override
							public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

								Log.i("getImgurImageInfo", "All API requests failed!");

								if(returnUrlOnFailure) {
									listener.onSuccess(new ImageInfo("https://i.imgur.com/" + imgId + ".jpg"));

								} else {
									listener.onFailure(type, t, status, readableMessage);
								}
							}
						});
					}
				});
			}
		});
	}

	private static abstract class AlbumInfoRetryListener implements GetAlbumInfoListener {

		private final GetAlbumInfoListener mListener;

		private AlbumInfoRetryListener(final GetAlbumInfoListener listener) {
			mListener = listener;
		}

		@Override
		public abstract void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage);

		@Override
		public void onSuccess(final ImgurAPI.AlbumInfo info) {
			mListener.onSuccess(info);
		}
	}

	public static void getImgurAlbumInfo(
			final Context context,
			final String albumId,
			final int priority,
			final int listId,
			final GetAlbumInfoListener listener) {

		Log.i("getImgurAlbumInfo", "Album " + albumId + ": trying API v3 with auth");

		ImgurAPIV3.getAlbumInfo(context, albumId, priority, listId, true, new AlbumInfoRetryListener(listener) {
			@Override
			public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

				Log.i("getImgurAlbumInfo", "Album " + albumId + ": trying API v3 without auth");

				ImgurAPIV3.getAlbumInfo(context, albumId, priority, listId, false, new AlbumInfoRetryListener(listener) {
					@Override
					public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

						Log.i("getImgurAlbumInfo", "Album " + albumId + ": trying API v2");

						ImgurAPI.getAlbumInfo(context, albumId, priority, listId, new AlbumInfoRetryListener(listener) {
							@Override
							public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

								Log.i("getImgurImageInfo", "All API requests failed!");
								listener.onFailure(type, t, status, readableMessage);
							}
						});

					}
				});
			}
		});
	}

	public static void getImageInfo(
			final Context context,
			final String url,
			final int priority,
			final int listId,
			final GetImageInfoListener listener) {

		{
			final Matcher matchImgur = imgurPattern.matcher(url);

			if(matchImgur.find()) {
				final String imgId = matchImgur.group(1);
				if(imgId.length() > 2 && !imgId.startsWith("gallery")) {
					getImgurImageInfo(context, imgId, priority, listId, true, listener);
					return;
				}
			}
		}

		{
			final Matcher matchGfycat = gfycatPattern.matcher(url);

			if(matchGfycat.find()) {
				final String imgId = matchGfycat.group(1);
				if(imgId.length() > 5) {
					GfycatAPI.getImageInfo(context, imgId, priority, listId, listener);
					return;
				}
			}
		}

		{
			final Matcher matchStreamable = streamablePattern.matcher(url);

			if(matchStreamable.find()) {
				final String imgId = matchStreamable.group(1);
				if(imgId.length() > 2) {
					StreamableAPI.getImageInfo(context, imgId, priority, listId, listener);
					return;
				}
			}
		}

		{
			final Matcher matchRedditUploads = reddituploadsPattern.matcher(url);

			if(matchRedditUploads.find()) {
				final String imgId = matchRedditUploads.group(1);
				if(imgId.length() > 10) {
					listener.onSuccess(new ImageInfo(url));
					return;
				}
			}
		}

		final String imageUrlPatternMatch = getImageUrlPatternMatch(url);

		if(imageUrlPatternMatch != null) {
			listener.onSuccess(new ImageInfo(imageUrlPatternMatch));
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

		final LinkedHashSet<String> result = new LinkedHashSet<>();

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
