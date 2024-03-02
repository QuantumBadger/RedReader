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

import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Parcelable;
import android.util.Log;
import android.util.TypedValue;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.AlbumListingActivity;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.CommentListingActivity;
import org.quantumbadger.redreader.activities.ImageViewActivity;
import org.quantumbadger.redreader.activities.PMSendActivity;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.activities.WebViewActivity;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.ShareOrderDialog;
import org.quantumbadger.redreader.fragments.UserProfileDialog;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.image.AlbumInfo;
import org.quantumbadger.redreader.image.DeviantArtAPI;
import org.quantumbadger.redreader.image.GetAlbumInfoListener;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.GfycatAPI;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.image.ImgurAPI;
import org.quantumbadger.redreader.image.ImgurAPIV3;
import org.quantumbadger.redreader.image.RedditGalleryAPI;
import org.quantumbadger.redreader.image.RedditVideosAPI;
import org.quantumbadger.redreader.image.RedgifsAPI;
import org.quantumbadger.redreader.image.RedgifsAPIV2;
import org.quantumbadger.redreader.image.StreamableAPI;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;
import org.quantumbadger.redreader.reddit.url.ComposeMessageURL;
import org.quantumbadger.redreader.reddit.url.OpaqueSharedURL;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LinkHandler {

	public static final Pattern youtubeDotComPattern
			= Pattern.compile("^https?://[.\\w]*youtube\\.\\w+/.*");

	public static final Pattern youtuDotBePattern = Pattern.compile(
					"^https?://[.\\w]*youtu\\.be/([A-Za-z0-9\\-_]+)(\\?.*|).*");

	public static final Pattern vimeoPattern
			= Pattern.compile("^https?://[.\\w]*vimeo\\.\\w+/.*");

	public static final Pattern googlePlayPattern = Pattern.compile(
					"^https?://[.\\w]*play\\.google\\.\\w+/.*");

	public enum LinkAction {
		SHARE(R.string.action_share),
		COPY_URL(R.string.action_copy_link),
		SHARE_IMAGE(R.string.action_share_image),
		SAVE_IMAGE(R.string.action_save),
		EXTERNAL(R.string.action_external);

		public final int descriptionResId;

		LinkAction(final int descriptionResId) {
			this.descriptionResId = descriptionResId;
		}
	}

	public static void onLinkClicked(final AppCompatActivity activity, final String url) {
		onLinkClicked(activity, url, false);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			final String url,
			final boolean forceNoImage) {
		onLinkClicked(activity, url, forceNoImage, null);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			final String url,
			final boolean forceNoImage,
			final RedditPost post) {

		onLinkClicked(activity, url, forceNoImage, post, null, 0);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			final String url,
			final boolean forceNoImage,
			final RedditPost post,
			final AlbumInfo albumInfo,
			final int albumImageIndex) {
		onLinkClicked(
				activity,
				url,
				forceNoImage,
				post,
				albumInfo,
				albumImageIndex,
				false);
	}

	public static void onLinkClicked(
			final AppCompatActivity activity,
			final String url,
			final boolean forceNoImage,
			final RedditPost post,
			final AlbumInfo albumInfo,
			final int albumImageIndex,
			final boolean fromExternalIntent) {

		if(url == null) {
			General.quickToast(activity, R.string.link_does_not_exist);
			return;
		}

		if(url.startsWith("rr://")) {

			final Uri rrUri = Uri.parse(url);

			if(rrUri.getAuthority().equals("msg")) {
				new Handler().post(() -> {
					final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(
							activity);
					builder.setTitle(rrUri.getQueryParameter("title"));
					builder.setMessage(rrUri.getQueryParameter("message"));
					final AlertDialog alert = builder.create();
					alert.show();
				});

				return;
			}
		}

		final Uri normalUrl = convertAndNormalizeUri(url);
		final String normalUrlString = normalUrl.toString();

		if(!forceNoImage && isProbablyAnImage(normalUrlString)) {

			final Intent intent = new Intent(activity, ImageViewActivity.class);
			intent.setData(normalUrl);
			intent.putExtra("post", post);

			if(albumInfo != null) {
				intent.putExtra("albumUrl", albumInfo.url);
				intent.putExtra("albumImageIndex", albumImageIndex);
			}

			activity.startActivity(intent);
			return;
		}

		if(!forceNoImage && (imgurAlbumPattern.matcher(normalUrlString).matches()
				|| redditGalleryPattern.matcher(normalUrlString).matches())) {

			final PrefsUtility.AlbumViewMode albumViewMode
					= PrefsUtility.pref_behaviour_albumview_mode();

			switch(albumViewMode) {

				case INTERNAL_LIST: {
					final Intent intent = new Intent(
							activity,
							AlbumListingActivity.class);
					intent.setData(normalUrl);
					intent.putExtra("post", post);
					activity.startActivity(intent);
					return;
				}

				case INTERNAL_BROWSER: {
					if(PrefsUtility.pref_behaviour_usecustomtabs()
							&& Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
						openCustomTab(activity, normalUrl, post);

					} else {
						openInternalBrowser(activity, normalUrlString, post);
					}
					return;
				}

				case EXTERNAL_BROWSER: {
					openWebBrowser(activity, normalUrl, fromExternalIntent);
					return;
				}
			}
		}

		final RedditURLParser.RedditURL redditURL = RedditURLParser.parse(normalUrl);
		if(redditURL != null) {

			switch(redditURL.pathType()) {
				case RedditURLParser.OPAQUE_SHARED_URL:
					// kick off a thread to get the real url
					new Thread(() -> {
						final String toFetchUrl = OpaqueSharedURL.getUrlToFetch(
								(OpaqueSharedURL) redditURL
						).toString();
						final String realUrl = HTTPBackend.getBackend()
								.resolveRedirectUri(toFetchUrl);
						if(realUrl != null) {
							activity.runOnUiThread(() -> onLinkClicked(
									activity,
									realUrl,
									forceNoImage,
									post,
									albumInfo,
									albumImageIndex,
									fromExternalIntent));
						}
					}).start();
					return;
				case RedditURLParser.SUBREDDIT_POST_LISTING_URL:
				case RedditURLParser.MULTIREDDIT_POST_LISTING_URL:
				case RedditURLParser.USER_POST_LISTING_URL:
				case RedditURLParser.SEARCH_POST_LISTING_URL:
				case RedditURLParser.UNKNOWN_POST_LISTING_URL: {
					final Intent intent = new Intent(activity, PostListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case RedditURLParser.POST_COMMENT_LISTING_URL:
				case RedditURLParser.USER_COMMENT_LISTING_URL:
				case RedditURLParser.UNKNOWN_COMMENT_LISTING_URL:{
					final Intent intent = new Intent(
							activity,
							CommentListingActivity.class);
					intent.setData(redditURL.generateJsonUri());
					activity.startActivityForResult(intent, 1);
					return;
				}

				case RedditURLParser.COMPOSE_MESSAGE_URL: {
					final Intent intent = new Intent(
							activity,
							PMSendActivity.class);
					final ComposeMessageURL cmUrl = redditURL.asComposeMessageURL();

					if(cmUrl.recipient != null) {
						intent.putExtra(PMSendActivity.EXTRA_RECIPIENT, cmUrl.recipient);
					}
					if(cmUrl.subject != null) {
						intent.putExtra(PMSendActivity.EXTRA_SUBJECT, cmUrl.subject);
					}
					if(cmUrl.message != null) {
						intent.putExtra(PMSendActivity.EXTRA_TEXT, cmUrl.message);
					}

					activity.startActivityForResult(intent, 1);
					return;
				}

				case RedditURLParser.USER_PROFILE_URL: {
					UserProfileDialog.show(activity, redditURL.asUserProfileURL().username);
					return;
				}
			}
		}

		// Use a browser

		if(!PrefsUtility.pref_behaviour_useinternalbrowser()) {
			if(openWebBrowser(activity, normalUrl, fromExternalIntent)) {
				return;
			}
		}

		if(youtubeDotComPattern.matcher(normalUrlString).matches()
				|| vimeoPattern.matcher(normalUrlString).matches()
				|| googlePlayPattern.matcher(normalUrlString).matches()
				|| normalUrlString.startsWith("mailto:")) {
			if(openWebBrowser(activity, normalUrl, fromExternalIntent)) {
				return;
			}
		}

		final Matcher youtuDotBeMatcher = youtuDotBePattern.matcher(normalUrlString);

		if(youtuDotBeMatcher.find() && youtuDotBeMatcher.group(1) != null) {
			final String youtuBeUrl = "http://youtube.com/watch?v="
					+ youtuDotBeMatcher.group(1)
					+ (!youtuDotBeMatcher.group(2).isEmpty()
							? "&" + youtuDotBeMatcher.group(2) .substring(1)
							: "");
			if(openWebBrowser(activity, Uri.parse(youtuBeUrl), fromExternalIntent)) {
				return;
			}
		}

		if(PrefsUtility.pref_behaviour_usecustomtabs()
				&& Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			openCustomTab(activity, normalUrl, post);

		} else {
			openInternalBrowser(activity, normalUrlString, post);
		}

	}

	public static void onLinkLongClicked(final BaseActivity activity, final String uri) {
		onLinkLongClicked(activity, uri, false);
	}

	public static void onLinkLongClicked(
			final BaseActivity activity,
			final String uri,
			final boolean forceNoImage) {
		if(uri == null) {
			return;
		}

		final String normalUriString = convertAndNormalizeUri(uri).toString();

		final EnumSet<LinkHandler.LinkAction> itemPref
				= PrefsUtility.pref_menus_link_context_items();

		if(itemPref.isEmpty()) {
			return;
		}

		final ArrayList<LinkMenuItem> menu = new ArrayList<>();

		if(itemPref.contains(LinkAction.COPY_URL)) {
			menu.add(new LinkMenuItem(
					activity,
					R.string.action_copy_link,
					LinkAction.COPY_URL));
		}
		if(itemPref.contains(LinkAction.EXTERNAL)) {
			menu.add(new LinkMenuItem(
					activity,
					R.string.action_external,
					LinkAction.EXTERNAL));
		}
		if(itemPref.contains(LinkAction.SAVE_IMAGE)
				&& isProbablyAnImage(normalUriString)
				&& !forceNoImage) {
			menu.add(new LinkMenuItem(
					activity,
					R.string.action_save_image,
					LinkAction.SAVE_IMAGE));
		}
		if(itemPref.contains(LinkAction.SHARE)) {
			menu.add(new LinkMenuItem(activity, R.string.action_share, LinkAction.SHARE));
		}
		if(itemPref.contains(LinkAction.SHARE_IMAGE)
				&& isProbablyAnImage(normalUriString)
				&& !forceNoImage) {
			menu.add(new LinkMenuItem(
					activity,
					R.string.action_share_image,
					LinkAction.SHARE_IMAGE));
		}
		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(activity);

		builder.setItems(menuText,
				(dialog, which) -> onActionMenuItemSelected(
						normalUriString,
						activity,
						menu.get(which).action));

		//builder.setNeutralButton(R.string.dialog_cancel, null);

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	public static void onActionMenuItemSelected(
			final String uri,
			final BaseActivity activity,
			final LinkAction action) {
		switch(action) {
			case SHARE:
				shareText(activity, null, getPreferredRedditUriString(uri));
				break;
			case COPY_URL:
				final ClipboardManager clipboardManager
						= (ClipboardManager)activity.getSystemService(Context.CLIPBOARD_SERVICE);
				if(clipboardManager != null) {
					// Using newPlainText here instead of newRawUri because links from
					// comments/self-text are often not valid URIs
					final ClipData data = ClipData.newPlainText(null, uri);
					clipboardManager.setPrimaryClip(data);

					General.quickToast(
							activity.getApplicationContext(),
							R.string.link_copied_to_clipboard);
				}
				break;

			case EXTERNAL:
				try {
					final Intent intent = new Intent(Intent.ACTION_VIEW);
					intent.setData(Uri.parse(uri));
					activity.startActivity(intent);
				} catch(final ActivityNotFoundException e) {
					General.quickToast(
							activity,
							R.string.error_no_suitable_apps_available);
				}
				break;
			case SHARE_IMAGE:
				FileUtils.shareImageAtUri(activity, uri);
				break;
			case SAVE_IMAGE:
				FileUtils.saveImageAtUri(activity, uri);
				break;
		}
	}

	public static boolean openWebBrowser(
			final AppCompatActivity activity,
			final Uri uri,
			final boolean fromExternalIntent) {

		if(!fromExternalIntent) {
			try {
				final Intent intent = new Intent(Intent.ACTION_VIEW);
				intent.setData(uri);
				activity.startActivity(intent);
				return true;

			} catch(final Exception e) {
				General.quickToast(
						activity,
						String.format(
								activity.getString(
										R.string.error_toast_failed_open_external_browser),
								uri.toString()));
			}

		} else {

			// We want to make sure we don't just pass this back to ourselves

			final Intent baseIntent = new Intent(Intent.ACTION_VIEW);
			baseIntent.setData(uri);

			final ArrayList<Intent> targetIntents = new ArrayList<>();

			for(final ResolveInfo info : activity.getPackageManager()
					.queryIntentActivities(baseIntent, 0)) {

				final String packageName = info.activityInfo.packageName;

				if(packageName != null && !packageName.startsWith(
						"org.quantumbadger.redreader")) {
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
					chooserIntent.putExtra(
							Intent.EXTRA_INITIAL_INTENTS,
							targetIntents.toArray(new Parcelable[] {}));
				}
				activity.startActivity(chooserIntent);

				return true;
			}
		}

		return false;
	}

	public static void openInternalBrowser(
			final AppCompatActivity activity,
			final String url,
			@Nullable final RedditPost post) {

		final Intent intent = new Intent();
		intent.setClass(activity, WebViewActivity.class);
		intent.putExtra("url", url);
		intent.putExtra("post", post);
		activity.startActivity(intent);
	}

	@RequiresApi(18)
	public static void openCustomTab(
			final AppCompatActivity activity,
			final Uri uri,
			@Nullable final RedditPost post) {

		try {
			final Intent intent = new Intent();
			intent.setAction(Intent.ACTION_VIEW);
			intent.setData(uri);
			intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

			final Bundle bundle = new Bundle();
			bundle.putBinder("android.support.customtabs.extra.SESSION", null);
			intent.putExtras(bundle);

			intent.putExtra("android.support.customtabs.extra.SHARE_MENU_ITEM", true);

			final TypedValue typedValue = new TypedValue();
			activity.getTheme().resolveAttribute(
					com.google.android.material.R.attr.colorPrimary,
					typedValue,
					true
			);

			intent.putExtra(
					"android.support.customtabs.extra.TOOLBAR_COLOR",
					typedValue.data);

			intent.putExtra("android.support.customtabs.extra.ENABLE_URLBAR_HIDING", true);

			activity.startActivity(intent);

		} catch(final ActivityNotFoundException e) {
			// No suitable web browser installed. Use internal browser.
			openInternalBrowser(activity, uri.toString(), post);
		}
	}

	public static final Pattern imgurPattern
			= Pattern.compile("https?://?(i\\.)?imgur\\.com/(\\w+).*");
	public static final Pattern imgurAlbumPattern
			= Pattern.compile(".*[^A-Za-z]imgur\\.com/(a|gallery)/(\\w+).*");
	public static final Pattern redditGalleryPattern
			= Pattern.compile(".*[^A-Za-z]reddit\\.com/gallery/(\\w+).*");
	public static final Pattern qkmePattern1
			= Pattern.compile(".*[^A-Za-z]qkme\\.me/(\\w+).*");
	public static final Pattern qkmePattern2
			= Pattern.compile(".*[^A-Za-z]quickmeme\\.com/meme/(\\w+).*");
	public static final Pattern lvmePattern
			= Pattern.compile(".*[^A-Za-z]livememe\\.com/(\\w+).*");
	public static final Pattern gfycatPattern
			= Pattern.compile(".*[^A-Za-z]gfycat\\.com/(?:gifs/detail/)?(\\w+).*");
	public static final Pattern redgifsPattern
			= Pattern.compile(".*[^A-Za-z]redgifs\\.com/watch/(?:gifs/detail/)?(\\w+).*");
	public static final Pattern streamablePattern
			= Pattern.compile(".*[^A-Za-z]streamable\\.com/(\\w+).*");
	public static final Pattern reddituploadsPattern
			= Pattern.compile(".*[^A-Za-z]i\\.reddituploads\\.com/(\\w+).*");
	public static final Pattern redditVideosPattern
			= Pattern.compile(".*[^A-Za-z]v.redd.it/(\\w+).*");
	public static final Pattern imgflipPattern
			= Pattern.compile(".*[^A-Za-z]imgflip\\.com/i/(\\w+).*");
	public static final Pattern makeamemePattern
			= Pattern.compile(".*[^A-Za-z]makeameme\\.org/meme/([\\w\\-]+).*");
	public static final Pattern deviantartPattern
			= Pattern.compile("https://www\\.deviantart\\.com/([\\w\\-]+)/art/([\\w\\-]+)");
	public static final Pattern giphyPattern
			= Pattern.compile(".*[^A-Za-z]giphy\\.com/gifs/(\\w+).*");

	public static boolean isProbablyAnImage(@Nullable final String url) {

		if(url == null) {
			return false;
		}

		{
			final Matcher matchImgur = imgurPattern.matcher(url);

			if(matchImgur.find()) {
				final String imgId = matchImgur.group(2);
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
			final Matcher matchRedgifs = redgifsPattern.matcher(url);

			if(matchRedgifs.find()) {
				final String imgId = matchRedgifs.group(1);
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

		{
			final Matcher matchImgflip = imgflipPattern.matcher(url);

			if(matchImgflip.find()) {
				final String imgId = matchImgflip.group(1);
				if(imgId.length() > 3) {
					return true;
				}
			}
		}

		{
			final Matcher matchMakeameme = makeamemePattern.matcher(url);

			if(matchMakeameme.find()) {
				final String imgId = matchMakeameme.group(1);
				if(imgId.length() > 3) {
					return true;
				}
			}
		}

		{
			final Matcher matchDeviantart = deviantartPattern.matcher(url);

			if(matchDeviantart.find()) {
				if(url.length() > 40) {
					return true;
				}
			}
		}

		{
			final Matcher matchRedditVideos = redditVideosPattern.matcher(url);

			if(matchRedditVideos.find()) {
				final String imgId = matchRedditVideos.group(1);
				if(imgId.length() > 3) {
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
			@NonNull final Priority priority,
			final boolean returnUrlOnFailure,
			final GetImageInfoListener listener) {

		if(General.isSensitiveDebugLoggingEnabled()) {
			Log.i("getImgurImageInfo", "Image " + imgId + ": trying API v3 with auth");
		}

		ImgurAPIV3.getImageInfo(
				context,
				imgId,
				priority,
				true,
				new ImageInfoRetryListener(listener) {
					@Override
					public void onFailure(@NonNull final RRError firstError) {

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.i(
									"getImgurImageInfo",
									"Image " + imgId + ": trying API v3 without auth");
						}

						ImgurAPIV3.getImageInfo(
								context,
								imgId,
								priority,
								false,
								new ImageInfoRetryListener(listener) {
									@Override
									public void onFailure(@NonNull final RRError error) {

										if(General.isSensitiveDebugLoggingEnabled()) {
											Log.i(
													"getImgurImageInfo",
													"Image " + imgId + ": trying API v2");
										}

										ImgurAPI.getImageInfo(
												context,
												imgId,
												priority,
												new ImageInfoRetryListener(listener) {
													@Override
													public void onFailure(
															@NonNull final RRError error) {

														Log.i(
																"getImgurImageInfo",
																"All API requests failed!");

														if(returnUrlOnFailure) {
															listener.onSuccess(new ImageInfo(
																	"https://i.imgur.com/"
																			+ imgId
																			+ ".jpg",
																	null,
																	ImageInfo.HasAudio.MAYBE_AUDIO
															));

														} else {
															listener.onFailure(firstError);
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
		public void onGalleryRemoved() {
			mListener.onGalleryRemoved();
		}

		@Override
		public void onGalleryDataNotPresent() {
			mListener.onGalleryDataNotPresent();
		}

		@Override
		public void onSuccess(@NonNull final AlbumInfo info) {
			mListener.onSuccess(info);
		}
	}

	public static void getImgurAlbumInfo(
			final Context context,
			final String albumUrl,
			final String albumId,
			@NonNull final Priority priority,
			final GetAlbumInfoListener listener) {

		if(General.isSensitiveDebugLoggingEnabled()) {
			Log.i("getImgurAlbumInfo", "Album " + albumId + ": trying API v3 with auth");
		}

		ImgurAPIV3.getAlbumInfo(
				context,
				albumUrl,
				albumId,
				priority,
				true,
				new AlbumInfoRetryListener(listener) {
					@Override
					public void onFailure(@NonNull final RRError firstError) {

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.i(
									"getImgurAlbumInfo",
									"Album " + albumId + ": trying API v3 without auth");
						}

						ImgurAPIV3.getAlbumInfo(
								context,
								albumUrl,
								albumId,
								priority,
								false,
								new AlbumInfoRetryListener(listener) {
									@Override
									public void onFailure(@NonNull final RRError error) {

										if(General.isSensitiveDebugLoggingEnabled()) {
											Log.i(
													"getImgurAlbumInfo",
													"Album " + albumId + ": trying API v2");
										}

										ImgurAPI.getAlbumInfo(
												context,
												albumUrl,
												albumId,
												priority,
												new AlbumInfoRetryListener(listener) {
													@Override
													public void onFailure(
															@NonNull final RRError error) {

														Log.i(
																"getImgurImageInfo",
																"All API requests failed!");
														listener.onFailure(firstError);
													}
												});

									}
								});
					}
				});
	}

	public static void getAlbumInfo(
			final Context context,
			final String url,
			@NonNull final Priority priority,
			final GetAlbumInfoListener listener) {

		{
			final Matcher matchImgur = imgurAlbumPattern.matcher(url);

			if(matchImgur.find()) {
				final String albumId = matchImgur.group(2);
				if(albumId.length() > 2) {
					getImgurAlbumInfo(context, url, albumId, priority, listener);
					return;
				}
			}
		}

		{
			final Matcher matchReddit = redditGalleryPattern.matcher(url);

			if(matchReddit.find()) {
				final String albumId = matchReddit.group(1);
				if(albumId.length() > 2) {
					RedditGalleryAPI.getAlbumInfo(
							context,
							url,
							albumId,
							priority,
							listener);
					return;
				}
			}
		}

		listener.onFailure(General.getGeneralErrorForFailure(
				context,
				CacheRequest.REQUEST_FAILURE_MALFORMED_URL,
				null,
				null,
				url,
				Optional.empty()));
	}

	public static void getImageInfo(
			final Context context,
			@Nullable final String url,
			@NonNull final Priority priority,
			final GetImageInfoListener listener) {

		if(url == null) {
			listener.onNotAnImage();
			return;
		}

		{
			final Matcher matchImgur = imgurPattern.matcher(url);

			if(matchImgur.find()) {
				final String imgId = matchImgur.group(2);
				if(imgId.length() > 2 && !imgId.startsWith("gallery")) {
					getImgurImageInfo(context, imgId, priority, true, listener);
					return;
				}
			}
		}

		{
			final Matcher matchGfycat = gfycatPattern.matcher(url);

			if(matchGfycat.find()) {
				final String imgId = matchGfycat.group(1);
				if(imgId.length() > 5) {
					GfycatAPI.getImageInfo(context, imgId, priority, listener);
					return;
				}
			}
		}

		{
			final Matcher matchRedgifs = redgifsPattern.matcher(url);

			if(matchRedgifs.find()) {
				final String imgId = matchRedgifs.group(1);
				if(imgId.length() > 5) {
					RedgifsAPIV2.getImageInfo(
							context,
							imgId,
							priority,
							new ImageInfoRetryListener(listener) {

						@Override
						public void onFailure(@NonNull final RRError error) {

							Log.e(
									"getImageInfo",
									"RedGifs V2 failed, trying V1 (" + error + ")",
									error.t);

							RedgifsAPI.getImageInfo(
									context,
									imgId,
									priority,
									new ImageInfoRetryListener(listener) {
								@Override
								public void onFailure(@NonNull final RRError error) {

									// Retry V2 so that the final error which is logged
									// relates to the V2 API
									Log.e(
											"getImageInfo",
											"RedGifs V1 also failed, retrying V2: " + error,
											error.t);

									RedgifsAPIV2.getImageInfo(context, imgId, priority, listener);
								}
							});
						}
					});
					return;
				}
			}
		}

		{
			final Matcher matchStreamable = streamablePattern.matcher(url);

			if(matchStreamable.find()) {
				final String imgId = matchStreamable.group(1);
				if(imgId.length() > 2) {
					StreamableAPI.getImageInfo(
							context,
							imgId,
							priority,
							listener);
					return;
				}
			}
		}
		{
			final Matcher matchDeviantart = deviantartPattern.matcher(url);

			if(matchDeviantart.find()) {
				if(url.length() > 40) {
					DeviantArtAPI.getImageInfo(
							context,
							url,
							priority,
							listener);
					return;
				}
			}
		}
		{
			final Matcher matchRedditVideos = redditVideosPattern.matcher(url);

			if(matchRedditVideos.find()) {

				final String imgId = matchRedditVideos.group(1);
				if(imgId.length() > 3) {
					RedditVideosAPI.getImageInfo(
							context,
							imgId,
							priority,
							listener);
					return;
				}
			}
		}

		final ImageInfo imageUrlPatternMatch = getImageUrlPatternMatch(url);

		if(imageUrlPatternMatch != null) {
			listener.onSuccess(imageUrlPatternMatch);
		} else {
			listener.onNotAnImage();
		}
	}

	private static ImageInfo getImageUrlPatternMatch(final String url) {

		final String urlLower = StringUtils.asciiLowercase(url);

		{
			final Matcher matchRedditUploads = reddituploadsPattern.matcher(url);

			if(matchRedditUploads.find()) {
				final String imgId = matchRedditUploads.group(1);
				if(imgId.length() > 10) {
					return new ImageInfo(
							url,
							ImageInfo.MediaType.IMAGE,
							ImageInfo.HasAudio.NO_AUDIO);
				}
			}
		}

		{
			final Matcher matchImgflip = imgflipPattern.matcher(url);

			if(matchImgflip.find()) {
				final String imgId = matchImgflip.group(1);
				if(imgId.length() > 3) {
					final String imageUrl = "https://i.imgflip.com/" + imgId + ".jpg";
					return new ImageInfo(
							imageUrl,
							ImageInfo.MediaType.IMAGE,
							ImageInfo.HasAudio.NO_AUDIO);
				}
			}
		}

		{
			final Matcher matchMakeameme = makeamemePattern.matcher(url);

			if(matchMakeameme.find()) {
				final String imgId = matchMakeameme.group(1);
				if(imgId.length() > 3) {
					final String imageUrl = "https://media.makeameme.org/created/"
							+ imgId
							+ ".jpg";
					return new ImageInfo(
							imageUrl,
							ImageInfo.MediaType.IMAGE,
							ImageInfo.HasAudio.NO_AUDIO);
				}
			}
		}

		{
			final Matcher matchGiphy = giphyPattern.matcher(url);

			if(matchGiphy.find()) {
				return new ImageInfo(
						"https://media.giphy.com/media/"
								+ matchGiphy.group(1)
								+ "/giphy.mp4",
						ImageInfo.MediaType.VIDEO,
						ImageInfo.HasAudio.NO_AUDIO);
			}
		}

		final String[] imageExtensions = {".jpg", ".jpeg", ".png"};

		final String[] videoExtensions = {
				".webm",
				".mp4",
				".h264",
				".gifv",
				".mkv",
				".3gp"};


		for(final String ext : imageExtensions) {
			if(urlLower.endsWith(ext)) {
				return new ImageInfo(
						url,
						ImageInfo.MediaType.IMAGE,
						ImageInfo.HasAudio.MAYBE_AUDIO);
			}
		}

		for(final String ext : videoExtensions) {
			if(urlLower.endsWith(ext)) {
				return new ImageInfo(
						url,
						ImageInfo.MediaType.VIDEO,
						ImageInfo.HasAudio.MAYBE_AUDIO);
			}
		}

		if(urlLower.endsWith(".gif")) {

			final ImageInfo.HasAudio audio;

			if(urlLower.contains(".redd.it")) { // preview.redd.it or i.redd.it
				audio = ImageInfo.HasAudio.NO_AUDIO;
			} else {
				audio = ImageInfo.HasAudio.MAYBE_AUDIO;
			}

			return new ImageInfo(url, ImageInfo.MediaType.GIF, audio);
		}


		if(url.contains("?")) {

			final String urlBeforeQ = urlLower.split("\\?")[0];

			for(final String ext : imageExtensions) {
				if(urlBeforeQ.endsWith(ext)) {
					return new ImageInfo(
							url,
							ImageInfo.MediaType.IMAGE,
							ImageInfo.HasAudio.MAYBE_AUDIO);
				}
			}

			for(final String ext : videoExtensions) {
				if(urlBeforeQ.endsWith(ext)) {
					return new ImageInfo(
							url,
							ImageInfo.MediaType.VIDEO,
							ImageInfo.HasAudio.MAYBE_AUDIO);
				}
			}

			if(urlBeforeQ.endsWith(".gif")) {

				final ImageInfo.HasAudio audio;

				if(urlLower.contains(".redd.it")) { // preview.redd.it or i.redd.it
					audio = ImageInfo.HasAudio.NO_AUDIO;
				} else {
					audio = ImageInfo.HasAudio.MAYBE_AUDIO;
				}

				return new ImageInfo(url, ImageInfo.MediaType.GIF, audio);
			}
		}

		final Matcher matchQkme1 = qkmePattern1.matcher(url);

		if(matchQkme1.find()) {
			final String imgId = matchQkme1.group(1);
			if(imgId.length() > 2) {
				return new ImageInfo(String.format(
						Locale.US,
						"http://i.qkme.me/%s.jpg",
						imgId), ImageInfo.MediaType.IMAGE, ImageInfo.HasAudio.NO_AUDIO);
			}
		}

		final Matcher matchQkme2 = qkmePattern2.matcher(url);

		if(matchQkme2.find()) {
			final String imgId = matchQkme2.group(1);
			if(imgId.length() > 2) {
				return new ImageInfo(String.format(
						Locale.US,
						"http://i.qkme.me/%s.jpg",
						imgId), ImageInfo.MediaType.IMAGE, ImageInfo.HasAudio.NO_AUDIO);
			}
		}

		final Matcher matchLvme = lvmePattern.matcher(url);

		if(matchLvme.find()) {
			final String imgId = matchLvme.group(1);
			if(imgId.length() > 2) {
				return new ImageInfo(String.format(
						Locale.US,
						"http://www.livememe.com/%s.jpg",
						imgId), ImageInfo.MediaType.IMAGE, ImageInfo.HasAudio.NO_AUDIO);
			}
		}

		return null;

	}

	public static LinkedHashSet<String> computeAllLinks(@NonNull final String text) {

		final LinkedHashSet<String> result = new LinkedHashSet<>();

		// From http://stackoverflow.com/a/1806161/1526861
		// TODO may not handle .co.uk, similar (but should handle .co/.us/.it/etc fine)
		final Pattern urlPattern = Pattern.compile(
				"\\b((((ht|f)tp(s?)\\:\\/\\/|~\\/|\\/)|www.)" +
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

		final Matcher subredditMatcher = Pattern.compile("(?<!\\w)(/?[ru]/\\w+)\\b")
				.matcher(text);

		while(subredditMatcher.find()) {
			result.add(subredditMatcher.group(1));
		}

		return result;
	}

	private static class LinkMenuItem {
		public final String title;
		public final LinkAction action;

		private LinkMenuItem(final Context context, final int titleRes, final LinkAction action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public static void shareText(
			@NonNull final AppCompatActivity activity,
			@Nullable final String subject,
			@Nullable String text) {

		if(text == null) {
			text = "<null>";
		}

		final Intent mailer = new Intent(Intent.ACTION_SEND);
		mailer.setType("text/plain");
		mailer.putExtra(Intent.EXTRA_TEXT, text);

		if(subject != null) {
			mailer.putExtra(Intent.EXTRA_SUBJECT, subject);
		}

		if(PrefsUtility.pref_behaviour_sharing_dialog()) {
			ShareOrderDialog.newInstance(mailer).show(
					activity.getSupportFragmentManager(),
					null);

		} else {
			activity.startActivity(Intent.createChooser(
					mailer,
					activity.getString(R.string.action_share)));
		}
	}

	public static Uri convertAndNormalizeUri(@NonNull String uri) {
		if(uri.startsWith("r/") || uri.startsWith("u/")) {
			uri = "/" + uri;
		}

		if(uri.startsWith("/")) {
			uri = "https://reddit.com" + uri;
		}

		if(!uri.contains("://") && !uri.startsWith("mailto:")) {
			uri = "http://" + uri;
		}

		final Uri parsedUri = Uri.parse(uri).normalizeScheme();
		final Uri.Builder uriBuilder = parsedUri.buildUpon();

		final String authority = parsedUri.getEncodedAuthority();
		if(authority != null) {
			final String normalAuthority;

			//Don't lowercase the rare userinfo component if present.
			if(authority.contains("@")) {
				final String[] authorityParts = authority.split("@", 2);
				normalAuthority =
						authorityParts[0] + "@" + StringUtils.asciiLowercase(authorityParts[1]);
			} else {
				normalAuthority = StringUtils.asciiLowercase(authority);
			}

			uriBuilder.encodedAuthority(normalAuthority);
		}

		return uriBuilder.build();
	}

	public static String getPreferredRedditUriString(final String uri) {
		final Uri parsedUri = convertAndNormalizeUri(uri);

		//Return non-Reddit links normalized but otherwise unaltered
		if (RedditURLParser.parse(parsedUri) == null) {
			return parsedUri.toString();
		}

		//Respect non-participation links
		if(parsedUri.getHost().equals("np.reddit.com")) {
			return parsedUri.toString();
		}

		final PostCommentListingURL potentialPostLink = PostCommentListingURL.parse(parsedUri);
		final String postId;
		if(potentialPostLink != null && potentialPostLink.commentId == null) {
			//Direct link to a post, not to a comment or anything else
			postId = potentialPostLink.postId;
		} else {
			postId = null;
		}

		final PrefsUtility.SharingDomain preferredDomain
				= PrefsUtility.pref_behaviour_sharing_domain();

		//Only direct links to posts will be converted to redd.it links
		if(preferredDomain == PrefsUtility.SharingDomain.SHORT_REDDIT && postId == null) {
			return parsedUri.toString();
		}

		final Uri.Builder uriBuilder = parsedUri.buildUpon();

		uriBuilder.encodedAuthority(preferredDomain.domain);
		if(preferredDomain == PrefsUtility.SharingDomain.SHORT_REDDIT) {
			uriBuilder.encodedPath("/" + postId);
		}

		return uriBuilder.build().toString();
	}
}
