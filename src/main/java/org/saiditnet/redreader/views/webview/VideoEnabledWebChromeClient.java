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
 *
 * It originally was written by Cristian Perez (http://cpr.name),
 * see https://github.com/cprcrack/VideoEnabledWebView for more details.
 ******************************************************************************/
package org.saiditnet.redreader.views.webview;

import android.media.MediaPlayer;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebChromeClient;
import android.widget.FrameLayout;

/**
 * This class serves as a WebChromeClient to be set to a WebView, allowing it to play video.
 * Video will play differently depending on target API level (in-line, fullscreen, or both).
 * <p>
 * It has been tested with the following video classes:
 * - android.widget.VideoView (typically API level <11)
 * - android.webkit.HTML5VideoFullScreen$VideoSurfaceView/VideoTextureView (typically API level 11-18)
 * - com.android.org.chromium.content.browser.ContentVideoView$VideoSurfaceView (typically API level 19+)
 * <p>
 * Important notes:
 * - For API level 11+, android:hardwareAccelerated="true" must be set in the application manifest.
 * - The invoking activity must call VideoEnabledWebChromeClient's onBackPressed() inside of its own onBackPressed().
 * - Tested in Android API levels 8-19. Only tested on http://m.youtube.com.
 *
 * For more information, see https://github.com/cprcrack/VideoEnabledWebView
 * @author Cristian Perez (http://cpr.name)
 */

// Taken from cprcrack/VideoEnabledWebView
// https://github.com/cprcrack/VideoEnabledWebView/blob/master/app/src/main/java/name/cpr/VideoEnabledWebChromeClient.java

public class VideoEnabledWebChromeClient extends WebChromeClient implements MediaPlayer.OnPreparedListener, MediaPlayer.OnCompletionListener, MediaPlayer.OnErrorListener {
	public interface ToggledFullscreenCallback {
		void toggledFullscreen(boolean fullscreen);
	}

	private View activityNonVideoView;
	private ViewGroup activityVideoView;
	private View loadingView;
	private WebViewFixed webView;

	private boolean isVideoFullscreen; // Indicates if the video is being displayed using a custom view (typically full-screen)
	private FrameLayout videoViewContainer;
	private CustomViewCallback videoViewCallback;

	private ToggledFullscreenCallback toggledFullscreenCallback;

	/**
	 * Never use this constructor alone.
	 * This constructor allows this class to be defined as an inline inner class in which the user can override methods
	 */
	@SuppressWarnings("unused")
	public VideoEnabledWebChromeClient() {
	}

	/**
	 * Builds a video enabled WebChromeClient.
	 *
	 * @param activityNonVideoView A View in the activity's layout that contains every other view that should be hidden when the video goes full-screen.
	 * @param activityVideoView    A ViewGroup in the activity's layout that will display the video. Typically you would like this to fill the whole layout.
	 */
	@SuppressWarnings("unused")
	public VideoEnabledWebChromeClient(View activityNonVideoView, ViewGroup activityVideoView) {
		this.activityNonVideoView = activityNonVideoView;
		this.activityVideoView = activityVideoView;
		this.loadingView = null;
		this.webView = null;
		this.isVideoFullscreen = false;
	}

	/**
	 * Builds a video enabled WebChromeClient.
	 *
	 * @param activityNonVideoView A View in the activity's layout that contains every other view that should be hidden when the video goes full-screen.
	 * @param activityVideoView    A ViewGroup in the activity's layout that will display the video. Typically you would like this to fill the whole layout.
	 * @param loadingView          A View to be shown while the video is loading (typically only used in API level <11). Must be already inflated and not attached to a parent view.
	 */
	@SuppressWarnings("unused")
	public VideoEnabledWebChromeClient(View activityNonVideoView, ViewGroup activityVideoView, View loadingView) {
		this.activityNonVideoView = activityNonVideoView;
		this.activityVideoView = activityVideoView;
		this.loadingView = loadingView;
		this.webView = null;
		this.isVideoFullscreen = false;
	}

	/**
	 * Builds a video enabled WebChromeClient.
	 *
	 * @param activityNonVideoView A View in the activity's layout that contains every other view that should be hidden when the video goes full-screen.
	 * @param activityVideoView    A ViewGroup in the activity's layout that will display the video. Typically you would like this to fill the whole layout.
	 * @param loadingView          A View to be shown while the video is loading (typically only used in API level <11). Must be already inflated and not attached to a parent view.
	 * @param webView              The owner WebViewFixed. Passing it will enable the VideoEnabledWebChromeClient to detect the HTML5 video ended event and exit full-screen.
	 *                             Note: The web page must only contain one video tag in order for the HTML5 video ended event to work. This could be improved if needed (see Javascript code).
	 */
	@SuppressWarnings("unused")
	public VideoEnabledWebChromeClient(View activityNonVideoView, ViewGroup activityVideoView, View loadingView, WebViewFixed webView) {
		this.activityNonVideoView = activityNonVideoView;
		this.activityVideoView = activityVideoView;
		this.loadingView = loadingView;
		this.webView = webView;
		this.isVideoFullscreen = false;
	}

	/**
	 * Indicates if the video is being displayed using a custom view (typically full-screen)
	 *
	 * @return true it the video is being displayed using a custom view (typically full-screen)
	 */
	public boolean isVideoFullscreen() {
		return isVideoFullscreen;
	}

	/**
	 * Set a callback that will be fired when the video starts or finishes displaying using a custom view (typically full-screen)
	 *
	 * @param callback A VideoEnabledWebChromeClient.ToggledFullscreenCallback callback
	 */
	@SuppressWarnings("unused")
	public void setOnToggledFullscreen(ToggledFullscreenCallback callback) {
		this.toggledFullscreenCallback = callback;
	}

	@Override
	public void onShowCustomView(View view, CustomViewCallback callback) {
		if (view instanceof FrameLayout) {
			// A video wants to be shown
			FrameLayout frameLayout = (FrameLayout) view;
			View focusedChild = frameLayout.getFocusedChild();

			// Save video related variables
			this.isVideoFullscreen = true;
			this.videoViewContainer = frameLayout;
			this.videoViewCallback = callback;

			// Hide the non-video view, add the video view, and show it
			activityNonVideoView.setVisibility(View.INVISIBLE);
			activityVideoView.addView(videoViewContainer, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));
			activityVideoView.setVisibility(View.VISIBLE);

			if (focusedChild instanceof android.widget.VideoView) {
				// android.widget.VideoView (typically API level <11)
				android.widget.VideoView videoView = (android.widget.VideoView) focusedChild;

				// Handle all the required events
				videoView.setOnPreparedListener(this);
				videoView.setOnCompletionListener(this);
				videoView.setOnErrorListener(this);
			} else {
				// Other classes, including:
				// - android.webkit.HTML5VideoFullScreen$VideoSurfaceView, which inherits from android.view.SurfaceView (typically API level 11-18)
				// - android.webkit.HTML5VideoFullScreen$VideoTextureView, which inherits from android.view.TextureView (typically API level 11-18)
				// - com.android.org.chromium.content.browser.ContentVideoView$VideoSurfaceView, which inherits from android.view.SurfaceView (typically API level 19+)

				// Handle HTML5 video ended event only if the class is a SurfaceView
				// Test case: TextureView of Sony Xperia T API level 16 doesn't work fullscreen when loading the javascript below
				if (webView != null && webView.getSettings().getJavaScriptEnabled() && focusedChild instanceof SurfaceView) {
					// Run javascript code that detects the video end and notifies the Javascript interface
					String js = "javascript:";
					js += "var _ytrp_html5_video_last;";
					js += "var _ytrp_html5_video = document.getElementsByTagName('video')[0];";
					js += "if (_ytrp_html5_video != undefined && _ytrp_html5_video != _ytrp_html5_video_last) {";
					{
						js += "_ytrp_html5_video_last = _ytrp_html5_video;";
						js += "function _ytrp_html5_video_ended() {";
						{
							js += "_WebViewFixed.notifyVideoEnd();"; // Must match Javascript interface name and method of VideoEnableWebView
						}
						js += "}";
						js += "_ytrp_html5_video.addEventListener('ended', _ytrp_html5_video_ended);";
					}
					js += "}";
					webView.loadUrl(js);
				}
			}

			// Notify full-screen change
			if (toggledFullscreenCallback != null) {
				toggledFullscreenCallback.toggledFullscreen(true);
			}
		}
	}

	@Override
	@SuppressWarnings("deprecation")
	public void onShowCustomView(View view, int requestedOrientation, CustomViewCallback callback) // Available in API level 14+, deprecated in API level 18+
	{
		onShowCustomView(view, callback);
	}

	@Override
	public void onHideCustomView() {
		// This method should be manually called on video end in all cases because it's not always called automatically.
		// This method must be manually called on back key press (from this class' onBackPressed() method).

		if (isVideoFullscreen) {
			// Hide the video view, remove it, and show the non-video view
			activityVideoView.setVisibility(View.INVISIBLE);
			activityVideoView.removeView(videoViewContainer);
			activityNonVideoView.setVisibility(View.VISIBLE);

			// Call back (only in API level <19, because in API level 19+ with chromium webview it crashes)
			if (videoViewCallback != null && !videoViewCallback.getClass().getName().contains(".chromium.")) {
				videoViewCallback.onCustomViewHidden();
			}

			// Reset video related variables
			isVideoFullscreen = false;
			videoViewContainer = null;
			videoViewCallback = null;

			// Notify full-screen change
			if (toggledFullscreenCallback != null) {
				toggledFullscreenCallback.toggledFullscreen(false);
			}
		}
	}

	@Override
	public View getVideoLoadingProgressView() // Video will start loading
	{
		if (loadingView != null) {
			loadingView.setVisibility(View.VISIBLE);
			return loadingView;
		} else {
			return super.getVideoLoadingProgressView();
		}
	}

	@Override
	public void onPrepared(MediaPlayer mp) // Video will start playing, only called in the case of android.widget.VideoView (typically API level <11)
	{
		if (loadingView != null) {
			loadingView.setVisibility(View.GONE);
		}
	}

	@Override
	public void onCompletion(MediaPlayer mp) // Video finished playing, only called in the case of android.widget.VideoView (typically API level <11)
	{
		onHideCustomView();
	}

	@Override
	public boolean onError(MediaPlayer mp, int what, int extra) // Error while playing video, only called in the case of android.widget.VideoView (typically API level <11)
	{
		return false; // By returning false, onCompletion() will be called
	}

	/**
	 * Notifies the class that the back key has been pressed by the user.
	 * This must be called from the Activity's onBackPressed(), and if it returns false, the activity itself should handle it. Otherwise don't do anything.
	 *
	 * @return Returns true if the event was handled, and false if was not (video view is not visible)
	 */
	@SuppressWarnings("unused")
	public boolean onBackPressed() {
		if (isVideoFullscreen) {
			onHideCustomView();
			return true;
		} else {
			return false;
		}
	}

}
