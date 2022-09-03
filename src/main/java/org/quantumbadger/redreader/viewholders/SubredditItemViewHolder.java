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

package org.quantumbadger.redreader.viewholders;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.reddit.SubredditDetails;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader;
import org.quantumbadger.redreader.views.SubredditToolbar;

import java.text.NumberFormat;
import java.util.Locale;

public class SubredditItemViewHolder extends RecyclerView.ViewHolder {

	private final BaseActivity mActivity;
	private final RRThemeAttributes mTheme;
	private final float mBodyFontScale;

	private final TextView mPrimaryText;
	private final TextView mSubText;
	private final FrameLayout mSupportingText;
	private final SubredditToolbar mActions;
	private final View mGoButton;

	public SubredditItemViewHolder(
			@NonNull final ViewGroup parent,
			final BaseActivity activity) {

		super(LayoutInflater.from(parent.getContext())
				.inflate(R.layout.subreddit_item_view, parent, false));

		mActivity = activity;
		mTheme = new RRThemeAttributes(activity);
		mBodyFontScale = PrefsUtility.appearance_fontscale_bodytext();

		mPrimaryText = this.itemView.findViewById(R.id.subreddit_item_view_primary_text);
		mSubText = this.itemView.findViewById(R.id.subreddit_item_view_sub_text);
		mSupportingText = this.itemView.findViewById(R.id.subreddit_item_view_supporting_text);
		mActions = this.itemView.findViewById(R.id.subreddit_item_view_actions);
		mGoButton = this.itemView.findViewById(R.id.subreddit_item_view_go);
	}

	public void bind(@NonNull final SubredditDetails subreddit) {

		mPrimaryText.setText(subreddit.name);

		final String subtitle;
		if(subreddit.subscribers == null) {
			subtitle = null;
		} else {
			subtitle = mActivity.getString(
					R.string.header_subscriber_count,
					NumberFormat.getNumberInstance(Locale.getDefault())
							.format(subreddit.subscribers));
		}

		if(subtitle == null) {
			mSubText.setVisibility(View.GONE);
		} else {
			mSubText.setVisibility(View.VISIBLE);
			mSubText.setText(subtitle);
		}

		mSupportingText.removeAllViews();

		if(subreddit.publicDescriptionHtmlEscaped != null
				&& !subreddit.publicDescriptionHtmlEscaped.trim().isEmpty()) {

			final BodyElement body = HtmlReader.parse(
					StringEscapeUtils.unescapeHtml4(subreddit.publicDescriptionHtmlEscaped),
					mActivity);

			mSupportingText.setVisibility(View.VISIBLE);

			mSupportingText.addView(body.generateView(
					mActivity,
					mTheme.rrCommentBodyCol,
					13.0f * mBodyFontScale,
					false));
		} else {
			mSupportingText.setVisibility(View.GONE);
		}

		mActions.bindSubreddit(subreddit, Optional.empty());

		mGoButton.setOnClickListener(
				v -> LinkHandler.onLinkClicked(mActivity, subreddit.url));
	}
}
