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

package org.quantumbadger.redreader.views;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.TextView;
import org.apache.commons.lang3.StringUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.reddit.url.SearchPostListURL;


public final class SearchListingHeader extends FrameLayout {

	final SearchPostListURL mUrl;
	final EditText mQuery;
	final EditText mLocation;
	final Button mSearchButton;

	public SearchListingHeader(
			final Activity parentActivity,
			final SearchPostListURL url) {
		super(parentActivity);
		mUrl = url;

		final LayoutInflater layoutInflater = (LayoutInflater)parentActivity.getSystemService(
				Context.LAYOUT_INFLATER_SERVICE);
		layoutInflater.inflate(R.layout.search_listing_header, this, true);

		mQuery = findViewById(R.id.search_listing_header_query_editText);
		mQuery.setText(url.query);
		mQuery.setImeOptions(EditorInfo.IME_ACTION_NEXT);

		mLocation = findViewById(R.id.search_listing_header_sub_editText);

		if(url.type == SearchPostListURL.Type.SUB_OR_SUB_COMBO && url.subreddit != null) {
			mLocation.setText(url.subreddit);
		} else if(url.type == SearchPostListURL.Type.MULTI && url.name != null) {
			if(url.username != null) {
				//noinspection SetTextI18n
				mLocation.setText("/u/" + url.username + "/m/" + url.name);
			} else {
				//noinspection SetTextI18n
				mLocation.setText("/me/m/" + url.name);
			}
		} else {
			// null and "all" are isomorphic; but SearchPostListURL takes null
			//noinspection SetTextI18n
			mLocation.setText("all");
		}

		final TextView.OnEditorActionListener onEnter = (v, actionId, event) -> {
			performSearch(parentActivity, mLocation, mQuery);
			return true;
		};
		mLocation.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
		mLocation.setOnEditorActionListener(onEnter);

		mSearchButton = findViewById(R.id.search_listing_header_search);
		mSearchButton.setOnClickListener(v -> performSearch(parentActivity, mLocation, mQuery));
	}

	private static void performSearch(
			final Activity parentActivity,
			final EditText mLocation,
			final EditText mQuery) {
		String location = mLocation.getText().toString().trim();
		if(StringUtils.isEmpty(location)) {
			location = null;
		}
		final SearchPostListURL url = SearchPostListURL.build(
				location,
				mQuery.getText().toString().trim());

		final Intent intent = new Intent(parentActivity, PostListingActivity.class);
		intent.setData(url.generateJsonUri());

		// Use a startActivity/finish combination to replace this activity with the new
		// search activity
		parentActivity.startActivity(intent);
		parentActivity.finish();
	}
}
