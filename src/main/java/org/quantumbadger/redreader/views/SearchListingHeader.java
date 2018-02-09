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
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.FrameLayout;

import org.apache.commons.lang3.StringUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.PostListingActivity;
import org.quantumbadger.redreader.reddit.url.SearchPostListURL;


public final class SearchListingHeader extends FrameLayout implements TextWatcher  {

	SearchPostListURL mUrl;
	EditText mQuery;
	EditText mSubreddit;
	Button mSearchButton;

	public SearchListingHeader(final Activity parentActivity, final SearchPostListURL url) {
		super(parentActivity);
		mUrl = url;

		LayoutInflater layoutInflater = (LayoutInflater) parentActivity.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		layoutInflater.inflate(R.layout.search_listing_header, this, true);

		mQuery = (EditText) findViewById(R.id.search_listing_header_query_editText);
		mQuery.setText(url.query);
		mQuery.addTextChangedListener(this);

		mSubreddit = (EditText) findViewById(R.id.search_listing_header_sub_editText);
		// null and "all" are isomorphic; but SearchPostListURL takes null
        if (url.subreddit == null) {
			mSubreddit.setText("all");
		} else {
			mSubreddit.setText(url.subreddit);
		}
		mSubreddit.addTextChangedListener(this);

		mSearchButton = (Button) findViewById(R.id.search_listing_header_search);
		mSearchButton.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				String subreddit = mSubreddit.getText().toString().trim();
				if (StringUtils.isEmpty(subreddit)) {
					subreddit = null;
				}
				SearchPostListURL url = SearchPostListURL.build(subreddit, mQuery.getText().toString().trim());

				final Intent intent = new Intent(parentActivity, PostListingActivity.class);
				intent.setData(url.generateJsonUri());

				// Use a startActivity/finish combination to replace this activity with the new
				// search activity
				parentActivity.startActivity(intent);
				parentActivity.finish();
			}
		});
		updateSearchButtonEnabled();
	}

	private void updateSearchButtonEnabled() {
		String query = mQuery.getText().toString();
		String subreddit = mSubreddit.getText().toString();
		boolean isSubredditEqual = mUrl.subreddit == null
				?  (StringUtils.isEmpty(subreddit) || subreddit.equals("all"))
				: subreddit.equals(mUrl.subreddit);
		boolean isSameAsUrl = query.equals(mUrl.query) && isSubredditEqual;
		mSearchButton.setEnabled(!isSameAsUrl);
	}

	@Override
	public void afterTextChanged(Editable editable) {
		updateSearchButtonEnabled();
	}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

	@Override
	public void onTextChanged(CharSequence s, int start, int before, int count) {}
}
