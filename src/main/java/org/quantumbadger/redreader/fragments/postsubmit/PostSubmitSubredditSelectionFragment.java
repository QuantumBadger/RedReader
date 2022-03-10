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

package org.quantumbadger.redreader.fragments.postsubmit;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.streams.Stream;
import org.quantumbadger.redreader.reddit.RedditSubredditHistory;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.viewholders.VH1Text;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Objects;

public class PostSubmitSubredditSelectionFragment extends Fragment {

	public static class Args {

		@NonNull private static final String KEY_SUBREDDIT = "subreddit";

		@Nullable public final SubredditCanonicalId subreddit;

		public Args(@Nullable final SubredditCanonicalId subreddit) {
			this.subreddit = subreddit;
		}

		@NonNull
		public Bundle toBundle() {
			final Bundle result = new Bundle(1);
			if(subreddit != null) {
				result.putParcelable(KEY_SUBREDDIT, subreddit);
			}
			return result;
		}

		@NonNull
		public static Args fromBundle(@NonNull final Bundle bundle) {
			return new Args(bundle.getParcelable(KEY_SUBREDDIT));
		}
	}

	public interface Listener {

		void onSubredditSelected(
				@NonNull String username,
				@NonNull SubredditCanonicalId subreddit);

		void onNotLoggedIn();
	}

	private static class AutocompleteEntry {

		public final long listId;
		@NonNull public final String nameWithoutPrefix;

		private AutocompleteEntry(final long listId, @NonNull final String nameWithoutPrefix) {
			this.listId = listId;
			this.nameWithoutPrefix = nameWithoutPrefix;
		}
	}

	private class AutocompleteAdapter extends RecyclerView.Adapter<VH1Text> {

		@NonNull private final ArrayList<AutocompleteEntry>
				mAllSuggestions = new ArrayList<>();

		@NonNull private final ArrayList<AutocompleteEntry> mCurrentSuggestions = new ArrayList<>();

		public AutocompleteAdapter(final Context context) {

			super();

			setHasStableIds(true);

			final ArrayList<SubredditCanonicalId> allSuggestions
					= RedditSubredditHistory.getSubredditsSorted(
					RedditAccountManager.getInstance(context)
							.getDefaultAccount());

			for(int i = 0; i < allSuggestions.size(); i++) {
				mAllSuggestions.add(new AutocompleteEntry(
						i,
						allSuggestions.get(i).getDisplayNameLowercase()));
			}

			mCurrentSuggestions.addAll(mAllSuggestions);
		}

		@SuppressLint("NotifyDataSetChanged")
		public void updateSuggestions() {

			mCurrentSuggestions.clear();

			final String currentText = StringUtils.asciiLowercase(
					mSubredditBox.getText().toString().trim());

			final String searchString;

			try {
				searchString = RedditSubreddit.stripRPrefix(currentText);

			} catch(final InvalidSubredditNameException e) {
				mCurrentSuggestions.addAll(mAllSuggestions);
				notifyDataSetChanged();
				return;
			}

			final ArrayList<AutocompleteEntry> possibleSuggestions
					= new ArrayList<>(mAllSuggestions);

			{
				final Iterator<AutocompleteEntry> it = possibleSuggestions.iterator();

				while(it.hasNext()) {
					final AutocompleteEntry entry = it.next();

					if(entry.nameWithoutPrefix.startsWith(searchString)) {
						mCurrentSuggestions.add(entry);
						it.remove();
					}
				}
			}

			{
				final Iterator<AutocompleteEntry> it = possibleSuggestions.iterator();

				while(it.hasNext()) {
					final AutocompleteEntry entry = it.next();

					if(entry.nameWithoutPrefix.contains(searchString)) {
						mCurrentSuggestions.add(entry);
						it.remove();
					}
				}
			}

			mCurrentSuggestions.addAll(possibleSuggestions);

			notifyDataSetChanged();
			scrollToTop();
		}

		@NonNull
		@Override
		public VH1Text onCreateViewHolder(@NonNull final ViewGroup viewGroup, final int i) {

			final View view = LayoutInflater.from(viewGroup.getContext())
					.inflate(R.layout.list_item_1_text, viewGroup, false);

			final VH1Text result = new VH1Text(view);

			view.setOnClickListener(v -> mSubredditBox.setText(result.text.getText()));

			return result;
		}

		@Override
		public void onBindViewHolder(
				@NonNull final VH1Text viewHolder,
				final int i) {

			viewHolder.text.setText(mCurrentSuggestions.get(i).nameWithoutPrefix);
		}

		@Override
		public int getItemCount() {
			return mCurrentSuggestions.size();
		}

		@Override
		public long getItemId(final int position) {
			return mCurrentSuggestions.get(position).listId;
		}
	}

	private Spinner mUsernameSpinner;
	private EditText mSubredditBox;

	private RecyclerView mAutocompleteSuggestions;
	private RecyclerView.LayoutManager mAutocompleteSuggestionsLayout;

	@Override
	public void onResume() {
		super.onResume();

		final FragmentActivity activity = getActivity();

		if(activity != null) {
			activity.setTitle(R.string.subreddit_selector_title);
		}
	}

	@Nullable
	@Override
	public View onCreateView(
			@NonNull final LayoutInflater inflater,
			@Nullable final ViewGroup container,
			@Nullable final Bundle savedInstanceState) {

		final Args args = Args.fromBundle(requireArguments());

		final Context context = Objects.requireNonNull(container).getContext();

		final View root = inflater.inflate(R.layout.subreddit_selection, container, false);

		mUsernameSpinner = root.findViewById(R.id.subreddit_selection_account);
		mSubredditBox = root.findViewById(R.id.subreddit_selection_textbox);

		mAutocompleteSuggestions = root.findViewById(R.id.subreddit_selection_autocomplete);
		mAutocompleteSuggestionsLayout
				= new LinearLayoutManager(context, RecyclerView.VERTICAL, false);

		mAutocompleteSuggestions.setLayoutManager(mAutocompleteSuggestionsLayout);

		final AutocompleteAdapter adapter = new AutocompleteAdapter(context);

		mAutocompleteSuggestions.setAdapter(adapter);

		AndroidCommon.onTextChanged(mSubredditBox, adapter::updateSuggestions);
		AndroidCommon.onSelectedItemChanged(mUsernameSpinner, adapter::updateSuggestions);

		final RedditAccountManager accountManager = RedditAccountManager.getInstance(context);

		final ArrayList<String> usernames = new ArrayList<>();

		Stream.from(accountManager.getAccounts())
				.filter(RedditAccount::isNotAnonymous)
				.forEach(account -> usernames.add(account.username));

		if(usernames.isEmpty()) {
			final FragmentActivity activity = getActivity();

			if(activity != null) {
				((Listener)activity).onNotLoggedIn();
			}

			return null;
		}

		mUsernameSpinner.setAdapter(new ArrayAdapter<>(
				context,
				android.R.layout.simple_list_item_1,
				usernames));

		{
			final Button continueButton
					= root.findViewById(R.id.subreddit_selection_button_continue);

			continueButton.setOnClickListener(v -> {

				final FragmentActivity activity = getActivity();

				if(activity == null) {
					return;
				}

				final SubredditCanonicalId subreddit;


				try {
					subreddit = new SubredditCanonicalId(mSubredditBox.getText().toString());

				} catch(final InvalidSubredditNameException e) {

					final Context applicationContext = activity.getApplicationContext();

					General.showResultDialog((AppCompatActivity)activity, new RRError(
							applicationContext.getString(R.string.invalid_subreddit_name),
							applicationContext.getString(R.string.invalid_subreddit_name_message),
							false,
							e));

					return;
				}

				((Listener)activity).onSubredditSelected(
						(String)mUsernameSpinner.getSelectedItem(),
						subreddit);
			});
		}

		if(args.subreddit != null) {
			mSubredditBox.setText(args.subreddit.getDisplayNameLowercase());
			adapter.updateSuggestions();
		}

		return root;
	}

	private void scrollToTop() {
		mAutocompleteSuggestionsLayout.smoothScrollToPosition(mAutocompleteSuggestions, null, 0);
	}
}
