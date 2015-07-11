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

package org.quantumbadger.redreader.adapters;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.preference.PreferenceManager;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.list.ListItemView;
import org.quantumbadger.redreader.views.list.ListSectionHeader;
import org.quantumbadger.redreader.views.list.MainMenuItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;

public class MainMenuAdapter extends BaseAdapter {

	private final ArrayList<MainMenuItem> mainItems = new ArrayList<MainMenuItem>(16);
	private final ArrayList<MainMenuItem> subredditItems = new ArrayList<MainMenuItem>(100);
	private final RedditAccount user;
	private final MainMenuSelectionListener selectionListener;

	private final Drawable rrIconPerson, rrIconEnvOpen, rrIconSend, rrIconStarFilled, rrIconCross, rrIconThumbUp, rrIconThumbDown;

	private final Context context;

	public MainMenuAdapter(final Context context, final RedditAccount user, final MainMenuSelectionListener selectionListener) {

		this.user = user;
		this.selectionListener = selectionListener;
		this.context = context;

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrIconPerson,
				R.attr.rrIconEnvOpen,
				R.attr.rrIconSend,
				R.attr.rrIconStarFilled,
				R.attr.rrIconCross,
				R.attr.rrIconThumbUp,
				R.attr.rrIconThumbDown
		});

		rrIconPerson = context.getResources().getDrawable(attr.getResourceId(0, 0));
		rrIconEnvOpen = context.getResources().getDrawable(attr.getResourceId(1, 0));
		rrIconSend = context.getResources().getDrawable(attr.getResourceId(2, 0));
		rrIconStarFilled = context.getResources().getDrawable(attr.getResourceId(3, 0));
		rrIconCross = context.getResources().getDrawable(attr.getResourceId(4, 0));
		rrIconThumbUp = context.getResources().getDrawable(attr.getResourceId(5, 0));
		rrIconThumbDown = context.getResources().getDrawable(attr.getResourceId(6, 0));

		build();
	}

	public int getCount() {
		return mainItems.size() + subredditItems.size();
	}

	public Object getItem(final int i) {
		return "item_" + i;
	}

	public long getItemId(final int i) {
		return i;
	}

	private MainMenuItem getItemInternal(final int id) {
		if(id < mainItems.size()) {
			return mainItems.get(id);
		} else {
			return subredditItems.get(id - mainItems.size());
		}
	}

	@Override
	public int getItemViewType(final int position) {
		return getItemInternal(position).isHeader ? 0 : 1;
	}

	@Override
	public int getViewTypeCount() {
		return 2;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	public View getView(final int i, View convertView, final ViewGroup viewGroup) {

		final MainMenuItem item = getItemInternal(i);

		if(convertView == null) {
			if(item.isHeader) {
				convertView = new ListSectionHeader(viewGroup.getContext());
			} else {
				convertView = new ListItemView(viewGroup.getContext());
			}
		}

		if(item.isHeader) {
			((ListSectionHeader)convertView).reset(item.title);
		} else {
			final boolean firstInSection = (i == 0) || getItemInternal(i - 1).isHeader;
			((ListItemView)convertView).reset(item.icon, item.title, firstInSection);
		}

		return convertView;
	}

	// Only run in UI thread
	private void build() {

		//items.add(new MainMenuItem("Reddit"));

		mainItems.add(makeItem(context.getString(R.string.mainmenu_frontpage), MainMenuFragment.MainMenuAction.FRONTPAGE, null, null));
		mainItems.add(makeItem(context.getString(R.string.mainmenu_all), MainMenuFragment.MainMenuAction.ALL, null, null));
		mainItems.add(makeItem(context.getString(R.string.mainmenu_custom), MainMenuFragment.MainMenuAction.CUSTOM, null, null));

		if(!user.isAnonymous()) {

			mainItems.add(new MainMenuItem(user.username));

			final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
			final EnumSet<MainMenuFragment.MainMenuUserItems> mainMenuUserItems
					= PrefsUtility.pref_menus_mainmenu_useritems(context, sharedPreferences);

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.PROFILE))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_profile), MainMenuFragment.MainMenuAction.PROFILE, null, rrIconPerson));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.INBOX))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_inbox), MainMenuFragment.MainMenuAction.INBOX, null, rrIconEnvOpen));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SUBMITTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_submitted), MainMenuFragment.MainMenuAction.SUBMITTED, null, rrIconSend));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SAVED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_saved), MainMenuFragment.MainMenuAction.SAVED, null, rrIconStarFilled));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.HIDDEN))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_hidden), MainMenuFragment.MainMenuAction.HIDDEN, null, rrIconCross));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.UPVOTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_upvoted), MainMenuFragment.MainMenuAction.UPVOTED, null, rrIconThumbUp));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.DOWNVOTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_downvoted), MainMenuFragment.MainMenuAction.DOWNVOTED, null, rrIconThumbDown));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.MODMAIL))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_modmail), MainMenuFragment.MainMenuAction.MODMAIL, null, rrIconEnvOpen));
		}

		mainItems.add(new MainMenuItem(context.getString(R.string.mainmenu_header_subreddits)));

		//items.add(makeItem("Add Subreddit", null, null, null)); // TODO

		notifyDataSetChanged();
	}

	private MainMenuItem makeItem(final int nameRes, final MainMenuFragment.MainMenuAction action, final String actionName, final Drawable icon) {
		return makeItem(context.getString(nameRes), action, actionName, icon);
	}

	private MainMenuItem makeItem(final String name, final MainMenuFragment.MainMenuAction action, final String actionName, final Drawable icon) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			public void onClick(final View view) {
				selectionListener.onSelected(action, actionName);
			}
		};

		return new MainMenuItem(name, icon, clickListener, null);
	}

	private MainMenuItem makeSubredditItem(final String name) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			public void onClick(final View view) {
				try {
					selectionListener.onSelected((PostListingURL) SubredditPostListURL.getSubreddit(RedditSubreddit.getCanonicalName(name)));
				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
			}
		};

		return new MainMenuItem(name, null, clickListener, null);
	}

	public void setSubreddits(final Collection<String> subscriptions) {

		final ArrayList<String> subscriptionsSorted = new ArrayList<String>(subscriptions);
		Collections.sort(subscriptionsSorted);

		General.UI_THREAD_HANDLER.post(new Runnable() {
			public void run() {

				subredditItems.clear();

				for(final String subreddit : subscriptionsSorted) {
					try {
						subredditItems.add(makeSubredditItem(RedditSubreddit.stripRPrefix(subreddit)));
					} catch(RedditSubreddit.InvalidSubredditNameException e) {
						subredditItems.add(makeSubredditItem("Invalid: " + subreddit));
					}
				}

				notifyDataSetChanged();
			}
		});
	}

	@Override
	public boolean areAllItemsEnabled() {
		return false;
	}

	@Override
	public boolean isEnabled(final int position) {
		return !getItemInternal(position).isHeader;
	}

	public void clickOn(final int position) {
		if(position < getCount()) {
			getItemInternal(position).onClick(null);
		}
	}
}
