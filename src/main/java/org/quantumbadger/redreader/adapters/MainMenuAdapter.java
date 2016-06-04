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
import android.support.annotation.UiThread;
import android.support.v4.content.ContextCompat;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.list.ListItemView;
import org.quantumbadger.redreader.views.list.MainMenuItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

public class MainMenuAdapter extends BaseAdapter {

	private final ArrayList<MainMenuItem> mainItems = new ArrayList<>(16);
	private final ArrayList<MainMenuItem> subredditItems = new ArrayList<>(100);
	private final RedditAccount user;
	private final MainMenuSelectionListener selectionListener;

	private final Drawable rrIconPerson, rrIconEnvOpen, rrIconSend, rrIconStarFilled, rrIconCross, rrIconUpvote, rrIconDownvote;

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
				R.attr.rrIconUpvote,
				R.attr.rrIconDownvote
		});

		rrIconPerson = ContextCompat.getDrawable(context, attr.getResourceId(0, 0));
		rrIconEnvOpen = ContextCompat.getDrawable(context, attr.getResourceId(1, 0));
		rrIconSend = ContextCompat.getDrawable(context, attr.getResourceId(2, 0));
		rrIconStarFilled = ContextCompat.getDrawable(context, attr.getResourceId(3, 0));
		rrIconCross = ContextCompat.getDrawable(context, attr.getResourceId(4, 0));
		rrIconUpvote = ContextCompat.getDrawable(context, attr.getResourceId(5, 0));
		rrIconDownvote = ContextCompat.getDrawable(context, attr.getResourceId(6, 0));

		attr.recycle();

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
				convertView = LayoutInflater.from(context)
					.inflate(R.layout.list_sectionheader, viewGroup, false);
			} else {
				convertView = new ListItemView(viewGroup.getContext());
			}
		}

		if(item.isHeader) {
			((TextView) convertView).setText(item.title);
		} else {
			final boolean firstInSection = (i == 0) || getItemInternal(i - 1).isHeader;
			((ListItemView)convertView).reset(item.icon, item.title, firstInSection);
		}

		return convertView;
	}

	@UiThread
	private void build() {

		//items.add(new MainMenuItem("Reddit"));

		mainItems.add(makeItem(context.getString(R.string.mainmenu_frontpage), MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE, null, null));
		mainItems.add(makeItem(context.getString(R.string.mainmenu_all), MainMenuFragment.MENU_MENU_ACTION_ALL, null, null));
		mainItems.add(makeItem(context.getString(R.string.mainmenu_custom), MainMenuFragment.MENU_MENU_ACTION_CUSTOM, null, null));

		if(!user.isAnonymous()) {

			if(PrefsUtility.pref_appearance_hide_username_main_menu(context, PreferenceManager.getDefaultSharedPreferences(context))) {
				mainItems.add(new MainMenuItem(context.getString(R.string.mainmenu_useritems)));
			} else {
				mainItems.add(new MainMenuItem(user.username));
			}

			final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
			final EnumSet<MainMenuFragment.MainMenuUserItems> mainMenuUserItems
					= PrefsUtility.pref_menus_mainmenu_useritems(context, sharedPreferences);

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.PROFILE))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_profile), MainMenuFragment.MENU_MENU_ACTION_PROFILE, null, rrIconPerson));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.INBOX))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_inbox), MainMenuFragment.MENU_MENU_ACTION_INBOX, null, rrIconEnvOpen));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SUBMITTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_submitted), MainMenuFragment.MENU_MENU_ACTION_SUBMITTED, null, rrIconSend));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SAVED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_saved), MainMenuFragment.MENU_MENU_ACTION_SAVED, null, rrIconStarFilled));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.HIDDEN))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_hidden), MainMenuFragment.MENU_MENU_ACTION_HIDDEN, null, rrIconCross));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.UPVOTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_upvoted), MainMenuFragment.MENU_MENU_ACTION_UPVOTED, null, rrIconUpvote));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.DOWNVOTED))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_downvoted), MainMenuFragment.MENU_MENU_ACTION_DOWNVOTED, null, rrIconDownvote));

			if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.MODMAIL))
				mainItems.add(makeItem(context.getString(R.string.mainmenu_modmail), MainMenuFragment.MENU_MENU_ACTION_MODMAIL, null, rrIconEnvOpen));
		}

		final List<String> pinnedSubreddits
				= PrefsUtility.pref_pinned_subreddits(context, PreferenceManager.getDefaultSharedPreferences(context));

		if(!pinnedSubreddits.isEmpty()) {
			mainItems.add(new MainMenuItem(context.getString(R.string.mainmenu_header_subreddits_pinned)));

			for(final String sr : pinnedSubreddits) {
				mainItems.add(makeSubredditItem(sr));
			}
		}

		mainItems.add(new MainMenuItem(context.getString(R.string.mainmenu_header_subreddits_subscribed)));

		//items.add(makeItem("Add Subreddit", null, null, null)); // TODO

		notifyDataSetChanged();
	}

	private MainMenuItem makeItem(final int nameRes, final @MainMenuFragment.MainMenuAction int action, final String actionName, final Drawable icon) {
		return makeItem(context.getString(nameRes), action, actionName, icon);
	}

	private MainMenuItem makeItem(final String name, final @MainMenuFragment.MainMenuAction int action, final String actionName, final Drawable icon) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			@Override
			public void onClick(final View view) {
				selectionListener.onSelected(action, actionName);
			}
		};

		return new MainMenuItem(name, icon, clickListener, null);
	}

	private MainMenuItem makeSubredditItem(final String name) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			@Override
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

		final ArrayList<String> subscriptionsSorted = new ArrayList<>(subscriptions);
		Collections.sort(subscriptionsSorted);

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
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
