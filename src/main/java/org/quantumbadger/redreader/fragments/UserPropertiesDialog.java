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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.os.Bundle;
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.things.RedditUser;

public final class UserPropertiesDialog extends PropertiesDialog {

	public static UserPropertiesDialog newInstance(final RedditUser user) {

		final UserPropertiesDialog pp = new UserPropertiesDialog();

		final Bundle args = new Bundle();
		args.putParcelable("user", user);
		pp.setArguments(args);

		return pp;
	}

	@Override
	protected String getTitle(final Context context) {
		return getArguments().<RedditUser>getParcelable("user").name;
	}

	@Override
	protected void prepare(
			@NonNull final BaseActivity context,
			@NonNull final LinearLayout items) {

		final RedditUser user = getArguments().getParcelable("user");

		items.addView(propView(
				context,
				R.string.props_id,
				user.id,
				true));

		if (user.created_utc != null) {
			items.addView(propView(
					context,
					R.string.userprofile_created,
					TimestampUTC.fromUtcSecs(user.created_utc).format(),
					false));
		}

		if (user.link_karma != null) {
			items.addView(propView(
					context,
					R.string.karma_link,
					String.valueOf(user.link_karma),
					false));
		}

		if (user.comment_karma != null) {
			items.addView(propView(
					context,
					R.string.karma_comment,
					String.valueOf(user.comment_karma),
					false));
		}

		if (user.is_friend != null) {
			items.addView(propView(
					context,
					R.string.userprofile_isfriend,
					user.is_friend ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.is_gold != null) {
			items.addView(propView(
					context,
					R.string.userprofile_isgold,
					user.is_gold ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.is_mod != null) {
			items.addView(propView(
					context,
					R.string.userprofile_moderator,
					user.is_mod ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.is_employee != null) {
			items.addView(propView(
					context,
					R.string.userprofile_tag_admin,
					user.is_employee ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.is_suspended != null) {
			items.addView(propView(
					context,
					R.string.userprofile_tag_suspended,
					user.is_suspended ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.is_blocked != null) {
			items.addView(propView(
					context,
					R.string.userprofile_tag_blocked,
					user.is_blocked ? R.string.general_true : R.string.general_false,
					false));
		}

		if (user.icon_img != null) {
			items.addView(propView(
					context,
					R.string.userprofile_avatar,
					user.icon_img,
					false));
		}
	}
}
