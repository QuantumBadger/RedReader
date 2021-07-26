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

import android.view.LayoutInflater;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.receivers.announcements.Announcement;
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader;

public class AnnouncementView extends FrameLayout {

	public AnnouncementView(
			@NonNull final AppCompatActivity activity,
			@NonNull final Announcement announcement) {

		super(activity);

		LayoutInflater.from(activity)
				.inflate(R.layout.announcement_view, this, true);

		final TextView textTitle = findViewById(R.id.announcement_view_title);
		final TextView textMessage = findViewById(R.id.announcement_view_message);

		final Button buttonView = findViewById(R.id.announcement_view_button_view);
		final Button buttonDismiss = findViewById(R.id.announcement_view_button_dismiss);

		textTitle.setText(announcement.title);

		if(announcement.message == null) {
			textMessage.setVisibility(GONE);
		} else {
			textMessage.setText(announcement.message);
		}

		buttonView.setOnClickListener(v -> {
			LinkHandler.onLinkClicked(activity, announcement.url);
			new RRAnimationShrinkHeight(this).start();
			AnnouncementDownloader.markAsRead(activity, announcement);
		});

		buttonDismiss.setOnClickListener(v -> {
			new RRAnimationShrinkHeight(this).start();
			AnnouncementDownloader.markAsRead(activity, announcement);
		});
	}
}
