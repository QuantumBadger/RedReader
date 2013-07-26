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

package org.quantumbadger.redreader.ui.frag;

import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.view.View;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public abstract class RRFragment {

	public static final int UNDEFINED = -1;

	private final Uri uri;
	private final Bundle args;
	private final Parcelable state;
	private View contentView;

	private final RRFragmentLayout fragmentManager;

	public RRFragment(RRFragmentLayout fragmentManager, Uri uri, Bundle args, Parcelable state) {
		this.fragmentManager = fragmentManager;
		this.uri = uri;
		this.args = args;
		this.state = state;
	}

	public static RRFragment restore(final RRFragmentLayout fragmentManager, final Bundle bundle)
			throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException,
			InvocationTargetException, InstantiationException {

		final Uri uri = Uri.parse(bundle.getString("uri"));
		final Bundle args = bundle.getBundle("args");
		final Parcelable state = bundle.getParcelable("state");

		@SuppressWarnings("unchecked")
		final Class<? extends RRFragment> fragType = (Class<? extends RRFragment>) Class.forName(bundle.getString("fragType"));

		final Constructor<? extends RRFragment> fragConstructor = fragType.getConstructor(RRFragmentLayout.class, Uri.class, Bundle.class, Parcelable.class);
		return fragConstructor.newInstance(fragmentManager, uri, args,  bundle);
	}

	public final Bundle saveState() {

		final Bundle bundle = new Bundle(4);

		bundle.putString("uri", uri.toString());
		bundle.putBundle("args", args);
		bundle.putParcelable("state", state);
		bundle.putString("fragType", this.getClass().getCanonicalName());

		return bundle;
	}

	public final Uri getUri() {
		return uri;
	}

	@Override
	public String toString() {
		return uri.toString();
	}

	public boolean showActionbarWhenFullscreen() {
		return true;
	}

	public boolean isInitiallyFullscreen() {
		return false;
	}

	public int minWidthPx(final float dpScale) {
		return (int) (100 * dpScale);
	}

	public int maxWidthPx(final float dpScale) {
		return (int) (1000 * dpScale);
	}

	public abstract int preferredWidthLeftcolPx(final float dpScale);

	// TODO prepare actionbar

	// TODO prepare view

	protected abstract View buildContentView();

	public final View getContentView() {
		if(contentView == null) {
			contentView = buildContentView();
		}

		return contentView;
	}

	public final void open(Uri uri) {
		open(uri, RRUriHandler.Mode.ANY, null);
	}

	public final void open(Uri uri, Bundle arguments) {
		open(uri, RRUriHandler.Mode.ANY, arguments);
	}

	public final void open(Uri uri, RRUriHandler.Mode mode) {
		open(uri, mode, null);
	}

	public final void open(Uri uri, RRUriHandler.Mode mode, Bundle arguments) {
		fragmentManager.handleUri(this, uri, mode, arguments);
	}

	public final void finish() {
		fragmentManager.removeFragmentsAfter(this);
	}
}
