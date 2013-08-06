package org.quantumbadger.redreader.ui.settings;

import android.net.Uri;
import android.os.Bundle;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.ui.RRContext;
import org.quantumbadger.redreader.ui.frag.RRUriHandler;

public class PrefsUriHandler extends RRUriHandler {

	@Override
	public Result handle(RRContext context, Uri uri, Mode mode, Bundle arguments) {

		if(!uri.getScheme().equals(Constants.Internal.URI_SCHEME)
				|| !uri.getHost().equals(Constants.Internal.URI_HOST_PREFSPAGE)) return null;

		return new Result(new PrefsPageFragment(context, uri, null, null));
	}
}
