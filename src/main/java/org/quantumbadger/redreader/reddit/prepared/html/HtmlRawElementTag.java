package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;

import java.util.ArrayList;

public abstract class HtmlRawElementTag extends HtmlRawElement {

	public abstract void reduce(
			@NonNull HtmlTextAttributes activeAttributes,
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<HtmlReducedElement> destination);
}
