package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElement;

import java.util.ArrayList;

public abstract class HtmlRawElementTag extends HtmlRawElement {

	public abstract void reduce(
			@NonNull HtmlTextAttributes activeAttributes,
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<BodyTextElement> destination);
}
