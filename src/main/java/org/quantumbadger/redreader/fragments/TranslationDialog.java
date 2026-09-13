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

import android.app.Dialog;
import android.content.Intent;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDialogFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textview.MaterialTextView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.settings.SettingsActivity;
import org.quantumbadger.redreader.translation.LocalTranslation;
import org.quantumbadger.redreader.translation.TranslationRequest;
import org.quantumbadger.redreader.translation.TranslationService;

import java.util.concurrent.Future;

/** Shows translated text separately, preserving the original Reddit data and formatting. */
public final class TranslationDialog extends AppCompatDialogFragment {

	private TextView output;
	private ProgressBar progress;
	private Future<String> task;
	private String translation;
	private boolean autoStart;

	public static void show(final AppCompatActivity activity, final String text) {
		if(text == null || text.trim().isEmpty()) {
			General.quickToast(activity, R.string.translation_empty);
			return;
		}
		if(activity.getSupportFragmentManager().isStateSaved()) {
			return;
		}
		final TranslationDialog dialog = new TranslationDialog();
		final Bundle args = new Bundle();
		args.putString("text", text);
		args.putString("language", LocalTranslation.getTargetLanguage(activity));
		dialog.setArguments(args);
		dialog.show(activity.getSupportFragmentManager(), "translation");
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(@Nullable final Bundle savedInstanceState) {
		final AppCompatActivity activity = (AppCompatActivity)requireActivity();
		final LinearLayout content = new LinearLayout(activity);
		content.setOrientation(LinearLayout.VERTICAL);
		final int padding = General.dpToPixels(activity, 16);
		content.setPadding(padding, padding, padding, padding);

		final TextView originalLabel = new MaterialTextView(activity);
		originalLabel.setText(R.string.translation_original);
		content.addView(originalLabel);
		final TextView original = new MaterialTextView(activity);
		original.setText(requireArguments().getString("text"));
		original.setTextIsSelectable(true);
		content.addView(original);

		final TextView translatedLabel = new MaterialTextView(activity);
		translatedLabel.setText(R.string.translation_result);
		translatedLabel.setPadding(0, padding, 0, padding / 2);
		content.addView(translatedLabel);
		progress = new ProgressBar(activity);
		progress.setVisibility(View.GONE);
		content.addView(progress);
		output = new MaterialTextView(activity);
		output.setTextIsSelectable(true);
		output.setAccessibilityLiveRegion(View.ACCESSIBILITY_LIVE_REGION_POLITE);
		content.addView(output);

		translation = savedInstanceState == null
				? null : savedInstanceState.getString("translation");
		autoStart = savedInstanceState == null;
		if(translation != null) {
			output.setText(translation);
		} else {
			output.setText(R.string.translation_start_hint);
		}
		final ScrollView scroll = new ScrollView(activity);
		scroll.addView(content);

		return new MaterialAlertDialogBuilder(activity)
				.setTitle(R.string.action_translate)
				.setView(scroll)
				.setPositiveButton(R.string.action_translate, null)
				.setNegativeButton(R.string.dialog_close, null)
				.setNeutralButton(R.string.options_settings, (dialog, which) -> {
					final Intent intent = new Intent(activity, SettingsActivity.class);
					intent.putExtra("panel", "translation");
					startActivity(intent);
				})
				.create();
	}

	@Override
	public void onStart() {
		super.onStart();
		((AlertDialog)requireDialog()).getButton(AlertDialog.BUTTON_POSITIVE)
				.setOnClickListener(view -> startTranslation());
		if(autoStart) {
			autoStart = false;
			startTranslation();
		}
	}

	private void startTranslation() {
		if(task != null && !task.isDone()) {
			return;
		}
		final LocalTranslation local = LocalTranslation.getInstance(requireContext());
		if(local.getModels().getModelSize() == 0) {
			output.setText(R.string.translation_model_missing);
			return;
		}
		translation = null;
		output.setText(R.string.translation_running);
		setBusy(true);
		task = local.getService().translate(new TranslationRequest(
				requireArguments().getString("text"),
				requireArguments().getString("language")), new TranslationService.Callback() {
			@Override
			public void onSuccess(final String result) {
				translation = result;
				output.setText(result);
				setBusy(false);
			}

			@Override
			public void onFailure(final Throwable error) {
				output.setText(getString(R.string.translation_failed, error.getMessage()));
				setBusy(false);
			}
		});
	}

	private void setBusy(final boolean busy) {
		progress.setVisibility(busy ? View.VISIBLE : View.GONE);
		((AlertDialog)requireDialog()).getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(!busy);
	}

	@Override
	public void onSaveInstanceState(@NonNull final Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("translation", translation);
	}

	@Override
	public void onDismiss(@NonNull final DialogInterface dialog) {
		if(task != null) {
			task.cancel(true);
		}
		super.onDismiss(dialog);
	}

	@Override
	public void onDestroyView() {
		if(task != null) {
			task.cancel(true);
		}
		super.onDestroyView();
	}
}
