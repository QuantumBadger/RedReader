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

package org.quantumbadger.redreader.settings;

import android.os.Bundle;
import android.text.format.Formatter;
import android.view.View;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.translation.LocalTranslation;
import org.quantumbadger.redreader.translation.ModelImportViewModel;

public final class TranslationSettingsFragment extends PreferenceFragmentCompat {

	private ModelImportViewModel model;
	private final ActivityResultLauncher<String[]> chooseModel = registerForActivityResult(
			new ActivityResultContracts.OpenDocument(),
			uri -> {
				if(uri != null) {
					model.updateModel(uri);
				}
			});

	@Override
	public void onCreatePreferences(final Bundle savedInstanceState, final String rootKey) {
		setPreferencesFromResource(R.xml.prefs_translation, rootKey);
		model = new ViewModelProvider(this).get(ModelImportViewModel.class);

		final Preference importModel = findPreference("translation_import");
		importModel.setOnPreferenceClickListener(preference -> {
			chooseModel.launch(new String[] {"*/*"});
			return true;
		});
		final Preference removeModel = findPreference("translation_remove");
		removeModel.setOnPreferenceClickListener(preference -> {
			new MaterialAlertDialogBuilder(requireContext())
					.setMessage(R.string.translation_remove_confirm)
					.setPositiveButton(R.string.translation_remove, (dialog, which) ->
							model.updateModel(null))
					.setNegativeButton(R.string.dialog_cancel, null)
					.show();
			return true;
		});
	}

	@Override
	public void onViewCreated(@NonNull final View view, @Nullable final Bundle savedInstanceState) {
		super.onViewCreated(view, savedInstanceState);
		model.getState().observe(getViewLifecycleOwner(), state -> {
			final Preference status = findPreference("translation_status");
			final Preference importModel = findPreference("translation_import");
			final Preference removeModel = findPreference("translation_remove");
			final long size = LocalTranslation.getInstance(requireContext()).getModels()
					.getModelSize();
			importModel.setEnabled(!state.busy);
			removeModel.setEnabled(!state.busy && size > 0);
			if(state.busy) {
				status.setSummary(R.string.translation_model_working);
			} else if(state.error != null) {
				status.setSummary(getString(R.string.translation_model_error, state.error));
			} else if(size > 0) {
				status.setSummary(getString(R.string.translation_model_ready,
						Formatter.formatFileSize(requireContext(), size)));
			} else {
				status.setSummary(R.string.translation_model_missing);
			}
		});
	}

	@Override
	public void onResume() {
		super.onResume();
		requireActivity().setTitle(R.string.prefs_category_translation);
	}
}
