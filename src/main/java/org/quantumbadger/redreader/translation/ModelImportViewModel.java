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

package org.quantumbadger.redreader.translation;

import android.app.Application;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/** Keeps a model copy running across settings-screen rotation. */
public final class ModelImportViewModel extends AndroidViewModel {

	public static final class State {

		public final boolean busy;
		public final String error;

		private State(final boolean busy, final String error) {
			this.busy = busy;
			this.error = error;
		}
	}

	private final MutableLiveData<State> state = new MutableLiveData<>(new State(false, null));
	private final ExecutorService worker = Executors.newSingleThreadExecutor();

	public ModelImportViewModel(@NonNull final Application application) {
		super(application);
	}

	public LiveData<State> getState() {
		return state;
	}

	/** A null URI removes the currently imported model. */
	public void updateModel(final Uri uri) {
		if(state.getValue().busy) {
			return;
		}
		state.setValue(new State(true, null));
		worker.execute(() -> {
			final TranslationModelStore models =
					LocalTranslation.getInstance(getApplication()).getModels();
			try {
				if(uri == null) {
					models.removeModel();
				} else {
					models.importModel(uri);
				}
				state.postValue(new State(false, null));
			} catch(final IOException | SecurityException error) {
				state.postValue(new State(false, error.toString()));
			}
		});
	}

	@Override
	protected void onCleared() {
		worker.shutdownNow();
	}
}
