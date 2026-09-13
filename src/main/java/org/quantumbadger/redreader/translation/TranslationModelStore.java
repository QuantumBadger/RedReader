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

import android.content.Context;
import android.net.Uri;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;

/** Model files are imported atomically and excluded from Android backups. */
public final class TranslationModelStore {

	private final Context context;
	private final File directory;
	private final File model;

	TranslationModelStore(final Context context) {
		this.context = context.getApplicationContext();
		directory = new File(this.context.getNoBackupFilesDir(), "translation");
		model = new File(directory, "model.gguf");
	}

	synchronized File requireModel() throws IOException {
		if(!model.isFile()) {
			throw new IOException("Import a translation model in Settings first");
		}
		return model;
	}

	public long getModelSize() {
		return model.isFile() ? model.length() : 0;
	}

	public void importModel(final Uri uri) throws IOException {
		if(!directory.isDirectory() && !directory.mkdirs()) {
			throw new IOException("Could not create the model directory");
		}
		final File temporary = File.createTempFile("import-", ".gguf", directory);
		try {
			try(InputStream raw = context.getContentResolver().openInputStream(uri)) {
				if(raw == null) {
					throw new IOException("Could not open the selected file");
				}
				try(DataInputStream input = new DataInputStream(raw)) {
					final byte[] header = new byte[8];
					input.readFully(header);
					if(header[0] != 'G' || header[1] != 'G' || header[2] != 'U' || header[3] != 'F'
							|| (header[4] != 2 && header[4] != 3)
							|| header[5] != 0 || header[6] != 0 || header[7] != 0) {
						throw new IOException("Select a GGUF version 2 or 3 model file");
					}
					try(FileOutputStream output = new FileOutputStream(temporary)) {
						output.write(header);
						final byte[] buffer = new byte[64 * 1024];
						int length;
						while((length = input.read(buffer)) != -1) {
							checkCancellation();
							output.write(buffer, 0, length);
						}
						output.getFD().sync();
					}
				}
			}
			if(temporary.length() <= 24) {
				throw new IOException("The model file is incomplete");
			}
			// Inference also holds this monitor, so replacement cannot remove its model.
			synchronized(this) {
				checkCancellation();
				if(!temporary.renameTo(model)) {
					throw new IOException("Could not replace the translation model");
				}
			}
		} finally {
			if(temporary.exists() && !temporary.delete()) {
				throw new IOException("Could not remove the incomplete model import");
			}
		}
	}

	public synchronized void removeModel() throws IOException {
		if(model.exists() && !model.delete()) {
			throw new IOException("Could not remove the translation model");
		}
	}

	private static void checkCancellation() throws InterruptedIOException {
		if(Thread.currentThread().isInterrupted()) {
			throw new InterruptedIOException("Model import cancelled");
		}
	}
}
