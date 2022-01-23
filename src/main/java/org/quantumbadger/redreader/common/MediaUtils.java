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

package org.quantumbadger.redreader.common;

import android.media.MediaCodec;
import android.media.MediaExtractor;
import android.media.MediaFormat;
import android.media.MediaMuxer;
import android.os.Build;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public final class MediaUtils {

	private static final String TAG = "MediaUtils";

	private MediaUtils() {}

	@RequiresApi(api = Build.VERSION_CODES.JELLY_BEAN_MR2)
	public static void muxFiles(
			@NonNull final File outputFile,
			@NonNull final File[] inputFiles,
			@NonNull final Runnable successCallback,
			@NonNull final FunctionOneArgNoReturn<Exception> failureCallback) {

		new Thread(() -> {

			class InputFile implements Closeable {
				private final File mFile;
				private final MediaExtractor mExtractor;
				private final Map<Integer, Integer> mTrackIds;

				InputFile(
						final File file,
						final MediaExtractor extractor,
						final Map<Integer, Integer> trackIds) {
					mFile = file;
					mExtractor = extractor;
					mTrackIds = trackIds;
				}

				public int getOutputTrackId(final int inputTrackId) {
					return mTrackIds.get(inputTrackId);
				}

				public MediaExtractor getExtractor() {
					return mExtractor;
				}

				public File getFile() {
					return mFile;
				}

				@Override
				public void close() throws IOException {
					mExtractor.release();
				}
			}

			MediaMuxer muxer = null;

			final ArrayList<InputFile> inputFilesToClose = new ArrayList<>();

			Log.i(TAG, "muxFiles: " + outputFile);

			try {

				muxer = new MediaMuxer(
						outputFile.getAbsolutePath(),
						MediaMuxer.OutputFormat.MUXER_OUTPUT_MPEG_4);

				for(final File inputFile : inputFiles) {

					final MediaExtractor mediaExtractor = new MediaExtractor();

					final String path = inputFile.getAbsolutePath();

					mediaExtractor.setDataSource(path);

					final HashMap<Integer, Integer> trackIds = new HashMap<>();

					for(int inputTrackId = 0;
						inputTrackId < mediaExtractor.getTrackCount();
						inputTrackId++) {

						mediaExtractor.selectTrack(inputTrackId);

						final MediaFormat format = mediaExtractor.getTrackFormat(inputTrackId);

						final int outputTrackId = muxer.addTrack(format);

						trackIds.put(inputTrackId, outputTrackId);

						Log.i(TAG, "Track "
								+ outputTrackId
								+ ": path '"
								+ path
								+ "' format "
								+ format.toString());
					}

					mediaExtractor.seekTo(0, MediaExtractor.SEEK_TO_CLOSEST_SYNC);

					inputFilesToClose.add(new InputFile(
							inputFile,
							mediaExtractor,
							trackIds));
				}

				final ArrayList<InputFile> inputFilesToRead
						= new ArrayList<>(inputFilesToClose);

				Log.i(TAG, "Starting mux for " + outputFile);

				muxer.start();

				final ByteBuffer sampleBuffer = ByteBuffer.allocateDirect(1024 * 1024); // 1MiB
				final MediaCodec.BufferInfo bufferInfo = new MediaCodec.BufferInfo();

				while(!inputFilesToRead.isEmpty()) {

					long minTime = Long.MAX_VALUE;

					for(final InputFile file : inputFilesToRead) {
						final long sampleTime = file.getExtractor().getSampleTime();
						minTime = Math.min(minTime, sampleTime);
					}

					{
						final Iterator<InputFile> iterator = inputFilesToRead.iterator();

						while(iterator.hasNext()) {

							final InputFile file = iterator.next();

							final MediaExtractor extractor = file.getExtractor();

							while(extractor.getSampleTime() == minTime) {

								sampleBuffer.clear();

								final int readResult
										= extractor.readSampleData(sampleBuffer, 0);

								if(readResult < 0) {
									iterator.remove();
									Log.i(TAG, "No bytes to read from "
											+ file.getFile().getAbsolutePath());
									break;

								} else {

									final int outputTrackId = file.getOutputTrackId(
											extractor.getSampleTrackIndex());

									sampleBuffer.limit(
											readResult);
									sampleBuffer.position(0);

									int flags = 0;

									if((extractor.getSampleFlags()
											& MediaExtractor.SAMPLE_FLAG_SYNC) != 0) {
										//noinspection deprecation
										flags |= MediaCodec.BUFFER_FLAG_SYNC_FRAME;
									}

									if((extractor.getSampleFlags()
											& MediaExtractor.SAMPLE_FLAG_PARTIAL_FRAME) != 0) {
										if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
											flags |= MediaCodec.BUFFER_FLAG_PARTIAL_FRAME;
										}
									}

									bufferInfo.set(
											0,
											sampleBuffer.remaining(),
											extractor.getSampleTime(),
											flags);

									muxer.writeSampleData(
											outputTrackId,
											sampleBuffer,
											bufferInfo);

									if(!extractor.advance()) {
										iterator.remove();
										Log.i(TAG, "Finished writing track " + outputTrackId);
										break;
									}
								}
							}
						}
					}
				}

				Log.i(TAG, "Stopping muxer...");
				muxer.stop();

				Log.i(TAG, "Releasing muxer...");
				muxer.release();

				Log.i(TAG, "Mux complete for " + outputFile);

				successCallback.run();

			} catch(final Exception e) {
				failureCallback.apply(e);

			} finally {
				if(muxer != null) {
					muxer.release();
				}

				for(final InputFile file : inputFilesToClose) {
					try {
						file.close();
					} catch(final IOException e) {
						Log.e(
								TAG,
								"Failed to clean up input file "
										+ file.getFile().getAbsolutePath(),
								e);
					}
				}
			}

		}).start();
	}
}
