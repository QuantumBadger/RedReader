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

import androidx.annotation.NonNull;

import java.util.HashMap;

public class GenerationalCache<In extends HasUniqueId, Out> {

	@NonNull private final FunctionOneArgWithReturn<In, Out> mCreator;

	@NonNull private HashMap<String, Out> mPreviousGen = new HashMap<>();
	@NonNull private HashMap<String, Out> mThisGen = new HashMap<>();


	public GenerationalCache(
			@NonNull final FunctionOneArgWithReturn<In, Out> creator) {
		mCreator = creator;
	}

	@NonNull
	public Out get(@NonNull final In in) {

		final String uniqueId = in.getUniqueId();

		Out result = mThisGen.get(uniqueId);

		if(result != null) {
			return result;
		}

		result = mPreviousGen.get(uniqueId);

		if(result == null) {
			result = mCreator.apply(in);
			mThisGen.put(uniqueId, result);
		}

		mThisGen.put(uniqueId, result);
		return result;
	}

	public void nextGeneration() {
		mPreviousGen = mThisGen;
		mThisGen = new HashMap<>();
	}
}
