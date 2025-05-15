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

package org.quantumbadger.redreader.common.collections;

import java.util.Collection;

public abstract class Stream<Type> {

	public abstract boolean hasNext();

	public abstract Type take();

	public final <Output> Stream<Output> map(final MapStream.Operator<Type, Output> operator) {
		return new MapStream<>(this, operator);
	}

	public final <Output> Stream<Output> mapRethrowExceptions(
			final MapStreamRethrowExceptions.Operator<Type, Output> operator) {
		return new MapStreamRethrowExceptions<>(this, operator);
	}

	public final <Output extends Collection<? super Type>> Output collect(
			final Output output) {

		while(hasNext()) {
			output.add(take());
		}

		return output;
	}
}
