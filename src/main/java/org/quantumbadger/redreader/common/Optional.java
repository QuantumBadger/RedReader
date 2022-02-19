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
import androidx.annotation.Nullable;

public final class Optional<E> {

	public static class OptionalHasNoValueException extends RuntimeException {}

	private static final Optional<?> EMPTY = new Optional<>(null);

	@Nullable private final E mValue;

	private Optional(@Nullable final E value) {
		mValue = value;
	}

	@NonNull
	public static <E> Optional<E> empty() {
		//noinspection unchecked
		return (Optional<E>)EMPTY;
	}

	@NonNull
	public static <E> Optional<E> of(@NonNull final E value) {
		return new Optional<>(value);
	}

	@NonNull
	public static <E> Optional<E> ofNullable(@Nullable final E value) {

		if(value == null) {
			return empty();
		}

		return new Optional<>(value);
	}

	public boolean isPresent() {
		return mValue != null;
	}

	public boolean isEmpty() {
		return mValue == null;
	}

	@NonNull
	public E get() {

		if(mValue == null) {
			throw new OptionalHasNoValueException();
		}

		return mValue;
	}

	@NonNull
	public E orElse(@NonNull final E alternative) {

		if(mValue == null) {
			return alternative;
		} else {
			return mValue;
		}
	}

	@NonNull
	public Optional<E> orElse(@NonNull final Optional<E> alternative) {

		if(mValue == null) {
			return alternative;
		} else {
			return Optional.of(mValue);
		}
	}

	@Nullable
	public E orElseNull() {
		return mValue;
	}

	@NonNull
	public <T extends Exception> E orThrow(
			@NonNull final GenericFactory<T, RuntimeException> factory) throws T {

		if(mValue == null) {
			throw factory.create();
		}

		return mValue;
	}

	@NonNull
	public <R> Optional<R> map(@NonNull final FunctionOneArgWithReturn<E, R> function) {

		if(mValue == null) {
			return Optional.empty();
		} else {
			return Optional.of(function.apply(mValue));
		}
	}

	public void apply(@NonNull final FunctionOneArgNoReturn<E> function) {

		if(mValue != null) {
			function.apply(mValue);
		}
	}

	@NonNull
	public <R> Optional<R> filter(
			@NonNull final FunctionOneArgWithReturn<E, Optional<R>> function) {

		if(mValue == null) {
			return Optional.empty();
		} else {
			return function.apply(mValue);
		}
	}

	public void ifPresent(@NonNull final Consumer<E> consumer) {
		if(mValue != null) {
			consumer.consume(mValue);
		}
	}

	@Override
	public int hashCode() {

		if(mValue == null) {
			return 0x28734823; // Random value
		} else {
			return mValue.hashCode();
		}
	}

	@Override
	public boolean equals(@Nullable final Object obj) {

		if(!(obj instanceof Optional)) {
			return false;
		}

		if(mValue == null) {
			return ((Optional<?>)obj).mValue == null;
		}

		return mValue.equals(((Optional<?>)obj).mValue);
	}

	@NonNull
	@Override
	public String toString() {

		if(mValue == null) {
			return "<empty>";
		} else {
			return mValue.toString();
		}
	}
}
