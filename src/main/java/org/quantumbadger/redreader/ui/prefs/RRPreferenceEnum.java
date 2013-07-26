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

package org.quantumbadger.redreader.ui.prefs;

import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

public class RRPreferenceEnum<E extends Enum> extends RRPreference {

	private E value;
	private final Method enumValueOf;

	enum TestEnum {A}

	protected static <E extends Enum> RRPreferenceEnum parse(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource)
			throws NoSuchFieldException, IllegalAccessException, IOException, XmlPullParserException, ClassNotFoundException, NoSuchMethodException, InvocationTargetException {

		final String id = attributes.get("id");

		final String defaultValue = attributes.get("default");
		final String userValue = preferenceManager.getRawUserPreference(id);

		final String enumClassName = attributes.get("enum");

		@SuppressWarnings("unchecked")
		final Class<E> enumClass = (Class<E>) Class.forName(enumClassName);

		final Method enumValueOf = enumClass.getMethod("valueOf", String.class);

		@SuppressWarnings("unchecked")
		final E value = (E) enumValueOf.invoke(null, userValue != null ? userValue : defaultValue);

		return new RRPreferenceEnum<E>(preferenceManager, attributes, itemSource, value, enumValueOf);
	}

	private RRPreferenceEnum(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource, E value, Method enumValueOf) throws NoSuchFieldException, IllegalAccessException {
		super(preferenceManager, attributes, itemSource);
		this.value = value;
		this.enumValueOf = enumValueOf;
	}

	public E get() {
		return value;
	}

	public void set(String value) {
		try {
			//noinspection unchecked
			this.value = (E) enumValueOf.invoke(null, value);
		} catch(IllegalAccessException e) {
			throw new RuntimeException(e);
		} catch(InvocationTargetException e) {
			throw new RuntimeException(e);
		}
		setRawUserPreference(value);
	}

	public void set(E enumValue) {
		this.value = enumValue;
		setRawUserPreference(enumValue.name());
	}
}
