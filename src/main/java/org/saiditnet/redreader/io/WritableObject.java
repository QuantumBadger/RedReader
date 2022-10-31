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

package org.saiditnet.redreader.io;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public interface WritableObject<K> {

	class CreationData {
		public final String key;
		public final long timestamp;

		public CreationData(String key, long timestamp) {
			this.key = key;
			this.timestamp = timestamp;
		}
	}

	K getKey();
	long getTimestamp();

	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectVersion {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectKey {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectTimestamp {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableField {}
}
