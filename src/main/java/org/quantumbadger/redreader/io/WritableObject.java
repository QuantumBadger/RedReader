package com.konneh.scroll.io;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public interface WritableObject<K> {

	public class CreationData {
		public final String key;
		public final long timestamp;

		public CreationData(String key, long timestamp) {
			this.key = key;
			this.timestamp = timestamp;
		}
	}

	public K getKey();
	public long getTimestamp();

	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectVersion {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectKey {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableObjectTimestamp {}
	@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.FIELD) @interface WritableField {}
}
