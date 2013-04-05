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

package org.quantumbadger.redreader.jsonwrap;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;


/**
 * Contains a literal, object, or array value, and is responsible for parsing
 * the incoming JSON stream.
 * 
 * <p>
 * <b>To parse a JSON stream, call the JsonValue constructor with a JsonParser
 * as the argument, then call the "build" method (with the same JsonParser as an
 * argument) in another thread.</b>
 * </p>
 */
public final class JsonValue {
	
	public static enum Type {
		OBJECT, ARRAY, NULL, BOOLEAN, STRING, FLOAT, INTEGER
	}
	
	private final Type type;
	private final Object value;
	
	private JsonParser jp = null;
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param jp
	 *			The incoming JSON stream
	 * @throws java.io.IOException
	 */
	public JsonValue(final JsonParser jp) throws IOException {
		this(jp, jp.nextToken());
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final InputStream source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final URL source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final byte[] source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final String source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final File source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	/**
	 * Begins parsing a JSON stream into a tree structure. The JsonValue object
	 * created contains the value at the root of the tree.
	 * 
	 * This constructor will block until the first JSON token is received. To
	 * continue building the tree, the "build" method (inherited from
	 * JsonBuffered) must be called in another thread.
	 * 
	 * @param source
	 *			The source of incoming JSON data.
	 * @throws java.io.IOException
	 */
	public JsonValue(final Reader source) throws IOException {
		this(new JsonFactory().createParser(source));
	}
	
	// The main constructor
	protected JsonValue(final JsonParser jp, final JsonToken firstToken) throws IOException {
		
		switch(firstToken) {
			
			case START_OBJECT:
				type = Type.OBJECT;
				value = new JsonBufferedObject();
				this.jp = jp;
				break;
				
			case START_ARRAY:
				type = Type.ARRAY;
				value = new JsonBufferedArray();
				this.jp = jp;
				break;
				
			case VALUE_FALSE:
				type = Type.BOOLEAN;
				value = false;
				break;
				
			case VALUE_TRUE:
				type = Type.BOOLEAN;
				value = true;
				break;
				
			case VALUE_NULL:
				type = Type.NULL;
				value = null;
				break;
				
			case VALUE_STRING:
				type = Type.STRING;
				value = jp.getValueAsString();
				break;
				
			case VALUE_NUMBER_FLOAT:

				//noinspection FloatingPointEquality,UnnecessaryExplicitNumericCast
				if(jp.getValueAsDouble() == (double)jp.getValueAsLong()) {
					type = Type.INTEGER;
					value = jp.getValueAsLong();
				} else {
					type = Type.FLOAT;
					value = jp.getValueAsDouble();
				}
				
				break;
				
			case VALUE_NUMBER_INT:
				type = Type.INTEGER;
				value = jp.getValueAsLong();
				break;
				
			default:
				throw new JsonParseException("Expecting an object, literal, or array", jp.getCurrentLocation());
		}
	}
	
	/**
	 * Continues the process of parsing the specified JSON stream.
	 * 
	 * This method will block until the stream is fully parsed, but it is
	 * possible to make use of this value and its descendants in other threads
	 * while this is occurring.
	 *
	 * @throws java.io.IOException
	 */
	public void buildInThisThread() throws IOException {
		
		if(type == Type.OBJECT || type == Type.ARRAY) {
			((JsonBuffered)value).build(jp);
		}
		
		this.jp = null;
	}
	
	/**
	 * Continues the process of parsing the specified JSON stream.
	 * 
	 * This method will create a thread and parse the JSON stream in the background.
	 */
	public Thread buildInNewThread() {
		
		final Thread t = new Thread() {
			public void run() {

				android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

				try {
					buildInThisThread();
				} catch (Throwable t) {
					// Ignore it -- it'll show up again when another thread tries to access the value
					t.printStackTrace();
				}
			}
		};
		
		t.start();
		
		return t;
	}
	
	/**
	 * @return The type of value this JsonValue contains.
	 */
	public Type getType() {
		return type;
	}
	
	/**
	 * @return True if the type of this value is NULL.
	 */
	public boolean isNull() {
		return type == Type.NULL;
	}
	
	/**
	 * @return If this JsonValue contains a JSON object, then this method
	 *		 returns that object. The type of value this JsonValue contains
	 *		 can be checked with the getType() method.
	 */
	public JsonBufferedObject asObject() {
		
		switch(type) {
			case NULL:
				return null;
			default:
				return (JsonBufferedObject)value;
		}
	}

	public <E> E asObject(final Class<E> clazz) throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		
		switch(type) {
			case NULL:
				return null;
			default:
				return asObject().asObject(clazz);
		}
	}
	
	/**
	 * @return If this JsonValue contains a JSON array, then this method returns
	 *		 that array. The type of value this JsonValue contains can be
	 *		 checked with the getType() method.
	 */
	public JsonBufferedArray asArray() {
		
		switch(type) {
			case NULL:
				return null;
			default:
				return (JsonBufferedArray)value;
		}
	}
	
	/**
	 * @return If this JsonValue contains a boolean, then this method returns
	 *		 that boolean. The type of value this JsonValue contains can be
	 *		 checked with the getType() method.
	 */
	public Boolean asBoolean() {
		
		switch(type) {
			case NULL:
				return null;
			default:
				return (Boolean)value;
		}
	}
	
	/**
	 * @return If this JsonValue contains a string, then this method returns
	 *		 that string. The type of value this JsonValue contains can be
	 *		 checked with the getType() method.
	 */
	public String asString() {
		
		switch(type) {
			case FLOAT:
				return String.valueOf(asDouble());
			case INTEGER:
				return String.valueOf(asLong());
			case BOOLEAN:
				return String.valueOf(asBoolean());
			case NULL:
				return null;
			default:
				return (String)value;
		}
	}
	
	/**
	 * @return If this JsonValue contains a double, then this method returns
	 *		 that double. The type of value this JsonValue contains can be
	 *		 checked with the getType() method.
	 */
	public Double asDouble() {
		switch(type) {
			case NULL:
				return null;
			case INTEGER:
				return ((Long)value).doubleValue();
			case STRING:
				return Double.parseDouble(asString());
			default:
				return (Double)value;
		}
	}
	
	/**
	 * @return If this JsonValue contains an integer, then this method returns
	 *		 that integer. The type of value this JsonValue contains can be
	 *		 checked with the getType() method.
	 */
	public Long asLong() {
		switch(type) {
			case NULL:
				return null;
			case FLOAT:
				return ((Double)value).longValue();
			case STRING:
				return Long.parseLong(asString());
			default:
				return (Long)value;
		}
	}
	
	@Override
	public String toString() {

		final StringBuilder sb = new StringBuilder();
		
		try {
			prettyPrint(0, sb);
			
		} catch (InterruptedException e) {
			e.printStackTrace();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return sb.toString();
	}
	
	protected void prettyPrint(final int indent, final StringBuilder sb) throws InterruptedException, IOException {
		
		switch(type) {
			case BOOLEAN:	sb.append(asBoolean()); break;
			case FLOAT:		sb.append(asDouble()); break;
			case INTEGER:	sb.append(asLong()); break;
			case NULL:		sb.append("null"); break;
			case STRING:	sb.append("\"").append(asString().replace("\\", "\\\\").replace("\"", "\\\"")).append("\""); break;
			case ARRAY:
			case OBJECT:	((JsonBuffered)value).prettyPrint(indent, sb); break;
		}
	}

	public void join() throws InterruptedException {
		if(getType() == Type.ARRAY || getType() == Type.OBJECT) {
			((JsonBuffered)value).join();
		}
	}
}
