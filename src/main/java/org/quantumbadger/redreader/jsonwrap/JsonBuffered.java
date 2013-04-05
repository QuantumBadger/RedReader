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

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;

import java.io.IOException;


/**
 * An abstract class, extended by objects which may be partially received/parsed
 * at the time they are used.
 * 
 */
public abstract class JsonBuffered {
	
	public static enum Status {
		/**
		 * This object or array is not fully parsed yet. Because of this,
		 * attempts to access some properties or elements may cause the current
		 * thread to block.
		 */
		LOADING,
		
		/**
		 * This object or array is now fully loaded. All properties and elements
		 * can be accessed without blocking.
		 */
		LOADED,
		
		/**
		 * There was a problem parsing this object/array (or one of its
		 * descendants). Attempting to access any of the data it contains may
		 * cause an exception.
		 */
		FAILED
	}
	
	private volatile Status status = Status.LOADING;
	private Throwable failReason = null;
	
	/**
	 * @return The current status of this object: LOADING, LOADED, or FAILED.
	 */
	public final Status getStatus() {
		return status;
	}
	
	/**
	 * Causes the current thread to wait until this object and all its children
	 * fully received.
	 * 
	 * @return The final status of the object (either LOADED or FAILED).
	 * @throws InterruptedException
	 */
	public final synchronized Status join() throws InterruptedException {
		
		while(status == Status.LOADING) {
			wait();
		}
		
		return status;
	}
	
	private synchronized void setLoaded() {
		status = Status.LOADED;
		notifyAll();
	}
	
	private synchronized void setFailed(final Throwable t) {
		status = Status.FAILED;
		failReason = t;
		notifyAll();
	}
	
	/**
	 * @return If this object or one of its children failed to parse, the
	 *		 exception that occurred at the time of failure is returned.
	 *		 Otherwise, null.
	 */
	public final Throwable getFailReason() {
		return failReason;
	}
	
	protected final void throwFailReasonException() throws IOException {
		
		final Throwable t = getFailReason();
		
		if(t instanceof JsonParseException)
			throw (JsonParseException)t;
		
		else if(t instanceof IOException)
			throw (IOException)t;
		
		else
			throw new RuntimeException(t);
	}
	
	protected final void build(final JsonParser jp) throws IOException {
		
		try {
			buildBuffered(jp);
			setLoaded();
			
		} catch (final IOException e) {
			setFailed(e);
			throw e;
			
		} catch (final Throwable t) {
			setFailed(t);
			throw new RuntimeException(t);
		}
	}
	
	protected abstract void buildBuffered(JsonParser jp) throws IOException;
	
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

	protected abstract void prettyPrint(int indent, StringBuilder sb) throws InterruptedException, IOException;
}
