package org.quantumbadger.redreader.common;

import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;

public final class VariableBoolean {

	private boolean lastValue, value;

	private final WeakReferenceListManager<VariableBooleanListener> listeners
			= new WeakReferenceListManager<VariableBooleanListener>();

	private final WeakReferenceListManager.Operator<VariableBooleanListener> listenerOperator
			 = new WeakReferenceListManager.Operator<VariableBooleanListener>() {
		public void operate(VariableBooleanListener object) {
			object.onBooleanChanged(VariableBoolean.this, lastValue, value);
		}
	};

	public VariableBoolean(boolean value) {
		this.value = value;
	}

	public synchronized void set(boolean newValue) {
		lastValue = value;
		value = newValue;
		listeners.map(listenerOperator);
	}

	public synchronized boolean get() {
		return value;
	}

	public interface VariableBooleanListener {
		public void onBooleanChanged(VariableBoolean variable, boolean oldValue, boolean newValue);
	}

}
