package org.quantumbadger.redreader.ui.views;

public final class MeasurementException extends RuntimeException {

	public enum InvalidMeasurementType {
		WIDTH_IS_FIXED, HEIGHT_IS_FIXED,
		WIDTH_DETERMINED_BY_HEIGHT, HEIGHT_DETERMINED_BY_WIDTH,
		EXPECTING_FIXED_WIDTH, EXPECTING_FIXED_HEIGHT
	}

	public MeasurementException(RRView view, InvalidMeasurementType type) {
		super(String.format("%s when measuring %s", type.name(), view.getClass().getName()));
	}
}
