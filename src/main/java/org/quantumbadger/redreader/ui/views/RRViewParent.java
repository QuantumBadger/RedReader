package org.quantumbadger.redreader.ui.views;

public interface RRViewParent {

	public void rrInvalidate(RRView child);
	public void rrRequestLayout(RRView child);
}
