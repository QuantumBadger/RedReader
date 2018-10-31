package org.quantumbadger.redreader.adapters;

public interface ShareOrderCallbackListener {

	enum MediaType{
		PLAINTEXT,
		IMAGE,
		VIDEO
	}

	void onSelectedIntent(int position);

}
