-dontobfuscate

-keepclassmembers class * extends org.quantumbadger.redreader.io.WritableObject {
	*;
}

-keepclassmembers class * extends org.quantumbadger.redreader.jsonwrap.JsonObject$JsonDeserializable {
	*;
}

-keepclassmembers class org.quantumbadger.redreader.R { *; }
-keepclassmembers class org.quantumbadger.redreader.R$xml {	*; }
-keepclassmembers class org.quantumbadger.redreader.R$string {	*; }

-keepclassmembers class com.github.luben.zstd.* {
	*;
}
