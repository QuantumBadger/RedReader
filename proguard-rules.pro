-dontobfuscate
-keepattributes LineNumberTable,SourceFile,RuntimeVisibleAnnotations,AnnotationDefault,InnerClasses,EnclosingMethod

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

-if @kotlinx.serialization.Serializable class **
{
    static **$* *;
}

-keepnames class <1>$$serializer { # -keepnames suffices; class is kept when serializer() is kept.
    static <1>$$serializer INSTANCE;
}
