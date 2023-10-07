
buildscript {
	repositories {
		mavenCentral()
		google()
	}
}

plugins {
    id("java-library")
    id("org.jetbrains.kotlin.jvm")
	kotlin("plugin.serialization") version("1.9.10") apply(true)
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

dependencies {
	implementation("org.jetbrains.kotlinx:kotlinx-datetime:0.4.0")
	implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.0")
}
