import org.jetbrains.kotlin.gradle.tasks.KotlinJvmCompile

buildscript {
	repositories {
		mavenCentral()
		google()
	}
}

plugins {
	`java-library`
	alias(libs.plugins.kotlin.jvm)
	alias(libs.plugins.kotlin.serialization)
}

java {
	JavaVersion.toVersion(libs.versions.java.get()).let {
		sourceCompatibility = it
		targetCompatibility = it
	}
}

tasks.withType(KotlinJvmCompile::class) {
    kotlinOptions.jvmTarget = libs.versions.java.get()
}

dependencies {
	implementation(libs.kotlinx.datetime)
	implementation(libs.kotlinx.serialization.json)
}
