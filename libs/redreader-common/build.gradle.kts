import org.jetbrains.kotlin.gradle.dsl.JvmTarget
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
    compilerOptions.jvmTarget.set(JvmTarget.fromTarget(libs.versions.java.get()))
}

dependencies {
	implementation(libs.kotlinx.datetime)
	implementation(libs.kotlinx.serialization.json)
}
