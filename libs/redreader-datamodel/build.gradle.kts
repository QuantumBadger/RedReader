import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinJvmCompile

plugins {
    `java-library`
    alias(libs.plugins.kotlin.jvm)
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
	implementation(project(":redreader-common"))
}
