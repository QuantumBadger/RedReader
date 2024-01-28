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
    kotlinOptions.jvmTarget = libs.versions.java.get()
}

dependencies {
	implementation(project(":redreader-common"))
}
