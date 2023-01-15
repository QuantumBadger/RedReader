buildscript {
	repositories {
		mavenCentral()
		google()
	}
	dependencies {
		classpath("com.android.tools.build:gradle:7.3.1")
	}
}

plugins {
	id("com.android.application") version("7.3.1") apply(true)
	pmd
	checkstyle
}

dependencies {
	"implementation"("androidx.annotation:annotation:1.3.0")
	"implementation"("androidx.appcompat:appcompat:1.4.0")
	"implementation"("androidx.recyclerview:recyclerview:1.2.1")
	"implementation"("com.google.android.flexbox:flexbox:3.0.0")
	"implementation"("androidx.swiperefreshlayout:swiperefreshlayout:1.1.0")
	"implementation"("androidx.preference:preference:1.1.1")
	"implementation"("com.google.android.material:material:1.4.0")
	"implementation"("androidx.constraintlayout:constraintlayout:2.1.2")
	"implementation"("androidx.fragment:fragment:1.4.0")

	"implementation"("com.fasterxml.jackson.core:jackson-core:2.13.1")
	"implementation"("org.apache.commons:commons-lang3:3.12.0")
	"implementation"("org.apache.commons:commons-text:1.9")
	"implementation"("joda-time:joda-time:2.10.13")
	"implementation"("com.squareup.okhttp3:okhttp:3.12.13")
	"implementation"("info.guardianproject.netcipher:netcipher:1.2.1")
	"implementation"("com.google.android.exoplayer:exoplayer-core:2.16.1")
	"implementation"("com.google.android.exoplayer:exoplayer-ui:2.16.1")
	"implementation"("com.github.luben:zstd-jni:1.5.1-1@aar")

	"testImplementation"("junit:junit:4.13.2")

	"androidTestImplementation"("androidx.test.espresso:espresso-core:3.4.0")
	"androidTestImplementation"("androidx.test:rules:1.4.0")
	"androidTestImplementation"("androidx.test.espresso:espresso-contrib:3.4.0")
}

android {
	compileSdk = 33
	buildToolsVersion = "33.0.1"
	ndkVersion = "23.1.7779620"

	defaultConfig {
		applicationId = "org.quantumbadger.redreader"
		minSdk = 16
		targetSdk = 31
		versionCode = 104
		versionName = "1.20"

		vectorDrawables.useSupportLibrary = true
		testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
	}
	// Flag to tell aapt to keep the attribute ids around
	aaptOptions {
		additionalParameters("--no-version-vectors")
	}

	buildTypes.forEach {
		it.isMinifyEnabled = true
		it.isShrinkResources = false

		it.proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
	}

	compileOptions {
		encoding = "UTF-8"
		sourceCompatibility = JavaVersion.VERSION_1_8
		targetCompatibility = JavaVersion.VERSION_1_8
	}

	lint {
		checkReleaseBuilds = false
		abortOnError = true
		warningsAsErrors = true

		error.add("DefaultLocale")

		baseline = file("config/lint/lint-baseline.xml")
		lintConfig = file("config/lint/lint.xml")
	}

	packagingOptions {
		excludes.add("META-INF/*")
	}

	testOptions {
		animationsDisabled = true
	}
}

pmd {
	toolVersion = "6.36.0"
}

tasks.register("pmd", Pmd::class) {
	dependsOn.add("assembleDebug")
	ruleSetFiles = files("${project.rootDir}/config/pmd/rules.xml")
	ruleSets = emptyList() // otherwise defaults clash with the list in rules.xml
	source("src/main/java/org/quantumbadger")
	include("**/*.java")
	isConsoleOutput = true
}

checkstyle {
	toolVersion = "8.35"
}

tasks.register("Checkstyle", Checkstyle::class) {
	source("src/main/java/org/quantumbadger")
	ignoreFailures = false
	setShowViolations(true)
	include("**/*.java")
	classpath = files()
	maxWarnings = 0
	configFile = rootProject.file("${project.rootDir}/config/checkstyle/checkstyle.xml")
}
