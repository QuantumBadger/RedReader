@file:Suppress("UnstableApiUsage")

plugins {
	id("com.android.application") version "8.2.2"
	kotlin("android") version "1.9.22"
	kotlin("plugin.serialization") version "1.9.22"
	kotlin("plugin.parcelize") version "1.9.22"
    pmd
	checkstyle
}

dependencies {

	implementation("androidx.multidex:multidex:2.0.1")

	implementation(project(":redreader-common"))
	implementation(project(":redreader-datamodel"))

	coreLibraryDesugaring("com.android.tools:desugar_jdk_libs:2.0.4")
	implementation("androidx.core:core-ktx:1.9.0")
	implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.3")
	implementation("org.jetbrains.kotlin:kotlin-reflect:1.9.22") // TODO use constant
	implementation("androidx.annotation:annotation:1.7.1")
	implementation("androidx.appcompat:appcompat:1.6.1")
	implementation("androidx.recyclerview:recyclerview:1.3.2")
	implementation("com.google.android.flexbox:flexbox:3.0.0")
	implementation("androidx.swiperefreshlayout:swiperefreshlayout:1.1.0")
	implementation("androidx.preference:preference:1.2.1")
	implementation("com.google.android.material:material:1.9.0")
	implementation("androidx.constraintlayout:constraintlayout:2.1.4")
	implementation("androidx.fragment:fragment:1.6.2")

	implementation("com.fasterxml.jackson.core:jackson-core:2.16.1")
	implementation("org.apache.commons:commons-lang3:3.14.0")

	implementation("org.apache.commons:commons-text:1.11.0")

	implementation("com.squareup.okhttp3:okhttp:3.12.13")
	implementation("info.guardianproject.netcipher:netcipher-webkit:2.1.0")
	implementation("com.google.android.exoplayer:exoplayer-core:2.19.0")
	implementation("com.google.android.exoplayer:exoplayer-ui:2.19.0")
	implementation("com.github.luben:zstd-jni:1.5.5-11@aar")

	testImplementation("junit:junit:4.13.2")

	androidTestImplementation("androidx.test.espresso:espresso-core:3.5.1")
	androidTestImplementation("androidx.test:rules:1.5.0")
	androidTestImplementation("androidx.test.espresso:espresso-contrib:3.5.1")
}

android {
	compileSdk = 33
	ndkVersion = "23.1.7779620"
	namespace = "org.quantumbadger.redreader"

	defaultConfig {
		applicationId = "org.quantumbadger.redreader"
		minSdk = 16
		targetSdk = 33
		versionCode = 112
		versionName = "1.23.1"

		multiDexEnabled = true
		vectorDrawables.generatedDensities("mdpi", "hdpi", "xhdpi", "xxhdpi", "xxxhdpi")
		testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
	}

	// Flag to tell aapt to keep the attribute ids around
	androidResources {
		additionalParameters.add("--no-version-vectors")
	}

	buildTypes.forEach {
		it.isMinifyEnabled = true
		it.isShrinkResources = false

		it.proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
	}

	compileOptions {
		encoding = "UTF-8"
		isCoreLibraryDesugaringEnabled = true
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

	packaging {
		resources.excludes.add("META-INF/*")
	}

	testOptions {
		animationsDisabled = true
	}

	kotlinOptions {
		jvmTarget = "1.8"
	}

	buildFeatures {
		buildConfig = true
	}
}

pmd {
	toolVersion = "6.55.0"
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
	toolVersion = "10.12.5"
}

tasks.register("Checkstyle", Checkstyle::class) {
	source("src/main/java/org/quantumbadger")
	ignoreFailures = false
	isShowViolations = true
	include("**/*.java", "**/*.kt")
	classpath = files()
	maxWarnings = 0
	configFile = rootProject.file("${project.rootDir}/config/checkstyle/checkstyle.xml")
}
