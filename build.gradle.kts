@file:Suppress("UnstableApiUsage")

plugins {
	alias(libs.plugins.android.application)
	alias(libs.plugins.kotlin.android)
	alias(libs.plugins.kotlin.serialization)
	alias(libs.plugins.kotlin.parcelize)
    pmd
	checkstyle

	// If plugin is used in multiple subprojects then it needs to be imported with apply(false) in the root project,
	// otherwise bad things will happen.
	// The reason is that Gradle isolates class loaders between subprojects and some plugins can't handle it.
	// Root project's class loader however is available to all subprojects and importing plugin here (but not applying it) solves the issue
	alias(libs.plugins.kotlin.jvm) apply false
}

dependencies {
	implementation(libs.androidx.multidex)

	implementation(project(":redreader-common"))
	implementation(project(":redreader-datamodel"))

	coreLibraryDesugaring(libs.jdk.desugar)

	implementation(libs.kotlinx.serialization.json)
	implementation(libs.kotlinx.serialization.json.okio)
	implementation(libs.kotlin.reflect)

	implementation(libs.androidx.annotation)
	implementation(libs.androidx.appcompat)
	implementation(libs.androidx.constraintlayout)
	implementation(libs.androidx.core)
	implementation(libs.androidx.fragment)
	implementation(libs.androidx.preference)
	implementation(libs.androidx.recyclerview)
	implementation(libs.androidx.swiperefreshlayout)

	implementation(libs.google.flexbox)
	implementation(libs.google.material)

	implementation(libs.jackson.core)
	implementation(libs.commons.lang)

	implementation(libs.commons.text)

	implementation(libs.okhttp)
	implementation(libs.netcipher.webkit)
	implementation(libs.exoplayer.core)
	implementation(libs.exoplayer.ui)
	implementation(libs.zstd) {
		artifact {
			type = "aar"
		}
	}

	testImplementation(libs.junit)

	androidTestImplementation(libs.androidx.test.core)
	androidTestImplementation(libs.androidx.test.espresso.core)
	androidTestImplementation(libs.androidx.test.espresso.contrib)
	androidTestImplementation(libs.androidx.test.rules)
	androidTestImplementation(libs.androidx.test.junit)
}

android {
	compileSdk = libs.versions.sdk.compile.get().toInt()
	ndkVersion = libs.versions.ndk.get()
	namespace = "org.quantumbadger.redreader"

	defaultConfig {
		applicationId = "org.quantumbadger.redreader"
		minSdk = libs.versions.sdk.min.get().toInt()
		targetSdk = libs.versions.sdk.target.get().toInt()
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

	buildTypes.configureEach {
		isMinifyEnabled = true
		isShrinkResources = false

		proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
	}

	compileOptions {
		encoding = "UTF-8"
		isCoreLibraryDesugaringEnabled = true
		JavaVersion.toVersion(libs.versions.java.get()).let {
			sourceCompatibility = it
			targetCompatibility = it
		}
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
		jvmTarget = libs.versions.java.get()
	}

	buildFeatures {
		buildConfig = true
	}
}

pmd {
	toolVersion = libs.versions.pmd.get()
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
	toolVersion = libs.versions.checkstyle.get()
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
