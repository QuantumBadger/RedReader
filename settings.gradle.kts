pluginManagement {
	repositories {
		gradlePluginPortal()
		google()
		mavenCentral()
	}
}

dependencyResolutionManagement {
	repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
	repositories {
		google()
		mavenCentral()
	}
}

include(":redreader-common")
project(":redreader-common").projectDir = File("./libs/redreader-common")

include(":redreader-datamodel")
project(":redreader-datamodel").projectDir = File("./libs/redreader-datamodel")
