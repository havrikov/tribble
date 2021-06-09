plugins {
    `java-library`
    `java-test-fixtures`
    id("com.github.maiflai.scalatest") version Versions.scalatestPlugin
}

version = Versions.core

dependencies {
    api(project(":tribble-model"))
    api("org.jgrapht", "jgrapht-core", Versions.jGraphT)

    implementation("org.log4s", "log4s_2.12", Versions.log4s)
    runtimeOnly("ch.qos.logback", "logback-classic", Versions.logback)

    implementation("com.lihaoyi", "fastparse_2.12", Versions.fastparse)
    implementation("com.github.pathikrit", "better-files_2.12", Versions.betterFiles)

    implementation("org.scala-lang", "scala-compiler", Versions.scala)
    implementation("dk.brics", "automaton", Versions.automaton)
    implementation("org.apache.commons", "commons-text", Versions.commonsText)


    testFixturesApi("org.scalatest", "scalatest_2.12", Versions.scalatest)
    testImplementation("org.scalatestplus", "scalacheck-1-15_2.12", Versions.scalatestPlus)
    testImplementation("org.scalacheck", "scalacheck_2.12", Versions.scalacheck)
    // needed for the report generation in the scalatest gradle plugin
    testRuntimeOnly("com.vladsch.flexmark", "flexmark-profile-pegdown", Versions.flexmark)
}
