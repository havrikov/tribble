import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    id("com.github.johnrengelman.shadow") version Versions.shadowPlugin
}

version = Versions.tool

dependencies {
    implementation(project(":tribble-model"))
    implementation(project(":tribble-core"))

    implementation("dk.brics", "automaton", Versions.automaton)

    compileOnly("org.backuity.clist", "clist-macros_2.12", Versions.clist)
    implementation("org.backuity.clist", "clist-core_2.12", Versions.clist)

    implementation("org.json4s", "json4s-native_2.12", Versions.json4sNative)
    implementation("com.github.tototoshi", "scala-csv_2.12", Versions.scalaCSV)

    implementation("org.log4s", "log4s_2.12", Versions.log4s)
    runtimeOnly("ch.qos.logback", "logback-classic", Versions.logback)
}

val shadow = tasks.named<ShadowJar>("shadowJar") {
    archiveBaseName.set("tribble")
    archiveClassifier.set("")
    manifest.attributes["Main-Class"] = "de.cispa.se.tribble.Main"
    minimize()
}

// make sure the runnable jar containing the entire classpath is built by default
tasks.named("assemble") {
    dependsOn(shadow)
}

// disable the standard jar task as part of the assembly
tasks.withType<Jar> {
    enabled = name == "shadowJar"
}

publishing {
    publications {
        create<MavenPublication>("mavenJava") {
            project.shadow.component(this)
            fillPomMetadata()
        }
    }
}
