import org.jfrog.gradle.plugin.artifactory.dsl.ArtifactoryPluginConvention
import org.jfrog.gradle.plugin.artifactory.dsl.DoubleDelegateWrapper
import org.jfrog.gradle.plugin.artifactory.dsl.PublisherConfig
import org.jfrog.gradle.plugin.artifactory.task.ArtifactoryTask

plugins {
    base
    id("com.jfrog.artifactory") version Versions.artifactoryPlugin apply false
}

subprojects {
    group = "de.cispa.se.tribble"
    apply(plugin = "org.gradle.scala")
    apply(plugin = "org.gradle.maven-publish")
    apply(plugin = "com.jfrog.artifactory")

    repositories {
        mavenCentral()
    }

    dependencies{
        "implementation"("org.scala-lang:scala-library:${Versions.scala}")
    }

    tasks.withType<ScalaCompile> {
        scalaCompileOptions.encoding = "UTF-8"
        scalaCompileOptions.isDeprecation = true
        scalaCompileOptions.isUnchecked = true
    }

    the<JavaPluginExtension>().apply {
        targetCompatibility = JavaVersion.VERSION_11
        withSourcesJar()
        withJavadocJar()
    }

    the<ArtifactoryPluginConvention>().apply {
        setContextUrl(System.getenv("artifactory_context_url"))
        publish(delegateClosureOf<PublisherConfig> {
            setPublishIvy(false)
            repository(delegateClosureOf<DoubleDelegateWrapper> {
                setProperty("repoKey", "libs-release-local")
                setProperty("username", System.getenv("artifactory_user"))
                setProperty("password", System.getenv("artifactory_password"))
                setProperty("maven", true)
            })
            defaults(delegateClosureOf<ArtifactoryTask> {
                publications("mavenJava")
            })
        })
    }

    tasks.named("artifactoryPublish") {
        dependsOn("build")
    }
}

// the tribble-tool project will set up its fat jar publishing itself
configure(subprojects.filter { it.name != "tribble-tool" }) {
    the<PublishingExtension>().apply {
        publications {
            create<MavenPublication>("mavenJava") {
                from(components["java"])
                fillPomMetadata()
            }
        }
    }
}
