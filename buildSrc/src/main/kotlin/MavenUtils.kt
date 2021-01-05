import org.gradle.api.publish.maven.MavenPublication

/** Adds common metadata to the pom descriptor of this maven publication. */
fun MavenPublication.fillPomMetadata() = apply {
    pom {
        developers {
            developer {
                name.set("Nikolas Havrikov")
                email.set("nikolas.havrikov@cispa.de")
            }
        }
        licenses {
            license {
                name.set("MIT License")
                url.set("https://opensource.org/licenses/MIT")
            }
        }
    }
}
