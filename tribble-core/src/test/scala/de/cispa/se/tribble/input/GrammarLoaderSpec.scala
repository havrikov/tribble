package de.cispa.se.tribble
package input

import java.io.File

class GrammarLoaderSpec extends TestSpecification with SharedModelAssembler {

  private val loader = new GrammarLoader(CompileGrammar(modelAssembler), EmptyGrammarCache)

  "The GrammarLoader" should "be able to load a valid grammar" in {
    val grammars = Table("path",
      "URL",
      "Clojure",
      "CSS3",
      "CSV",
      "Iso8601",
      "Json",
      "Kotlin",
      "Regex",
    )

    forAll(grammars) { name =>
      val grammar = loader.loadGrammar(new File(s"src/test/resources/typesafe/$name.scala"))
    }
  }

  it should "not compile empty grammars" in {
    assertThrows[java.lang.reflect.InvocationTargetException] {
      loader.loadGrammar(new File("src/test/resources/typesafe/Empty.scala"))
    }
  }
}
