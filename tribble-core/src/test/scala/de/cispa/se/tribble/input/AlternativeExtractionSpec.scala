package de.cispa.se.tribble
package input

import de.cispa.se.tribble.dsl._

class AlternativeExtractionSpec extends TestSpecification with SharedModelAssembler {

  "The alternative extraction" should "extract alternatives" in {
    val g = Grammar(
      'S := 'A | 'B,
      'A := "a1" ~ ('D | "a2").? ~ ("a3" ~ ('D | "a4" | "a5")).rep,
      'B := "b1" | "b2" ~ ("b3" ~ ('D | 'C ~ "b4") | 'C),
      'C := "c1" | "c2" ~ ("c3" | "c4"),
      'D := "d1" | "d2"
    )
    val grammar: GrammarRepr = modelAssembler.assemble(g.productions)

    val extracted: GrammarRepr = AlternativeExtraction.process(grammar)

    // disambiguate newly created nodes
    new NodeDisambiguation().process(extracted)

    val eG = Grammar(
      'S := 'A | 'B,
      'A := "a1" ~ 'A_a0.? ~ ("a3" ~ 'A_a1).rep,
      'A_a0 := 'D | "a2",
      'A_a1 := 'D | "a4" | "a5",
      'B := "b1" | "b2" ~ 'B_a0,
      'B_a0 := "b3" ~ 'B_a0_a0 | 'C,
      'B_a0_a0 := 'D | 'C ~ "b4",
      'C := "c1" | "c2" ~ 'C_a0,
      'C_a0 := "c3" | "c4",
      'D := "d1" | "d2"
    )

    val expected: GrammarRepr = modelAssembler.assemble(eG.productions)

    extracted shouldEqual expected
  }
}
