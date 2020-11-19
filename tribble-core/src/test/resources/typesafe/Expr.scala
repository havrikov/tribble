import de.cispa.se.tribble.dsl._

Grammar(
  'start := 'expr/*38*/ ~ "\n"/*40*/.?,
  'expr := 'term/*12*/ | 'term/*14*/ ~ "+"/*15*/ ~ 'expr/*16*/ | 'term/*18*/ ~ "-"/*19*/ ~ 'expr/*20*/,
  'term := 'factor/*2*/ | 'term/*4*/ ~ "*"/*5*/ ~ 'factor/*6*/ | 'term/*8*/ ~ "/"/*9*/ ~ 'factor/*10*/,
  'factor := 'literal/*22*/ | "("/*24*/ ~ 'expr/*25*/ ~ ")"/*26*/ | "+"/*28*/ ~ 'factor/*29*/ | "-"/*31*/ ~ 'factor/*32*/,
  'literal := 'number/*35*/ | 'variable/*36*/,
  'variable := "[a-z]".regex/*33*/,
  'number := "[0-9]+".regex/*0*/
)
