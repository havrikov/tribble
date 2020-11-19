package de.cispa.se.tribble

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TestSpecification extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks with Inside
