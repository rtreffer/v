package de.measite.v

import de.measite.v.data._

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class MySpecTest extends JUnit4(MySpec)

class MySpecRunner extends ConsoleRunner(MySpec)

object MySpec extends Specification {
  name= "Specification"
  "KVector"    isSpecifiedBy KVectorSpec
  "RRectangle" isSpecifiedBy RRectangleSpec
} 
