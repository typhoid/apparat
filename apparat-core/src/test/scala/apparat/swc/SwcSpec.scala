package apparat.swc

import org.specs.SpecificationWithJUnit

/**
 * @author Maxim Zaks
 */
class SwcSpec extends SpecificationWithJUnit {
  val swc = Swc.fromFile("target/test-classes/ApparatTest.swc")

  "SWC is a zip file that" should {
    "contain catalog.xml" >> {
      swc.catalog.get.isEmpty must_== false
    }

    "contain a library" >> {
      swc.library.get.isEmpty must_== false
    }
  }

}