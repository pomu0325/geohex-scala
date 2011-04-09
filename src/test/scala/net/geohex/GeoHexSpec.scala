package net.geohex

import org.scalatest.Spec
import scala.io._

class GeoHexSpec extends Spec {
	// make it easier to switch test target: Scala <-> Java
	val encode = GeoHex.encode _
	val decode = GeoHex.decode _
	//val getZoneByXY = GeoHex.getZoneByXY _
	
	describe("encode") {
		it ("'s result should match with testdata_ll2hex.txt") {
			doAll ("src/test/resources/testdata_ll2hex.txt") {(lat: Lat, lon: Lon, level: Int, expect: String) =>
				val actual = encode(lat, lon, level)
				assert(actual === expect)
			}
		}
		
		it("should throw IAE when lon is not between -180 ~ 180") {
			intercept[IllegalArgumentException] {
				encode(40, 180.1, 1)
				encode(40, -180.1, 1)
			}
		}
		
		it("should throw IAE when lat is not between -90 ~ 90") {
			intercept[IllegalArgumentException] {
				encode(90.1, 139, 1)
				encode(-90.1, 139, 1)
			}
		}
		
		it("should throw IAE when level is not between 0 ~ 15") {
			intercept[IllegalArgumentException] {
				encode(40, 139, -1)
				encode(40, 139, 16)
			}

      encode(40, 139, 0)
      encode(40, 139, 15)
		}
	}
	
	describe("decode") {
		it ("'s result should match with testdata_hex2ll.txt") {
			doAll ("src/test/resources/testdata_hex2ll.txt") {(lat: Lat, lon: Lon, level: Int, code: String) =>
				val z = decode(code)
				//println(z)
				assert(z.code === code)
        assert(z.level === level)
        assert((z.lat - lat).toLong === 0)
        val lonDiff = (z.lon - lon).toLong match {
          case -360 | 360 => 0
          case e => e
        }

        assert(lonDiff === 0)

				// re-encode with returned (lat,lon,lv), should yield same code
				val aCode2 = encode(z.lat, z.lon, level)
				assert(aCode2 === code)
			}
		}
	}

  describe("getZoneByLocation") {
    it ("'s hexSize should match with testdata_hex2hexsize.txt") {
      doAll ("src/test/resources/testdata_ll2hexsize.txt") {(lat: Lat, lon: Lon, level: Int, code: String) =>
        val z = GeoHex.getZoneByLocation(lat, lon, level)
        assert(z.hexSize.toString === code)
      }
    }
  }

  /*
	describe("getZoneByXY") {
		it ("'s result should match with testdata.csv") {
			doAll ("src/test/resources/testdata_ll2hex.txt") {(lat: Lat, lon: Lon, level: Int, code: String) =>
				val z = decode(code)
				
				// re-encode with returned (x,y,lv), should yield same lat,lon
				val z2 = getZoneByXY(z.x, z.y, level)
				assert(z2.code === code)
				assert(z2.lat === z.lat)
				assert(z2.lon === z.lon)
			}
		}
	}
	*/

	def doAll(file: String = "src/test/resources/testdata.csv")(fn: (Lat, Lon, Int, String) => Unit) = {
		// lat,lon,level,code
		for (line <- Source.fromFile(file).getLines) {
			println(line)
			if (!line.startsWith("#")) {	// skip commented line
				val vals = line.split(",")
				vals.toList match {
					case List(lat, lon, level, code) =>
						fn(Lat(lat.toDouble), Lon(lon.toDouble), level.toInt, code)
					case _ => fail("invalid line in testdata.csv: " + line)
				}
			}
		}
	}
}