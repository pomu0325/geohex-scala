package net.geohex

import org.scalatest.Spec
import scala.io._

class GeoHexSpec extends Spec {
	// make it easier to switch test target: Scala <-> Java
	val encode = GeoHex.encode _
	val decode = GeoHex.decode _
	val getZoneByXY = GeoHex.getZoneByXY _
	
	describe("encode") {
		it ("'s result should match with testdata.csv") {
			doAll {(lat: Lat, lon: Lon, level: Int, expect: String) =>
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
		
		it("should throw IAE when level is not between 1 ~ 24") {
			intercept[IllegalArgumentException] {
				encode(40, 139, 0)
				encode(40, 139, 25)
			}
		}
	}
	
	
	describe("decode") {
		it ("'s result should match with testdata.csv") {
			doAll {(lat: Lat, lon: Lon, level: Int, code: String) =>
				//val z @ Zone(aLat, aLon, _, _, aCode) = GeoHex.decode(code)
				val z = decode(code)
				printf("Zone(%s %s %s %s %s)\n", z.lat, z.lon, z.x, z.y, z.code)
				assert(z.code === code)
				
				// re-encode with returned (lat,lon,lv), should yield same code
				val aCode2 = encode(z.lat, z.lon, level)
				assert(aCode2 === code)
			}
		}
	}
	
	describe("getZoneByXY") {
		it ("'s result should match with testdata.csv") {
			doAll {(lat: Lat, lon: Lon, level: Int, code: String) =>
				val z = decode(code)
				
				// re-encode with returned (x,y,lv), should yield same lat,lon
				val z2 = getZoneByXY(z.x, z.y, level)
				assert(z2.code === code)
				assert(z2.lat === z.lat)
				assert(z2.lon === z.lon)
			}
		}
	}
	
	def doAll(fn: (Lat, Lon, Int, String) => Unit) = {
		// lat,lon,level,code
		for (line <- Source.fromFile("src/test/resources/testdata.csv").getLines) {
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