package net.geohex

/*
 * "scala-geohex" is a Scala porting of "GeoHex" and "geohex4j",
 *  licensed under Creative Commons BY-SA 2.1 Japan License.
 *
 * * "GeoHex" is licensed by @sa2da (http://geogames.net) 
 *    under Creative Commons BY-SA 2.1 Japan License.
 * * "geohex4j" (https://github.com/chsh/geohex4j)  is licensed by CHIKURA Shinsaku
 *    under Creative Commons BY-SA 2.1 Japan License.
 * 
 * Copyright (C) 2010 Pomu TAKEUCHI, Some rights reserved
 */
import scala.math._

case class Lat(lat: Double)
case class Lon(lon: Double)	
case class Loc(lat: Lat, lon: Lon)

case class XY(x: Double, y: Double) {
	def +(that: XY) = XY(x + that.x, y + that.y)
	def -(that: XY) = XY(x - that.x, y - that.y)
	def map(fn: Double => Double) = XY(fn(x), fn(y))
	def mapAsList[A](fn: Double => A): List[A] = List(fn(x), fn(y))
	def swap = XY(y, x)
}

case class Zone(lat: Lat, lon: Lon, x: Long, y: Long, code: String) {
	lazy val level = code.length - 2
	
	lazy val hexSize: Double = calcHexSize(level + 2)
	
	lazy val hexCoords: List[Loc] = {
		val xy: XY = loc2xy(lat, lon)
		val deg = math.tan(math.Pi * (60 / 180))
		
		val top = xy2loc(xy.x, xy.y + deg * hexSize).lat 
		val btm = xy2loc(xy.x, xy.y - deg * hexSize).lat 
		
		val l = xy2loc(xy.x - 2 * hexSize, xy.y).lon 
		val r = xy2loc(xy.x + 2 * hexSize, xy.y).lon 
		val cl = xy2loc(xy.x - 1 * hexSize, xy.y).lon 
		val cr = xy2loc(xy.x + 1 * hexSize, xy.y).lon 
		
		List(
			(this.lat, l),
			(top, cl),
			(top, cr),
			(this.lat, r),
			(btm, cr),
			(btm, cl)
		)
	}
}

object GeoHex {
	val version = "3.0"
	def encode(lat: Double, lon: Double, level: Int) = getZoneByLocation(lat, lon, level).code
	
	private def calcHexSizeAndUnit(level: Int): (Double, XY) = {
		val size = calcHexSize(level)
		def unitXY(size: Double): XY = (6.0 * size, 6.0 * size * K)
		(size, unitXY(size))
	}
	private def unitMax(unit: XY): Long = round(BASE / unit.x + BASE / unit.y)
	
	def getZoneByLocation(lat: Double, lon: Double, level: Int): Zone = {
		if (lat < -90 || lat > 90) 
			throw new IllegalArgumentException("latitude must be between -90 and 90");
		if (lon < -180 || lon > 180)
			throw new IllegalArgumentException("longitude must be between -180 and 180");
		if (level < 0 || level > 15)
			throw new IllegalArgumentException("level must be between 0 and 15");

    val lv = level + 2
		val (size, unit) = calcHexSizeAndUnit(lv)

		val XY(lonGrid, latGrid) = loc2xy(Lat(lat), Lon(lon))
		
		val posXY = XY(
					(lonGrid + latGrid / K) / unit.x,
					(latGrid - K * lonGrid) / unit.y
				)
		val xy0 = posXY.map {floor}
		val q = posXY - xy0
		var xy = posXY.map(round(_))

		if (q.y > -q.x + 1) {
			if ((q.y < 2 * q.x) && (q.y > 0.5 * q.x)) {
				xy = xy0.map {1+}
			}
		} else if (q.y < -q.x + 1) {
			if ((q.y > (2 * q.x) - 1) && (q.y < (0.5 * q.x) + 0.5)) {
				xy = xy0
			}
		}
		val hLat = (K * xy.x * unit.x + xy.y * unit.y) / 2
		val hLon = (hLat - xy.y * unit.y) / K
		
		var loc = xy2loc(hLon, hLat)
		if (BASE - hLon < size) {
			//println((BASE-hLon), size, loc.lon)
			loc = loc.copy(lon = Lon(180))
			xy = xy.swap
		}

    def code3(code3: List[Int], mod: Double)(i: Int): (List[Int],  Double) = {
      val hPow = math.pow(3, lv - i)
      if (mod >= math.ceil(hPow / 2)) {
        (code3 ::: List(2), mod - hPow)
      } else if (mod <= -math.ceil(hPow / 2)) {
        (code3 ::: List(0), mod + hPow)
      } else {
        (code3 ::: List(1), mod)
      }
    }

    val (code3x, modX) = (0 to lv).foldLeft((List[Int](), xy.x)){(a, i) =>
      code3(a._1, a._2)(i)
    }
    val (code3y, modY) = (0 to lv).foldLeft((List[Int](), xy.y)){(a, i) =>
      code3(a._1, a._2)(i)
    }

    val code = code3x.zip(code3y).map {
      case (x,y) => Integer.parseInt(("" + x + y).toString, 3).toString
    }.mkString

    val h2 = code.substring(3)
    val h1 = code.take(3).mkString.toInt
    val a1 = math.floor(h1 / 30).toInt
    val a2 = h1 % 30
    val r = "" + KEY(a1) + KEY(a2) + h2

		Zone(loc.lat, loc.lon, xy.x.toLong, xy.y.toLong, r)
	}
	
	private def calcCode(xy: XY, level: Int, unit: XY): String = {
		val max = unitMax(unit)
		
		val latY = (K * xy.x * unit.x + xy.y * unit.y) / 2.0
		val lonX = (latY - xy.y * unit.y) / K
		
		val xyAbs = xy.map {z =>
			val p = if (z < 0) 1 else 0
			math.abs(z) * 2 + p
		}
		
		def enc(i: Int)(abs: Double): Char = KEY(i match {
			case 0 => math.floor(abs % 3600).intValue % 60
			case i => math.floor(abs % pow(60, i+1)).intValue / pow(60, i).intValue
		})

		var res: List[Char] = Nil
		// 1, 60, 3600, 216000, 12960000
		(0 to 4).takeWhile(max >= pow(60, _) / 2).foreach{i =>
			res = xyAbs.mapAsList(enc(i)_) ::: res
		}
		res = KEY(level % 60) :: res
		
		res.mkString
	}
	
	def decode(code: String) = getZoneByCode(code)
	
	def getZoneByCode(code: String): Zone = {
    val level = code.length
    val size = calcHexSize(level)
    val unitX = 6 * size
    val unitY = 6 * size * K

    var x, y = 0L

    var dec9 = "" + (KEY.indexOf(code(0)) * 30 + KEY.indexOf(code(1))) + code.substring(2)

    val Inc15 = "15"
    val Exc125 = "125"
    if (Inc15.contains(dec9(0)) && !Exc125.contains(dec9(1)) && !Exc125.contains(dec9(2))) {
      if (dec9(0) == '5') {
        dec9 = "7" + dec9.drop(1)
      } else if (dec9(0) == '1') {
        dec9 = "3" + dec9.drop(1)
      }
    }

    val fill = (level + 1 - dec9.length)
    dec9 = ("0" * fill) + dec9

    dec9.map{c =>
      Integer.toString(c.toString.toInt, 3) match {
        case d if d.length == 1 => "0" + d
        case d => d
      }
    }.mkString.grouped(2).map{cc =>
      (cc(0), cc(1))
    }.zipWithIndex.foreach{case ((decX, decY), i) =>
      val pow = math.pow(3, level - i).toLong
      x += (decX match {
        case '0' => -pow
        case '2' => +pow
        case _ => 0
      })
      y += (decY match {
        case '0' => -pow
        case '2' => +pow
        case _ => 0
       })
    }

    val latY = (K * x * unitX + y * unitY) / 2.0
    val lonX = (latY - y * unitY) / K

    val loc = xy2loc(lonX, latY) match {
      case loc if loc.lon > 180 =>
        loc.copy(lon = Lon(loc.lon - 360))
      case loc if loc.lon < -180 =>
        loc.copy(lon = Lon(loc.lon + 360))
      case loc => loc
    }
    Zone(loc.lat, loc.lon, x, y, code)
	}
	
	private def xyu2loc(x: Double, y: Double, unit: XY): Loc = {
		val latY = (K * x * unit.x + y * unit.y) / 2
		val lonX = (latY - y * unit.y) / K
		
		xy2loc(lonX, latY)
	}

  /*
	def getZoneByXY(x: Long, y: Long, level: Int): Zone = {
		val (_, unit) = calcHexSizeAndUnit(level)
		
		val loc = xyu2loc(x, y, unit)
		Zone(loc.lat, loc.lon, x, y, calcCode(XY(x,y),level,unit))
	}
	*/
}