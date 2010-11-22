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

case class XY(x: Double, y: Double)

case class Zone(lat: Lat, lon: Lon, x: Double, y: Double, code: String) {
	lazy val level = KEY.indexOf(code(0))
	
	lazy val hexSize: Double = calcHexSize(level)
	
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
	val version = "2.03"
	def encode(lat: Double, lon: Double, level: Int) = getZoneByLocation(lat, lon, level).code
	
	private def unitXY(size: Double): XY = (6.0 * size, 6.0 * size * K)
	private def unitMax(unit: XY): Long = round(BASE / unit.x + BASE / unit.y)
	
	def getZoneByLocation(lat: Double, lon: Double, level: Int): Zone = {
		if (lat < -90 || lat > 90) 
			throw new IllegalArgumentException("latitude must be between -90 and 90");
		if (lon < -180 || lon > 180)
			throw new IllegalArgumentException("longitude must be between -180 and 180");
		if (level < 0 || level > 24) 
			throw new IllegalArgumentException("level must be between 1 and 24");
		
		val size = calcHexSize(level)

		val xy: XY = loc2xy(Lat(lat), Lon(lon))
		val (lonGrid, latGrid) = (xy.x, xy.y)
		val unit = unitXY(size)
		
		val posX: Double = (lonGrid + latGrid / K) / unit.x 
		val posY: Double = (latGrid - K * lonGrid) / unit.y
		
		val x0: Long = math.floor(posX).longValue
		val y0: Long = math.floor(posY).longValue
		
		val xQ = posX - x0
		val yQ = posY - y0
		
		var x: Long = math.round(posX)
		var y: Long = math.round(posY)
		//printf("posY: %s, y0: %s, y: %s\n", posY, y0, y)

		if (yQ > -xQ + 1) {
			if ((yQ < 2 * xQ) && (yQ > 0.5 * xQ)) {
				x = x0 + 1
				y = y0 + 1
			}
		} else if (yQ < -xQ + 1) {
			if ((yQ > (2 * xQ) - 1) && (yQ < (0.5 * xQ) + 0.5)) {
				x = x0
				y = y0
			}
		}
		val hLat = (K * x * unit.x + y * unit.y) / 2
		val hLon = (hLat - y * unit.y) / K
		
		var loc = xy2loc(hLon, hLat)
		if (BASE - hLon < size) {
			println((BASE-hLon), size, loc.lon)
			loc = loc.copy(lon = Lon(180))
			val tmp = x
			x = y
			y = tmp
		}
		
		Zone(loc.lat, loc.lon, x, y, calcCode(x,y,level,unit))
	}
	
	private def calcCode(x: Double, y: Double, level: Int, unit: XY): String = {
		val max = unitMax(unit)
		
		val latY = (K * x * unit.x + y * unit.y) / 2.0
		val lonX = (latY - y * unit.y) / K
		
		def _abs(z: Double): Long = {
			val p = if (z < 0) 1 else 0
			math.abs(z).longValue * 2 + p
		}
		val xAbs = _abs(x)
		val yAbs = _abs(y)
		
		def enc(abs: Long, i: Int): Char = KEY(i match {
			case 0 => math.floor(abs % 3600).intValue % 60
			case i => math.floor(abs % pow(60, i+1)).intValue / pow(60, i).intValue
		})
		
		var res: List[Char] = Nil
		// 1, 60, 3600, 216000, 12960000
		(0 to 4).takeWhile(max >= pow(60, _) / 2).foreach{i =>
			res = enc(xAbs, i) :: enc(yAbs, i) :: res
		}
		res = KEY(level % 60) :: res
		
		res.mkString
	}
	
	def decode(code: String) = getZoneByCode(code)
	
	def getZoneByCode(code: String): Zone = {
		val level = KEY.indexOf(code(0))
		val size = calcHexSize(level)
		val unit = unitXY(size)
		
		// 12960000 ...
		//(5 to (1, -1)).find(i => max >= pow(60, i - 1) / 2) match {
		//	case Some(codeLen) =>
		var (x, y) = code.tail.reverse.toList.sliding(2, 2).zipWithIndex.map {
			case (List(y, x), i) => 
				(KEY.indexOf(x) * pow(60, i),
				 KEY.indexOf(y) * pow(60, i))
		} reduceLeft {
			(_, _) match {
				case ((x, y), (x2, y2)) => (x + x2, y + y2)
			}
		}
		//	case _ => throw new IllegalArgumentException(code)
		//}
		val fn = (a: Double) => if ((a % 2) != 0) -(a - 1) / 2 else a / 2
		x = fn(x)
		y = fn(y)
		
		val latY = (K * x * unit.x + y * unit.y) / 2
		val lonX = (latY - y * unit.y) / K
		
		val loc = xy2loc(lonX, latY)
		Zone(loc.lat, loc.lon, x, y, code)
	}
	
	def getZoneByXY(x: Double, y: Double, level: Int): Zone = {
		val size = calcHexSize(level)
		val unit = unitXY(size)
		
		val latY = (K * x * unit.x + y * unit.y) / 2
		val lonX = (latY - y * unit.y) / K
		
		val loc = xy2loc(lonX, latY)
		Zone(loc.lat, loc.lon, x, y, calcCode(x,y,level,unit))
	}
}