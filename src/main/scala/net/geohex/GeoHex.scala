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

case class Zone(lat: Lat, lon: Lon, x: Long, y: Long, code: String) {
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
		printf("posY: %s, y0: %s, y: %s\n", posY, y0, y)
		
		val max = unitMax(unit)

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
		
		calcZone(x, y, level, max, loc)
	}
	
	private def calcZone(x: Long, y: Long, level: Int, max: Long, loc: Loc) = {
		val xP: Long = if (x < 0) 1 else 0
		val yP: Long = if (y < 0) 1 else 0
		val xAbs = math.abs(x) * 2 + xP
		val yAbs = math.abs(y) * 2 + yP
		
		def fn(abs: Long): Tuple5[Int, Int, Int, Int, Int] = (
			math.floor(abs % 777600000).intValue / 12960000,
			math.floor(abs % 12960000).intValue / 216000,
			math.floor(abs % 216000).intValue / 3600,
			math.floor(abs % 3600).intValue / 60,
			math.floor(abs % 3600).intValue % 60
		)
		
		val (x10000, x1000, x100, x10, x1) = fn(xAbs)
		val (y10000, y1000, y100, y10, y1) = fn(yAbs)
		
		println(fn(xAbs))
		println(fn(yAbs))
		
		val sb = new StringBuilder
		sb.append(KEY(level % 60))

		if (max >= 12960000 / 2) sb.append(KEY(x10000)).append(KEY(y10000))
		if (max >= 216000 / 2) sb.append(KEY(x1000)).append(KEY(y1000))
		if (max >= 3600 / 2) sb.append(KEY(x100)).append(KEY(y100))
		if (max >= 60 /2) sb.append(KEY(x10)).append(KEY(y10))
		sb.append(KEY(x1)).append(KEY(y1))
		
		Zone(loc.lat, loc.lon, x, y, sb.toString)
	}
	
	def decode(code: String) = getZoneByCode(code)
	
	def getZoneByCode(code: String): Zone = {
		val level = KEY.indexOf(code(0))
		val size = calcHexSize(level)
		val unit = unitXY(size)
		val max = unitMax(unit)
		
		// 12960000 ...
		//(5 to (1, -1)).find(i => max >= pow(60, i - 1) / 2) match {
		//	case Some(codeLen) =>
		var (x, y) = code.tail.reverse.toList.sliding(2, 2).zipWithIndex.map {
					case (List(y, x), i) => 
						(KEY.indexOf(x) * pow(60, i).longValue,
						 KEY.indexOf(y) * pow(60, i).longValue)
				} reduceLeft {
					(_, _) match {
						case ((x, y), (x2, y2)) => (x + x2, y + y2)
					}
				}
		//	case _ => throw new IllegalArgumentException(code)
		//}
		val fn = (a: Long) => if ((a % 2) != 0) -(a - 1) / 2 else a / 2
		x = fn(x)
		y = fn(y)
		
		val latY = (K * x * unit.x + y * unit.y) / 2
		val lonX = (latY - y * unit.y) / K
		
		val loc = xy2loc(lonX, latY)
		Zone(loc.lat, loc.lon, x, y, code)
	}
	
	def getZoneByXY(x: Long, y: Long, level: Int): Zone = {
		val size = calcHexSize(level)
		val unit = unitXY(size)
		val max = unitMax(unit)
		
		val latY = (K * x * unit.x + y * unit.y) / 2.0
		val lonX = (latY - y * unit.y) / K
		
		val loc = xy2loc(lonX, latY)
		calcZone(x, y, level, max, loc)
	}
}