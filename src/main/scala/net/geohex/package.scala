package net

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

package object geohex {
	val KEY = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	val BASE = 20037508.34
	val DEG = math.Pi * (30.0 / 180.0)
	val K = math.tan(DEG)

	implicit def pair2loc(p: Pair[Lat, Lon]): Loc = Loc(p._1, p._2)
	implicit def pair2xy(p: Pair[Double, Double]): XY = XY(p._1, p._2)
	implicit def lat2double(lat: Lat): Double = lat.lat
	implicit def lon2double(lon: Lon): Double = lon.lon
	
	private[geohex] def calcHexSize(level: Int): Double = BASE / math.pow(2.0, level) / 3.0
	
	private[geohex] implicit def loc2xy(loc: Loc): XY = {
		import loc._
		val x = lon * BASE / 180
		val a = (90.0 + lat) * math.Pi / 360.0
		
		var y = math.log(math.tan((90.0 + lat) * math.Pi / 360.0)) / (math.Pi / 180.0)
		y = y * (BASE / 180.0)
		XY(x, y)
	}
	
	private[geohex] implicit def xy2loc(xy: XY): Loc = {
		import xy._
		val lon = (x / BASE) * 180 match {	// lon must be between -180 ~ 180
			case d if d > 180.0 => 180.0
			case d if d < -180.0 => -180.0
			case d => d
		}
		val lat = 180 / math.Pi *
			(2 * math.atan(math.exp((y / BASE) * 180 * math.Pi / 180)) - math.Pi / 2)
		Loc(Lat(lat), Lon(lon))
	}	
}