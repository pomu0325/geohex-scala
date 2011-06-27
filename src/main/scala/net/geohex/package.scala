package net

/*
 * This library "geohex-scala" is a Scala porting of "GeoHex"
 *  originally licensed by @sa2da (http://geogames.net)
 *  under Creative Commons BY-SA 2.1 Japan License.
 *
 * This library partly derives from "geohex4j" (https://github.com/chsh/geohex4j),
 *  licensed by CHIKURA Shinsaku
 *  under Creative Commons BY-SA 2.1 Japan License.
 *
 *  geohex-scala is licensed by Pomu TAKEUCHI
 *   under Creative Commons BY-SA 2.1 Japan License.
 */

package object geohex {
	val KEY = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	val BASE = 20037508.34
	val DEG = math.Pi * (30.0 / 180.0)
	val K = math.tan(DEG)

	implicit def pair2loc(p: Pair[Lat, Lon]): Loc = Loc(p._1, p._2)
	implicit def pair2xy(p: Pair[Double, Double]): XY = XY(p._1, p._2)
	implicit def lat2double(lat: Lat): Double = lat.lat
	implicit def lon2double(lon: Lon): Double = lon.lon
	
	private[geohex] def calcHexSize(level: Int): Double = BASE / math.pow(3.0, level + 1)
	
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
		val lon = (x / BASE) * 180
		val lat = 180 / math.Pi *
			(2 * math.atan(math.exp((y / BASE) * 180 * math.Pi / 180)) - math.Pi / 2)
		Loc(Lat(lat), Lon(lon))
	}
}