import week_12.funsets.FunSets._

val s = (x: Int) => List(1,3,4,5,7,1000).contains(x)
val smap = map(s, (x: Int) => x - 1)
val rng = (-10 to 1010).filter(smap).toList
