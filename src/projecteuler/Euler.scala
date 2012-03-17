package projecteuler

import scala.collection.mutable.ArrayBuffer

object Euler {

	def getDigits(num: Int): List[Int] = {
		
		val list = new ArrayBuffer[Int]		
		var n = num
		var a = 10
		
		while(n > 0) {
			list.append(n % 10)
			n /= 10
		}
		
		list.toList
		
	}
	
	def choose[T](items: List[T], n: Int): Seq[List[T]] = {
		
		if(n == 1)
			return items.map(i => List(i))
		
		(0 until items.size).map(i => 
			choose(remove(items, i), n-1).iterator.map(choice => items(i) :: choice)
		).reduceLeft(_ ++ _).toSeq
		
	}
	
	def remove[T](list: List[T], i: Int): List[T] = {
		val (a, b) =  list.splitAt(i)
		a ::: b.tail
	}
	
	def makeInt(list: List[Int]): Int = list.foldLeft(0)((acc, i) => acc*10 + i)
	
}