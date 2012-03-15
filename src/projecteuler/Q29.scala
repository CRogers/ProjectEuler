package projecteuler

import scala.collection.mutable._
import scala.math.BigInt

object Q29 {
	
	def solve(a: Int, b: Int) = {
		
		val set = new HashSet[BigInt]()
		
		for (i <- 2 to a) {
			var total = BigInt(i)
			for (j <- 2 to b) {
				total *= i
				if(!set.contains(total))
					set.add(total)
			}
		}
		
		set.size		
	}
	
	
	def main(args: Array[String]) = {
		println(solve(100,100))
	}
	
}