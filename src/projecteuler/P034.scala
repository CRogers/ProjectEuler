package projecteuler

object P034 {
	
	def main(args: Array[String]) = {
		
		val facts = new Array[Int](10)
		
		for(i <- 0 to 9)
				facts(i) = Euler.factorial(i)
			
			var total = 0
				
			for(i <- 3 to 1e6.toInt) {
				
				val sum = Euler.getDigits(i).map(facts(_)).sum
			
			if(sum == i) {
				total += sum
				println(total)
			}
		}
		
	}
	
}