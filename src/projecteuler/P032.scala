package projecteuler
import scala.collection.mutable.HashSet

object P032 {
	
	
	def main(args: Array[String]) = {
		
		solve()
		
	}
	

	def solve() = {
		
		// Since 10^a + 10^b = 10^(a+b) is a lower bound for the size of the result, we can infer that
		// (l-1)+(m-1)+1 <= 10-l-m    where l is len(first number) m is len(second number) and 10-l-m is
		// the number of digits left to use for the product. We can solve this to: l+m <= 5.5, which means
		// 1 <= m <= 4, 1 <= l <= 4  whilst l+m <= 5.5
		
		val digits = (1 to 9).toList
		
		val results = new HashSet[(Int, Int, Int)]
		
		
		for (i <- 1 to 5)			
			for(choice <- Euler.choose(digits, i)) {
				
				if(choice(0) != 0) {
				
					val a = Euler.makeInt(choice)
					
					val difference = digits diff choice
					
					for (j <- 1 to (5-i))			
						for(choice <- Euler.choose(difference, j)) {
							
							if(choice(0) != 0) {
							
								val b = Euler.makeInt(choice)
								val product = a * b
								val productDigits = Euler.getDigits(product)
								
								if(productDigits.size == digits.size-i-j && ((difference diff choice) diff productDigits).isEmpty)
									if(!results.contains((b,a,product)))
										results.add((a, b, product))
							}
						}
				}
			}						
		
		println(results.map(a=>a._3).sum)
	}
	
	
}