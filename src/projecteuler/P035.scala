package projecteuler
import scala.collection.mutable._

object P035 {

	def main(args: Array[String]) = {
		
		val primes = PrimeCalculator.load("primes-1e6.bin", 1000000)
		println(primes.size)
		val primesHash = new HashSet[Int]
		
		for(p <- primes)
			primesHash.add(p)
		
		val circularPrimes = new ArrayBuffer[Int]
		
		var i = 0
		while(primes(i) < 1e6.toInt) {
			
			var circular = true
				
			if(i % 10000 == 0)
				println("%d\t%d" format (primes(i), circularPrimes.size))
			
			val digits = Euler.getDigits(primes(i))
			for(permutation <- digits.permutations) {
				val num = Euler.makeInt(permutation)
				if(!primesHash.contains(num))
					circular = false
			}
			
			if(circular) {
				circularPrimes.append(primes(i))
				println(primes(i))
			}
		
			i += 1
		}
		
		println(circularPrimes.toList)	
	}
}