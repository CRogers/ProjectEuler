package projecteuler

import scala.collection.mutable.ArrayBuffer
import java.io._
import java.nio._
import java.nio.channels._

object PrimeCalculator {
	
	def calculatePrimes(N: Int): Array[Int]= {
		
		val primes = new Array[Int](N)
		primes(0) = 2
		
		var i = 3
		var size = 1
		while(size < N) {
			
			val sqrt = math.sqrt(i).toInt + 1
			
			var j = 0
			while(j < primes.size && primes(j) < sqrt) {
				if(i % primes(j) == 0)
					j = Int.MaxValue-1
				j += 1
			}
			
			if(j != Int.MaxValue){ 
				primes(size) = i
				size += 1
				println(i)
				
				if(primes.size % 10000 == 0)
					println("%d: %d" format (size, i))
			}
			
			i += 2
		}
		
		primes		
	}
	
	def main(args: Array[String]) = {
		
		val outfile = "primes-1e3.bin"
		
		val primes = calculatePrimes(1e3.toInt)
		
		val buf = ByteBuffer.allocate(4 * primes.size)
		
		for(i <- 0 until primes.size) {
			buf.putInt(primes(i))
		}		
		
		buf.rewind()
		
		val outchannel = new FileOutputStream(outfile).getChannel()
		outchannel.write(buf)
		
		outchannel.close()
	}
	
	def load(name: String, N: Int): Array[Int] = {
		
		val buf = ByteBuffer.allocate(4*N)
		val inchannel = new FileInputStream(name).getChannel()
		inchannel.read(buf)
		buf.rewind()
		
		val arr = new Array[Int](N)
		
		for(i <- 0 until N) {
			arr(i) = buf.getInt()
		}
		
		arr
	}
	
}