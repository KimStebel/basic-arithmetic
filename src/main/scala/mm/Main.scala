package mm

import spire.math.{Rational, gcd}
import spire.random.Cmwc5

object Main {

  def main(args: Array[String]) {
    def loop {
      val x = randomRational
      val y = randomRational
      val op = randomOp
      val res = op._2(x,y)
      println(s"$x ${op._1} ($y) = ?")
      Parser.parseInput(Console.readLine.trim) match {
        case Left(error) => {
          println(error)
          loop
        }
        case Right((input, warnings)) => {
          for (w <- warnings.reverse) {
            println(w)
          }
          if (input == res) {
			      println("correct!")
			    } else {
			      println("not quite, it's " + res)
			    }
      	}
      }
      loop
    }
    loop
  }
  object Parser {
    val optSpace = "\\s*"
    val space = "\\s+"
    val sign = s"(\\-?)$optSpace"
    val num = "(\\d+)"
    val WholeNum = s"$sign$num".r
    val fractionAbs = s"$num$optSpace/$optSpace$num"
    val Fraction = s"$sign$fractionAbs".r
    val Mixed = s"$sign$num$space$fractionAbs".r
    object AsInt {
      def unapply(s:String) = try {
        Some(s.toInt)
      } catch {
        case _:NumberFormatException => None
      }
    }
    object Sign {
      def unapply(s:String) = s match {
        case "-" => Some(-1)
        case "" => Some(1)
        case _ => None
      }
    }
    def parseInput(line:String):Either[String, (Rational, List[String])] = {
	    var warnings:List[String] = Nil
	    val (sign,numer,denom) = line match {
	      case WholeNum(Sign(sign), AsInt(num)) => (sign, num, 1)
	      case Fraction(Sign(sign), AsInt(numer), AsInt(denom)) => {
	        if (denom == 1) {
			      warnings ::= "denominator must not be 1!"
			    }
			    if (numer >= denom) {
	          warnings ::= "numerator must be smaller than denominator!"
	        }
			    (sign, numer, denom)
	      }
	      case Mixed(Sign(sign), AsInt(whole), AsInt(rest), AsInt(denom)) => {
	        if (whole == 0) {
	          warnings ::= "whole number part must not be 0!"
	        }
	        if (rest >= denom) {
	          warnings ::= "rest must be smaller than denominator!"
	        }
	        (sign, whole * denom + rest, denom)
	      }
	      case _ => return Left("parse error")
	    }
	    if (denom == 0) {
	      return Left("denominator can't be 0!")
	    }
	    if (gcd(numer,denom) != 1) {
        warnings ::= "numerator and denominator must not have common factors!"
      }
	    Right(Rational(sign * numer, denom) -> warnings)
    }
  }    
  val ops = Seq("+" -> ((x:Rational,y:Rational) => x + y),
                "-" -> ((x:Rational,y:Rational) => x - y),
                "*" -> ((x:Rational,y:Rational) => x * y),
                "/" -> ((x:Rational,y:Rational) => x / y))
  def randomOp = ops(r.nextInt.abs % ops.size)
  val r = Cmwc5()
  val intLimit = 10
  def randomRational = {
     //denom is a random number in ]-intLimt,intLimit[ but not 0
    val denom = ((randomInt.abs % (intLimit - 1)) + 1) * //absolute value 
                (if (randomBool) 1 else -1) //sign
    Rational(randomInt, denom)
  }
  def randomBool = r.next[Boolean]
  def randomInt = r.nextInt % intLimit
}
