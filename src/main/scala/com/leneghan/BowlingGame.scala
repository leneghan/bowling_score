package com.leneghan

import scala._


class BowlingGame(val name:String) {
  // Total number of pins in a game
  val Max = 10

  private var scores:List[Int] = Nil

  /*
   * Calculates the players current total
   */
  def total():Int = perFrameTotal(scores).sum

  /*
   * Creates a list of the score for each frame the player has played including bonus points
   */
  def perFrameTotal():List[Int] = perFrameTotal(scores)

  /*
  * Calculates the total number of frames played so far
  */
  def totalFrames():Int = totalFrames(scores)

  /*
  * Formats the players frames using the standard Strike and Spare notation
  */
  def formattedFrames():List[String] = formattedFrames(scores)

  /*
  * Produces a string pairing each formatted frame notation with the points scored in that frame
  */
  def prettyPrint():String = formattedFrames.zip(perFrameTotal).foldLeft("")((x,y)=>x + y)

  /*
  * Add a standard frame to the players game
  */
  def addFrame(a:Int, b:Int = 0): Either[String,BowlingGame] =
    addFrame((a,b), scores) match {
      case Right(ls)    => scores = ls; Right(this)
      case Left(error)  => Left(error)
    }

  /*
  * Add a final frame to the players game
  */
  def addFinalFrame(a:Int, b:Int, c:Int = 0): Either[String,BowlingGame]  =
    addFinalFrame((a,b,c), scores) match {
      case Right(ls)    => scores = ls; Right(this)
      case Left(error)  => Left(error)
    }

  // Marker that signifies the end of a game
  val End = -1

  def perFrameTotal(scores: List[Int]):List[Int] =
    scores match {
      case Nil                        => Nil
      case a :: b :: c :: End :: Nil  => (a + b + c) :: Nil
      case Max :: a :: b :: ls        => (Max + a + b) :: perFrameTotal(a :: b :: ls)
      case a :: b :: c :: ls
        if (a + b == Max)             => (a + b + c) :: perFrameTotal(c :: ls)
      case a :: b :: ls               => (a + b) :: perFrameTotal(ls)
      case _                          => Nil
    }

  def totalFrames(scores: List[Int]):Int =
    scores match {
      case Nil                        => 0
      case _ :: _ :: _ :: End :: Nil  => 1
      case Max :: ls                  => 1 + totalFrames(ls)
      case a :: b :: ls               => 1 + totalFrames(ls)
    }

  def addFrame(score: Tuple2[Int, Int], ls: List[Int]): Either[String,List[Int]] =
    score match {
      case (Max, 0)   => Right(ls ::: List(Max))
      case (a, b)
        if(a+b <= 10) => Right(ls ::: List(a, b))
      case invalid    => Left("Not a valid score %s".format(invalid.toString()))
    }

  def addFinalFrame(score: Tuple3[Int, Int, Int], ls: List[Int]): Either[String,List[Int]] =
    score match {
      case (Max, a, b)  => Right(ls ::: List(Max, a, b, End))
      case (a, b, c)
        if(a+b == Max)  => Right(ls ::: List(a, b, c, End))
      case (a, b, 0)
        if(a+b < Max)   => Right(ls ::: List(a, b, 0, End))
      case invalid      => Left("Not a valid score %s".format(invalid.toString()))
    }

  def formattedFrames(scores: List[Int]):List[String] =
    scores match {
      case Nil                             =>  Nil
      case Max :: Max :: Max :: End :: Nil => "[X,X,X]" :: Nil
      case Max :: Max :: a :: End :: Nil   => "[X,X,%d]".format(a) :: Nil
      case Max :: a :: b :: End :: Nil
        if(a+b == 10)                      => "[X,%d,/]".format(a) :: Nil
      case Max :: a :: b :: End :: Nil     => "[X,%d,%d]".format(a,b) :: Nil

      case a :: b :: Max :: End  :: Nil
        if(a+b == 10)                      => "[%d,/,X]".format(a) :: Nil
      case a :: b :: c :: End  :: Nil
        if(a+b == 10)                      => "[%d,/,%d]".format(a,b) :: Nil
      case a :: b :: 0 :: End  :: Nil      => "[%d,%d]".format(a,b) :: Nil

      case Max :: ls                       => "[X]" :: formattedFrames(ls)
      case a :: b :: ls if (a+b == Max)    => "[%d,/]".format(a) :: formattedFrames(ls)
      case a :: b :: ls                    => "[%d,%d]".format(a, b) :: formattedFrames(ls)
    }
}
