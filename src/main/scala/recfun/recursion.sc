import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

val r=3

def createNextRow(row:Vector[Int]):Vector[Int]={
  var previous = 0
  val mainPart = row.map(current => {
    val sum = current + previous
    previous = current
    sum
  })
  mainPart :+ previous
}

@tailrec
def getRowRecursively(row:Vector[Int]):Vector[Int] = {
  if (row.size == r+1)
    row
  else
    getRowRecursively(createNextRow(row))
}



getRowRecursively(Vector(1))

