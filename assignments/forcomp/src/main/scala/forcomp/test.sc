import forcomp._

import scala.collection.immutable.HashMap

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]

/** The dictionary is simply a sequence of words.
  *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
  */
val dictionary: List[Word] = loadDictionary

/** Converts the word into its character occurrence list.
  *
  *  Note: the uppercase and lowercase version of the character are treated as the
  *  same character, and are represented as a lowercase character in the occurrence list.
  */
def merge[A >: Char,B >:Int](f: (B,B) => B)(m: Map[A,B], n: Map[A,B]): Map[A,B] = {
  m ++ n.map{ case (k,v) => k -> (f(v,m.getOrElse(k,0))) }
}

def wordOccurrences(w: Word) = {
  //wordRecurse(w.toLowerCase.toList, Map[Char,Int]()).toList.sorted
  val vals: List[Map[Char,Int]] = w.toList.map(c => HashMap(c->1))
  vals.foldLeft(Map[Char,Int]())(merge(_+_))
}