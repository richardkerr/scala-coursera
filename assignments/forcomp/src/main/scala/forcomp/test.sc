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

def wordOccurrencesMap(w: Word): Map[Char,Int] = {
  w.toLowerCase groupBy(char => char) mapValues(_.length)
}

def wordOccurrences(w: Word): Occurrences = {
  wordOccurrencesMap(w).toList.sorted
}

val s: Sentence = List[Word](("asdfdsaf"),("assavvv"))
/** Converts a sentence into its character occurrence list. */

dictionary groupBy wordOccurrences