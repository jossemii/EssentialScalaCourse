
/**
Music ::= JsNumber value:Double
          | JsString value:String
          | JsBoolean value:Boolean
          | JsNull
          | JsSequence
          | JsObject
          JsSequence ::= SeqCell head:Json tail:JsSequence
          | SeqEnd
          JsObject ::= ObjectCell key:String value:Json tail:JsObject
          | ObjectEnd
 */

sealed trait Note
case object Do extends Note
case object Re extends Note
case object Mi extends Note
case object Fa extends Note
case object Sol extends Note
case object La extends Note
case object Si extends Note


sealed trait Song {
  def print: String = this match
    case NoteSeq(note, tail) => s"${note.toString} ${tail.print}"
    case NoteEnd => ""
}
final case class NoteSeq(note: Note, tail: Song) extends Song
case object NoteEnd extends Song

def music_test = {
  assert("Do Re Mi Fa " == NoteSeq(Do, NoteSeq(Re, NoteSeq(Mi, NoteSeq(Fa, NoteEnd)))).print)
}
