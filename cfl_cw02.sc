//regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case object ALL extends Rexp
// case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 
case class CFUN(f: Char => Boolean) extends Rexp 
// case class RANGE(r: List[CHAR]) extends Rexp
case class PLUS(r: Rexp) extends Rexp 
case class OPTIONAL(r: Rexp) extends Rexp
// case class UPTO(r: Rexp, m: Int) extends Rexp
// case class FROM(r: Rexp, n: Int) extends Rexp
// case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
// case class NOT(r: Rexp) extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp  

// values  
abstract class Val
case object Empty extends Val
// case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Pls(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
case class Cf(f: Char => Boolean) extends Val
case class Opt(v: Val) extends Val
case class Ntime(v: Val, i: Int) extends Val

// some convenience for typing in regular expressions
def RANGE(s: String) =  CFUN(s.toSet)
def CHAR(s: Char) = CFUN(Set(s))
// def PLUS(r: Rexp) = SEQ(r, STAR(r))
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp = 
  charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}

// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case ALL => false
  // case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case PLUS(r) => nullable(r)
  case OPTIONAL(_) => true
  // case UPTO(_, _) => true
  // case FROM(r, i) => if (i == 0) true else nullable(r)
  // case BETWEEN(r, n, m) => if (n == 0 || m == 0) true else nullable(r)
  // case NOT(r) => !nullable(r)
  case CFUN(f) => false
  case RECD(_, r1) => nullable(r1)
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case ALL => ONE
  // case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, i) => 
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case PLUS(r1) => SEQ(der(c, r1), STAR(r1))
  case OPTIONAL(r1) => der(c, r1)
  // case UPTO(r1, m) => if (m == 0) ZERO else SEQ(der(c, r1), UPTO(r1,m-1))
  // case FROM(r1, n) => if (n == 0) SEQ(der(c, r1), STAR(r1)) else SEQ(der(c, r1), FROM(r1,n-1))
  // case BETWEEN(r1, n, m) => if (n == 0) der(c,UPTO(r1,m)) else SEQ(der(c, r1),BETWEEN(r1, n-1, m-1))
  // case NOT(r1) => NOT(der(c, r1))
  case CFUN(f) => if (f(c)) ONE else ZERO
  case RECD(_, r1) => der(c, r1)
}
def fomat(c: String): String = {
    val n = c.length
    c.substring(4, n-1)
}
// extracts a string from a value
def flatten(v: Val) : String = v match {
  case Empty => ""
  // case Chr(c) => c.toString
  case Cf(f) => fomat(f.toString)
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(_, v) => flatten(v)
  case Ntime(v1, i) => flatten(v1)*i
  case Pls(vs) => vs.map(flatten).mkString
  case Opt(v1) => flatten(v1)

}

// extracts an environment from a value;
// used for tokenising a string
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  // case Chr(c) => Nil
  case Cf(f) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flatten(v))::env(v)
  case Ntime(v1, i) => env(v1)
  case Pls(vs) => vs.flatMap(env)
  case Opt(v1) => env(v1)
}


// The injection and mkeps part of the lexer
//===========================================

def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) => 
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case RECD(x, r) => Rec(x, mkeps(r))
  case PLUS(r) => Sequ(mkeps(r), Stars(Nil))
  case OPTIONAL(r) => if (nullable(r)) Opt(mkeps(r)) else Empty
  case NTIMES(r, i) => if (i == 0) Empty else Ntime(mkeps(r), i-1)
}

def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (PLUS(r1), Sequ(v1, Stars(vs))) => Pls(inj(r1, c, v1)::vs)
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  // case (CHAR(d), Empty) => Chr(c) 
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
  case (OPTIONAL(r1), Opt(v1)) => Opt(inj(r1, c, v1))
  case (OPTIONAL(r1), Empty) => Opt(inj(r1, c, v))
  case (NTIMES(r1, i), Sequ(v1, Empty)) => Ntime(inj(r1, c, v1), i)
  case (NTIMES(r1, i), Sequ(v1, Ntime(v2, i2))) => Ntime(inj(r1, c, v1), i)
  case (CFUN(f), Empty) => Cf(Set(c))
}

// some "rectification" functions for simplification
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(v), f2(Empty))

def F_ERROR(v: Val): Val = throw new Exception("error")

// simplification
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID)
}

// lexing functions including simplification
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") } 
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def lexing_simp(r: Rexp, s: String) = 
  env(lex_simp(r, s.toList))


// The Lexing Rules for the WHILE Language

// def PLUS(r: Rexp) = r ~ r.%

// def Range(s : List[Char]) : Rexp = s match {
//   case Nil => ZERO
//   case c::Nil => CHAR(c)
//   case c::s => ALT(CHAR(c), Range(s))
// }

val LET = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
val SYM = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.<>=;,\\:")
val DIGIT = RANGE("0123456789")
val ID = LET ~ ("_" | LET | DIGIT).% 
// val NUM = PLUS(DIGIT)
val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "for" | "to" | "true" | "false" 
val SEMI: Rexp = ";"
val OP: Rexp = ":=" | "-" | "+" | "*" | "!=" | "<" | ">" | "%" | "==" | "!=" | "<=" | ">=" | "&&" | "||" | "/"//"=" not allowed
val WHITESPACE = PLUS(" " | "\n" | "\t" | "\r")
// val RPAREN: Rexp = "{"
// val LPAREN: Rexp = "}"
// val STRING: Rexp = "\"" ~ SYM.% ~ "\""
val STRINGS: Rexp = "\"" ~ (SYM | DIGIT | WHITESPACE).% ~ "\""
val PARA: Rexp = "{" | "}" | "(" | ")"//may change

val NUM: Rexp =  DIGIT | (RANGE("123456789") ~ DIGIT.% )
val COMMENT: Rexp = "//" ~ (DIGIT | SYM | " ").% ~ ("\n" | "\t" | "\r")

val WHILE_REGS = (("k" $ KEYWORD) | 
                  ("i" $ ID) | 
                  ("o" $ OP) | 
                  ("n" $ NUM) | 
                  ("s" $ SEMI) | 
                  ("str" $ STRINGS) |
                  ("p" $ PARA) | 
                  ("c" $ COMMENT) | 
                  ("w" $ WHITESPACE)).%

//test a{3} and (a+1){3}
@arg(doc = "Question 2")
@main
def q2() = {

  val a = NTIMES(CHAR('a'), 3)
  println(s"test: $a")
  println(lexing_simp(a, "aaa"))

  val b = NTIMES(OPTIONAL(CHAR('a')),3)
  println(s"test: $b")
  println(lexing_simp(b, "aa"))
}


// Two Simple While Tests
//========================

@arg(doc = "small tests")
@main
def small() = {

  val prog0 = """read n;"""
  println(s"test: $prog0")
  println(lexing_simp(WHILE_REGS, prog0))

  val prog1 = """read  n; write n"""  
  println(s"test: $prog1")
  println(lexing_simp(WHILE_REGS, prog1))
}

// Bigger Tests
//==============

// escapes strings and prints them out as "", "\n" and so on
def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: List[(String, String)]) =
  tks.map{ case (s1, s2) => (s1, esc(s2))}

val pq2 = "read n;"
val prog2 = """
write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
  temp := minus2;
  minus2 := minus1 + minus2;
  minus1 := temp;
  n := n - 1
};
write "Result";
write minus2
"""

@arg(doc = "Fibonacci test")
@main
def fib() = {
  println("lexing fib program")
  println(escape(lexing_simp(WHILE_REGS, prog2)).mkString("\n"))
}


val prog3 = """
start := 1000;
x := start;
y := start;
z := start;
while 0 < x do {
 while 0 < y do {
  while 0 < z do {
    z := z - 1
  };
  z := start;
  y := y - 1
 };     
 y := start;
 x := x - 1
}
"""

@arg(doc = "Loops test")
@main
def loops() = {
  println("lexing Loops")
  println(escape(lexing_simp(WHILE_REGS, prog3)).mkString("\n"))
}

@arg(doc = "Email Test")
@main
def email() = {
  val lower = "abcdefghijklmnopqrstuvwxyz"

  val NAME = RECD("name", PLUS(RANGE(lower ++ "_.-")))
  val DOMAIN = RECD("domain", PLUS(RANGE(lower ++ "-")))
  val RE = RANGE(lower ++ ".")
  val TOPLEVEL = RECD("top", (RE ~ RE) |
                             (RE ~ RE ~ RE) | 
                             (RE ~ RE ~ RE ~ RE) | 
                             (RE ~ RE ~ RE ~ RE ~ RE) |
                             (RE ~ RE ~ RE ~ RE ~ RE ~ RE))

  val EMAIL = NAME ~ "@" ~ DOMAIN ~ "." ~ TOPLEVEL

  println(lexing_simp(EMAIL, "christian.urban@kcl.ac.uk"))
}

val prog4 = """
// Find all factors of a given input number


write "Input n please";
read n;
write "The factors of n are";
f := 2;
while (f < n / 2 + 1) do {
  if ((n / f) * f == n) then  { write(f) } else { skip };
  f := f + 1
}
"""
val prog5 ="""
// Collatz series
//
// needs writing of strings and numbers; comments

bnd := 1;
while bnd < 101 do {
  write bnd;
  write ": ";
  n := bnd;
  cnt := 0;

  while n > 1 do {
    write n;
    write ",";
    
    if n % 2 == 0 
    then n := n / 2 
    else n := 3 * n+1;

    cnt := cnt + 1
  };

  write " => ";
  write cnt;
  write "\n";
  bnd := bnd + 1
}
"""

@arg(doc = "factors test")
@main
def factors() = {
  println("lexing factors")
  println(escape(lexing_simp(WHILE_REGS, prog4)).mkString("\n"))
}

@arg(doc = "All tests.")
@main
def all() = { small(); fib() ; loops() ; email() } 


// runs with amm2 and amm3



