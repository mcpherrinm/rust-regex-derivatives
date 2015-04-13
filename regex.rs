#![allow(dead_code)]
use std::vec::Vec;
use std::collections::HashMap;

// Stores a set of characters as a collection of dense ranges
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(Hash)]
#[derive(Ord)]
#[derive(PartialOrd)]
struct RangeSet {
  //Todo: Sometimes it might be better to store the elements not in the set
  //alphabetminus: bool,
  assoc_list: Vec<(char, char)>,
}

impl RangeSet {
  fn empty(&self) -> bool {
    self.assoc_list.len() == 0
  }

  fn has(&self, c: char) -> bool {
    for &(start, end) in self.assoc_list.iter() {
      if c >= start && c <= end {
        return true;
      }
    }
    false
  }

  fn add(&mut self, c: char) {
    self.assoc_list.push((c, c));
  }

  fn add_range(&mut self, start: char, end: char) {
    self.assoc_list.push((start, end));
  }

  /// returns some member of the set
  fn some_member(&self) -> char {
    let (c, _) = self.assoc_list[0]; c
  }

  fn to_str(&self) -> String {
    let mut r = String::new();
    for &(s,e) in self.assoc_list.iter() {
      if s == e {
        r.push_str(&format!("{}", s)[..]);
      } else {
        r.push_str(&format!("{}-{}", s, e)[..]);
      }
    }
    r
  }
}

fn char(c: char) -> Regex {
  let l = vec!((c,c));
  CharSet(RangeSet{ assoc_list: l })
}

#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Clone)]
#[derive(Debug)]
#[derive(Hash)]
#[derive(Ord)]
#[derive(PartialOrd)]
enum Regex {
  Null, // Matches nothing
  Epsilon, // Matches the empty string
  AnyChar, // Matches any character
  CharSet(RangeSet), // set of chars, nonempty
  Alt(Vec<Regex>), // Alternation, aka "or"
  Seq(Vec<Regex>), // One after another
  Rep(Box<Regex>), // Kleene closure, repeated matching
}

use Regex::Null;
use Regex::Epsilon;
use Regex::AnyChar;
use Regex::CharSet;
use Regex::Alt;
use Regex::Seq;
use Regex::Rep;

impl Regex {
  /// Stringification
  fn to_str(&self) -> String {
    match *self {
      Null => "(∅)".to_string(),
      Epsilon => "ε".to_string(),
      AnyChar => ".".to_string(),
      CharSet(ref rs) => rs.to_str(),
      Alt(ref v) => {
        let mut s = "(".to_string();
        for r in v {
          s.push_str(&*r.to_str());
          s.push('|')
        }
        s.push(')');
        s
      },
      Seq(ref v) => {
        let mut s = "(".to_string();
        for r in v {
          s.push_str(&*r.to_str());
        }
        s.push(')');
        s
      },
      Rep(ref r1) => format!("({})*", r1.to_str()),
    }
  }
}

// Keep regexes in canonical form so it's easier to compare them
fn simplify(re: Regex) -> Regex {
  match re {
    Null => Null,
    Epsilon => Epsilon,
    AnyChar => AnyChar,
    CharSet(ref cset) if cset.empty() => Null,
    CharSet(c) => CharSet(c),
    Alt(v) => {
      let mut e: Vec<Regex> = v.into_iter().map(|r| simplify(r)).filter(|r| *r != Null).collect();
      let mut nested = vec![];
      let mut i: usize = 0;
      while i < e.len() {
          while i < e.len() {
              if let Alt(_) = e[i] {
                nested.push(e.swap_remove(i));
              } else {
                  break
              }
          }
          i += 1;
      }
      e.extend(nested);
      if e.len() == 0 { // The vector was all Nulls
          Null
      } else {
          e.sort();
          Alt(e)
      }
    }
    Seq(v) => {
      let e: Vec<Regex> = v.into_iter().map(|r| simplify(r)).filter(|r| *r != Epsilon).collect();
      if e.iter().any(|r| *r == Null) {
        Null
      } else if e.len() == 1 {
        e.into_iter().next().unwrap()
      } else if e.len() == 0 {
        Epsilon
      } else {
        Seq(e)
      }
    },
    Rep(r) => {
      match simplify(*r) {
        Null | Epsilon => Epsilon,
        x @ Rep(_) => x,
        r => Rep(Box::new(r)),
      }
    }
  }
}

/// Does this regex match the empty string?
fn regex_empty(re: &Regex) -> bool {
  match *re {
    Epsilon => true,
    CharSet(ref range) => range.empty(),
    Rep(_) => true,
    Seq(ref v) => v.iter().all(regex_empty),
    Alt(ref v) => v.iter().any(regex_empty),
    _ => false
  }
}

// Take the derivative of a regex wrt a character
fn derive(re: Regex, c: char) -> Regex{
  simplify(match re {
    Null => Null,
    Epsilon => Null,
    AnyChar => Epsilon,
    CharSet(ref cpat) if cpat.has(c) => Epsilon,
    CharSet(_) => Null,
    Alt(v) => Alt(v.into_iter().map(|r| derive(r, c)).collect()),
    Seq(v) => {
      let mut derivatives = vec![];
      for (index, entry) in v.iter().enumerate() {
        // Every entry before this one could have been empty.
        // Thus we consider the possiblity of the derivative of this entry in
        // the sequence wrt c and add it to the sequence's derivative.
        let mut derivative: std::vec::Vec<Regex> = vec![derive(entry.clone(), c)];
        derivative.extend(v.clone().into_iter().skip(index+1));
        derivatives.push(Seq(derivative));
        if !regex_empty(entry) {
          break;
        }
      }
      simplify(Alt(derivatives))
    },
    Rep(p) => simplify(Seq([derive((*p).clone(), c), Rep(p)].to_vec())),
  })
}

#[test]
fn derive_tests() {
  assert_eq!(derive(char('a'), 'a'), Epsilon);
  assert_eq!(derive(char('a'), 'b'), Null);
  //assert_eq!(derive(Seq(vec![char('a'), char('b')]), 'f'), char('b'));
  //assert_eq!(derive(Alt(Box::new(Seq(Box::new(Char('f')))), Box::new(Char('b')),
  //                      Box::new(Seq(Box::new(Char('f')), Box::new(Rep(Box::new(Char('z'))))))),
  //                  'f'),
  //           Alt(Box::new(Char('b')), Box::new(Rep(Box::new(Char('z'))))));
  assert_eq!(derive(Rep(Box::new(char('a'))), 'a'), Rep(Box::new(char('a'))));
}

// Use the derivatives directly to match against a string
fn do_match(mut re: Regex, data: &str) -> bool {
  for c in data.chars() {
    println!("derive re {:?} with respect to {:?}", re, c);
    re = derive(re, c);
    println!("Derived re {:?}", re);
  }
  regex_empty(&re)
}

#[test]
fn matcher_tests() {
  let bostar = Seq(vec![char('b'), Rep(Box::new(char('o')))]);
  assert!(do_match(bostar.clone(), "boooo"));
  assert!(!do_match(bostar, "bozo"));
}

// Parsing regexes
// We create a simple recursive descent parser for regexes.
// Matt Might provides the following EBNF grammar:
/*
   <regex> ::= <term> '|' <regex>
            |  <term>

   <term> ::= { <factor> }

   <factor> ::= <base> { '*' }

   <base> ::= <char>
           |  '\' <char>
           |  '(' <regex> ')'
*/

type CharIter<'a> = std::iter::Peekable<std::str::Chars<'a>>;

fn parse_regex(data: &mut CharIter) -> Regex {
  let term = parse_term(data);
  if data.peek() == Some(&'|') {
    data.next();
    let regex = parse_regex(data);
    Alt([term, regex].to_vec())
  } else {
    term
  }
}

fn parse_term(data: &mut CharIter) -> Regex {
  let mut factors = [parse_factor(data)].to_vec();
  loop {
    match data.peek() {
      None | Some(&')') | Some(&'|') => return Seq(factors),
      _ => (),
    }
    factors.push(parse_factor(data));
  }
}

fn parse_factor(data: &mut CharIter) -> Regex {
  let mut base = parse_base(data);
  while data.peek() == Some(&'*') {
    data.next();
    base = Rep(Box::new(base));
  };
  base
}

fn parse_base(data: &mut CharIter) -> Regex {
  match data.next().unwrap() {
    '(' => {
      let nested = parse_regex(data);
      data.next(); // consume the ')'
      nested
    }
    '[' => {
      let range = parse_range(data);
      data.next(); // consume the ']'
      range
    }
    '.' => AnyChar,
    '\\' => char(data.next().unwrap()),
    c => char(c),
  }
}

fn parse_range(data: &mut CharIter) -> Regex {
  let mut rangeset = RangeSet{ assoc_list: vec!() };
  loop {
    match *data.peek().unwrap() {
      ']' => return CharSet(rangeset),
      c => {
        data.next(); // consume the c
        if data.peek() == Some(&'-') {
          data.next(); // consume the '-'
          if data.peek() == Some(&']') {
            // A dash in trailing position matches a literal -
            rangeset.add(c);
            rangeset.add('-');
          } else {
            rangeset.add_range(c, data.next().unwrap());
          }
        } else {
          rangeset.add(c);
        }
      }
    }
  }
}

fn parse(expr: &str) -> Regex {
  simplify(parse_regex(&mut expr.chars().peekable()))
}

fn matches(expr: &str, data: &str) -> bool {
  let r = parse(expr);
  do_match(r, data)
}

#[test]
fn parsing_tests() {
  let abcstar = parse_regex(&mut "abc*".chars().peekable());
  println!("abc*: {}", abcstar.to_str());
  let abcstar = simplify(abcstar);
  println!("abc*: {}", abcstar.to_str());
  assert!(do_match(abcstar.clone(), "ab"));
  assert!(do_match(abcstar.clone(), "abc"));
  assert!(do_match(abcstar.clone(), "abccccc"));
  assert!(!do_match(abcstar, "abbbcc"));
  assert!(matches("((a)*)", "aaaaa"));
  assert!(matches("a(a|b)*", "aaaabaabbbaa"));
  assert!(matches("[abc]*", "abccabacbbaccba"));
  assert!(matches("a[b-c]d", "abd"));
  assert!(matches("A[a-z]Z", "AcZ"));
  assert!(matches("A[a-]Z", "A-Z"));
  assert!(matches("A[a-]Z", "AaZ"));
  assert!(matches("ab(c|d*)ef", "abef"));
  assert!(matches(".*", ""));
  assert!(matches(".*", "Happy little flowers"));
  assert!(!matches("abc", "abcd"));
}

// This approximate equivalence relation on regexes is very important for DFA
// generation.  This relies on the regexes being in a canonical form as
// produced by the `simplify` function
fn equiv(re1: &Regex, re2: &Regex) -> bool {
  match (re1, re2) {
    (r1, r2) if r1 == r2 => true,

/*
    (&Alt(ref r1, ref s1), &Alt(ref r2, ref s2)) =>
         equiv(r1, r2) && equiv(s1, s2)
      || equiv(r1, s2) && equiv(s1, r2),

    (&Seq(ref r1, ref s1), &Seq(ref r2, ref s2)) =>
      equiv(r1, r2) && equiv(s1, s2),
*/

    (&Rep(ref r1), &Rep(ref r2)) => equiv(&*r1, &*r2),
    _ => false,
  }
}

#[test]
fn equiv_test() {
  assert!(!equiv(&parse("a"), &parse("b")));
  assert!(equiv(&parse("a|b"), &parse("b|a")));
  assert!(equiv(&parse("a|b|c"), &parse("a|b|c")));
  assert!(equiv(&parse("a|b|c"), &parse("a|(b|c)")));
  assert!(equiv(&parse("(a|b)|c"), &parse("a|b|c")));
  assert!(equiv(&parse("(a|b)|c"), &parse("a|(b|c)")));
  assert!(equiv(&parse("(a|b)|c"), &parse("a|(b|c)")));
  assert!(equiv(&parse("(a|b)(c|d)"), &parse("(a|b)(c|d))")));
  assert!(equiv(&parse("(b|a)(c|d)"), &parse("(a|b)(c|d))")));
  assert!(equiv(&parse("(a|b)(d|c)"), &parse("(a|b)(c|d))")));
  assert!(equiv(&parse("(b|a)(d|c)"), &parse("(a|b)(c|d))")));
  assert!(equiv(&parse("(a|b)|(c|d)"), &parse("(a|b)|(c|d))")));
  assert!(equiv(&parse("(b|a)|(c|d)"), &parse("(a|b)|(c|d))")));
  assert!(equiv(&parse("(a|b)|(d|c)"), &parse("(a|b)|(c|d))")));
  assert!(equiv(&parse("(b|a)|(d|c)"), &parse("(a|b)|(c|d))")));
  assert!(equiv(&parse("(ab)c"), &parse("a(bc)")));
  assert!(equiv(&parse("abc"), &parse("a(bc)")));
  assert!(equiv(&parse("(ab)c"), &parse("abc")));
  assert!(equiv(&parse("((x|y)(w|z))(1|2)"), &parse("(x|y)((w|z)(1|2))")));
  assert!(equiv(&parse("(x|y)(w|z)(1|2)"), &parse("(x|y)((w|z)(1|2))")));
  assert!(equiv(&parse("((x|y)(w|z))(1|2)"), &parse("(x|y)(w|z)(1|2)")));
  assert!(equiv(&parse("a*"), &parse("a**")));
  assert!(equiv(&parse("a*"), &parse("a****")));
  assert!(equiv(&parse("((ab)c)*"), &parse("(a(bc))*")));
  assert!(equiv(&parse("((ab)c)***"), &parse("(a(bc))*")));
  assert!(!equiv(&parse("[abc]"), &parse("[q]")));
  assert!(equiv(&parse("."), &parse(".")));
  assert!(equiv(&parse("(a|b)**"), &parse("(b|a)**")));
}

fn intersect(rs1: &RangeSet, rs2: &RangeSet) -> RangeSet {
  rs1.clone()
}

fn pairwise_intersect(r:Vec<Vec<RangeSet>>) -> Vec<RangeSet> {
  let mut ret = vec!();
  // This is obviously incorrect
  for rs in r[0].iter() {
    for rs2 in r[1].iter() {
      ret.push(intersect(rs, rs2));
    }
  }
  ret
}

// For a given regular expression, we can partition the character set into sets
// that we need to take the derivative with respect to.
// Implicitly, there's also a rangeset of characters not in the vector
fn partition(re: &Regex) -> Vec<RangeSet> {
  match *re {
    Null | Epsilon | AnyChar => vec!(), // All chars are equivalent
    CharSet(ref rs) => vec!(rs.clone()),
    Alt(ref v) | Seq(ref v) =>
      pairwise_intersect(v.iter().map(partition).collect()),
    Rep(ref r) => partition(r),
  }
}

#[derive(Debug)]
struct Dfa {
  dfa: Vec<(Regex, HashMap<char, Regex>)>,
}

fn build(re: Regex) -> Dfa {
  // Each node is labelled with a Regex, and has a map of char -> DerivedRegex
  let mut builder = vec!();
  let mut worklist: Vec<Regex> = vec!(re);
  loop {
    if worklist.len() == 0 {
      return Dfa{dfa: builder };
    }
    let work = worklist.pop().unwrap();
    let parts = partition(&work);
    for set in parts.iter() {
      let c = set.some_member();
      let derivative = derive(work.clone(), c);
      {
        let pos = match builder.iter().position(|&(ref e,_)| equiv(&work, e)) {
          Some(i) => i,
          None => {
            builder.push((work.clone(),HashMap::new()));
            builder.len()-1
          }
        };
        if let Some(&mut (_, ref mut table)) = builder.get_mut(pos) {
          table.insert(c, derivative.clone());
        }
      }
      if !builder.iter().any(|&(ref e,_)| equiv(e, &derivative)) {
        worklist.push(derivative);
      }
    }
  }
}

fn make_dot(dfa: Dfa) {
  println!(r"digraph g {{");
  for &(ref node, ref v) in dfa.dfa.iter() {
    println!("\"{:?}\" [ label=\"{}\"];", node, node.to_str());
    for (c, to) in v.iter() {
      println!("  \"{:?}\" -> \"{:?}\" [ label=\"{}\"];", node, to, c);
    }
  println!("");

  }
  println!(r"}}");
}

#[cfg(not(test))]
fn main() {
  make_dot(build(parse("ab(c|(ec|d)*)ef(a|b)(c|d)(e|f)")));
}
