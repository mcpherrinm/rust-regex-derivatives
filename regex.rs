// Stores a set of characters as a collection of dense ranges
#[deriving(Eq)]
#[deriving(Clone)]
#[deriving(Show)]
struct RangeSet {
  assoc_list: std::vec_ng::Vec<(char, char)>,
}

impl RangeSet {
  fn empty(&self) -> bool {
    self.assoc_list.len() == 0
  }

  fn has(&self, c: char) -> bool {
    for pair in self.assoc_list.iter() {
      let &(start, end) = pair;
      if c >= start && c <= end {
        return true;
      }
    }
    false
  }
}

fn Char(c: char) -> Regex {
  let l = vec!((c,c));
  CharSet(RangeSet{ assoc_list: l })
}

#[deriving(Eq)]
#[deriving(Clone)]
#[deriving(Show)]
enum Regex {
  Null, // Matches nothing
  Epsilon, // Matches the empty string
  AnyChar, // Matches any character
  CharSet(RangeSet), // set of chars, nonempty
  Alt(~Regex, ~Regex), // Alternation, aka "or"
  Seq(~Regex, ~Regex), // One after another
  Rep(~Regex), // Kleene closure, repeated matching
}

// Keep regexes in canonical form so it's easier to compare them
fn simplify(re: Regex) -> Regex {
  match re {
    Null => Null,
    Epsilon                           => Epsilon,
    AnyChar                           => AnyChar,
    CharSet(ref cset) if cset.empty() => Null,
    CharSet(c)                        => CharSet(c),
    Alt(~r, ~s)                     => {
      match (simplify(r), simplify(s)) {
        (Null, r) | (r, Null) => r,
        (r, s) => Alt(~r, ~s)
      }
    },
    Seq(~r, ~s)                     => {
      match (simplify(r), simplify(s)) {
        (Null, _)    | (_, Null)    => Null,
        (Epsilon, r) | (r, Epsilon) => r,
        (r,s) => Seq(~r,~s),
      }
    },
    Rep(~r) => {
      match simplify(r) {
        Null | Epsilon => Epsilon,
        Rep(~r) | r => Rep(~r),
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
    Seq(~ref p1, ~ref p2) => regex_empty(p1) && regex_empty(p2),
    Alt(~ref p1, ~ref p2) => regex_empty(p1) || regex_empty(p2),
    _ => false
  }
}

// Take the derivative of a regex wrt a character
fn derive(re: Regex, c: char) -> Regex{
  let r = derive2(re, c);
  println!("derived {}", r);
  r
}
fn derive2(re: Regex, c: char) -> Regex{
  simplify(match re {
    Null => Null,
    Epsilon => Null,
    AnyChar => Epsilon,
    CharSet(ref cpat) if cpat.has(c) => Epsilon,
    CharSet(_) => Null,
    Alt(~p1, ~p2) => Alt(~derive(p1, c), ~derive(p2, c)),
    Seq(~p1, ~p2) => {
      let p1div = Seq(~derive(p1.clone(), c), ~p2.clone());
      if regex_empty(&p1) {
        Alt(~derive(p2, c), ~p1div)
      } else {
        p1div
      }
    },
    Rep(~p) => simplify(Seq(~derive(p.clone(), c), ~Rep(~p))),
  })
}

#[test]
fn derive_tests() {
  assert_eq!(derive(Char('a'), 'a'), Epsilon);
  assert_eq!(derive(Char('a'), 'b'), Null);
  assert_eq!(derive(Seq(~Char('f'), ~Char('b')), 'f'), Char('b'));
  assert_eq!(derive(Alt(~Seq(~Char('f'), ~Char('b')),
                        ~Seq(~Char('f'), ~Rep(~Char('z')))),
                    'f'),
             Alt(~Char('b'), ~Rep(~Char('z'))));
  assert_eq!(derive(Rep(~Char('a')), 'a'), Rep(~Char('a')));
}

// Use the derivatives directly to match against a string
fn do_match(mut re: Regex, data: &str) -> bool {
  for c in data.chars() {
    println!("Deriving re {} with respect to {}", re, c);
    re = derive(re, c);
    println!("Derived re {}", re);
  }
  regex_empty(&re)
}

#[test]
fn matcher_tests() {
  assert!(do_match(Seq(~Char('b'), ~Rep(~Char('o'))), "boooo"));
  assert!(!do_match(Seq(~Char('b'), ~Rep(~Char('o'))), "bozo"));

  assert!(do_match(Seq(~Char('f'), ~Rep(~Alt(~Char('b'), ~Char('z')))),
                  "fbzbb"));
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

type CharIter<'a> = std::iter::Peekable<char,std::str::Chars<'a>>;

fn parse_regex<'a>(data: &mut CharIter<'a>) -> Regex {
  let term = parse_term(data);
  if data.peek() == Some(&'|') {
    data.next();
    let regex = parse_regex(data);
    Alt(~term, ~regex)
  } else {
    term
  }
}

fn parse_term<'a>(data: &mut CharIter<'a>) -> Regex {
  let mut factor = parse_factor(data);
  loop {
    match data.peek() {
      None => return factor,
      Some(&')') => return factor,
      Some(&'|') => return factor,
      _ => (),
    }
    let nextfactor = parse_factor(data);
    factor = Seq(~factor, ~nextfactor);
  }
}

fn parse_factor<'a>(data: &mut CharIter<'a>) -> Regex {
  let mut base = parse_base(data);
  while !data.is_empty() && data.peek() == Some(&'*') {
    data.next();
    base = Rep(~base);
  };
  base
}

fn parse_base<'a>(data: &mut CharIter<'a>) -> Regex {
  match data.next().unwrap() {
    '(' => {
      let nested = parse_regex(data);
      data.next(); // consume the ')'
      nested
    }
    '.' => AnyChar,
    '\\' => Char(data.next().unwrap()),
    c => Char(c),
  }
}

fn matches(expr: &str, data: &str) -> bool {
  let r = parse_regex(&mut expr.chars().peekable());
  do_match(r, data)
}

#[test]
fn parsing_tests() {
  let abcstar = parse_regex(&mut "abc*".chars().peekable());
  assert!(do_match(abcstar.clone(), "ab"));
  assert!(do_match(abcstar.clone(), "abc"));
  assert!(do_match(abcstar.clone(), "abccccc"));
  assert!(!do_match(abcstar, "abbbcc"));
  assert!(matches("((a)*)", "aaaaa"));
  assert!(matches("(a|b)*", "aaaabaabbbaa"));
}
