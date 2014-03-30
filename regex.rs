// Stores a set of characters as a collection of dense ranges
#[deriving(Eq)]
#[deriving(Clone)]
#[deriving(Show)]
struct RangeSet {
  assoc_list: std::vec::Vec<(char, char)>,
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

  fn add(&mut self, c: char) {
    self.assoc_list.push((c, c));
  }

  fn add_range(&mut self, start: char, end: char) {
    self.assoc_list.push((start, end));
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
    Epsilon => Epsilon,
    AnyChar => AnyChar,
    CharSet(ref cset) if cset.empty() => Null,
    CharSet(c) => CharSet(c),
    Alt(~r, ~s) => {
      match (simplify(r), simplify(s)) {
        (Null, r) | (r, Null) => r,
        (r, s) => Alt(~r, ~s)
      }
    },
    Seq(~r, ~s) => {
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

fn parse_regex(data: &mut CharIter) -> Regex {
  let term = parse_term(data);
  if data.peek() == Some(&'|') {
    data.next();
    let regex = parse_regex(data);
    Alt(~term, ~regex)
  } else {
    term
  }
}

fn parse_term(data: &mut CharIter) -> Regex {
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

fn parse_factor(data: &mut CharIter) -> Regex {
  let mut base = parse_base(data);
  while !data.is_empty() && data.peek() == Some(&'*') {
    data.next();
    base = Rep(~base);
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
    '\\' => Char(data.next().unwrap()),
    c => Char(c),
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
  let abcstar = parse("abc*");
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
// generation.  
fn equiv(re1: &Regex, re2: &Regex) -> bool {
  match (re1, re2) {
    (r1, r2) if r1 == r2 => true,

    (&CharSet(ref rs1), &CharSet(ref rs2)) => rs1 == rs2,

    (&Alt(~Alt(~ref r1, ~ref s1), ~ref t1),
     &Alt(~ref r2, ~Alt(~ref s2, ~ref t2))) 
      if equiv(r1, r2) && equiv(s1, s2) && equiv(t1, t2) => true,

    (&Alt(~ref r1, ~ref s1), &Alt(~ref s2, ~ref r2)) 
      if equiv(r1, r2) && equiv(s1, s2) => true,

    (&Alt(~ref r1, ~ref s1), &Alt(~ref r2, ~ref s2)) =>
      equiv(r1, r2) && equiv(s1, s2),

    (&Seq(~Seq(~ref r1, ~ref s1), ~ref t1),
     &Seq(~ref r2, ~Seq(~ref s2, ~ref t2)))
      if (equiv(r1, r2) && equiv(s1, s2) && equiv(t1, t2)) => true,

    (&Seq(~ref r1, ~ref s1), &Seq(~ref r2, ~ref s2)) =>
      equiv(r1, r2) && equiv(s1, s2),

    (&Rep(~ref r1), &Rep(~ref r2)) => equiv(r1, r2),

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


