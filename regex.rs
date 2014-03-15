#[deriving(Clone)]
#[deriving(Eq)]
#[deriving(Show)]
enum Regex {
  Null,
  Blank,
  AnyChar,
  Char(char),
  Alt(~Regex, ~Regex),
  Seq(~Regex, ~Regex),
  Rep(~Regex),
  Not(~Regex),
}

// Keep regexes in canonical form so it's easier to compare them
fn simplify(re: Regex) -> Regex {
  match re {
    Null                              => Null,
    Blank                             => Blank,
    AnyChar                           => AnyChar,
    Char(c)                           => Char(c),
    Alt(~Null, ~r)  | Alt(~r, ~Null)  => simplify(r),
    Alt(~r1, ~r2)                     => Alt(~simplify(r1), ~simplify(r2)),
    Seq(~Null, _)   | Seq(_, ~Null)   => Null,
    Seq(~Blank, ~r) | Seq(~r, ~Blank) => simplify(r),
    Seq(~r1, ~r2)                     => Seq(~simplify(r1), ~simplify(r2)),
    Rep(~Null)      | Rep(~Blank)     => Blank,
    Rep(~r)         | Rep(~Rep(~r))   => Rep(~simplify(r)),
    Not(~Not(~r))                     => simplify(r),
    Not(~Null)                        => Rep(~AnyChar),
    Not(~r)                           => Not(~simplify(r)),
  }
}

/// Does this regex match the empty string?
fn regex_empty(re: &Regex) -> bool {
  match *re {
    Blank => true,
    Rep(_) => true,
    Seq(~ref p1, ~ref p2) => regex_empty(p1) && regex_empty(p2),
    Alt(~ref p1, ~ref p2) => regex_empty(p1) || regex_empty(p2),
    _ => false
  }
}

// The magic Brzozowski juice
fn derive(re: Regex, c: char) -> Regex{
  simplify(match re {
    Null => Null,
    Blank => Null,
    AnyChar => Blank,
    Char(cpat) if c == cpat => Blank,
    Char(_) => Null,
    Alt(~p1, ~p2) => Alt(~derive(p1, c), ~derive(p2, c)),
    Seq(~p1, ~p2) => {
      let p1div = Seq(~derive(p1.clone(), c), ~p2.clone());
      if regex_empty(&p1) {
        Alt(~derive(p2, c), ~p1div)
      } else {
        p1div
      }
    },
    Rep(~p) => Seq(~derive(p.clone(), c), ~Rep(~p)),
    Not(~p) => Not(~derive(p, c)),
  })
}

// Use the derivatives directly to match against a string
fn do_match(mut re: Regex, data: &str) -> bool {
  for c in data.chars() {
    println!("Deriving re {} with c {}", re, c);
    re = derive(re, c);
    println!("Derived re {}", re);
  }
  regex_empty(&re)
}

#[test]
fn matcher_tests() {
  assert_eq!(derive(Char('a'), 'a'), Blank);
  assert_eq!(derive(Char('a'), 'b'), Null);
  assert_eq!(derive(Seq(~Char('f'), ~Char('b')), 'f'), Char('b'));
  assert_eq!(derive(Alt(~Seq(~Char('f'), ~Char('b')),
                        ~Seq(~Char('f'), ~Rep(~Char('z')))),
                    'f'),
             Alt(~Char('b'), ~Rep(~Char('z'))));
  assert!(do_match(Seq(~Char('b'), ~Rep(~Char('o'))), "boooo"));
  assert!(!do_match(Seq(~Char('b'), ~Rep(~Char('o'))), "bozo"));

  assert!(do_match(Seq(~Char('f'), ~Rep(~Alt(~Char('b'), ~Char('z')))),
                  "fbzbb"));
  assert!(do_match(Not(~Char('a')), "b"));
  assert!(do_match(Not(~Char('a')), "bbb"));
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
  let abcstar = parse_regex(&mut "abc**".chars().peekable());
  assert!(do_match(abcstar.clone(), "abccccc"));
  assert!(!do_match(abcstar, "abbbcc"));
  assert!(matches("((a)*)", "aaaaa"));
  assert!(matches("(a|b)*", "aaaabaabbbaa"));
}
