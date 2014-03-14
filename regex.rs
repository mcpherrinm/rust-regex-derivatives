#[deriving(Clone)]
#[deriving(Eq)]
#[deriving(Show)]
enum Regex {
  Null,
  Blank,
  Char(char),
  Alt(~Regex, ~Regex),
  Seq(~Regex, ~Regex),
  Rep(~Regex)
}

fn simplify(re: Regex) -> Regex {
  match re {
    Seq(~Null, _) => Null,
    Seq(_, ~Null) => Null,
    Seq(~Blank, ~r) => r,
    Seq(~r, ~Blank) => r,
    Alt(~Null, ~r) => r,
    Alt(~r, ~Null) => r,
    Rep(~Null) => Blank,
    Rep(~Blank) => Blank,
    _ => re
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
  })
}

// Use the derivatives directly to match against a string
fn do_match(mut re: Regex, data: &str) -> bool {
  for c in data.chars() {
    re = derive(re, c);
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

fn parse_regex<'a>(data: CharIter<'a>) -> (Regex, CharIter<'a>) {
  let (term, mut data) = parse_term(data);
  if data.peek() == Some(&'|') {
    data.next();
    let (regex, data) = parse_regex(data);
    (Alt(~term, ~regex), data)
  } else {
    (term, data)
  }
}

fn parse_term<'a>(data: CharIter<'a>) -> (Regex, CharIter<'a>) {
  let (mut factor, mut data) = parse_factor(data);
  loop {
    match data.peek() {
      None => return (factor, data),
      Some(&')') => return (factor, data),
      Some(&'|') => return (factor, data),
      _ => (),
    }
    let (nextfactor, nextdata) = parse_factor(data);
    factor = Seq(~factor, ~nextfactor);
    data = nextdata;
  }
}

fn parse_factor<'a>(data: CharIter<'a>) -> (Regex, CharIter<'a>) {
  let (mut base, mut data) = parse_base(data);
  while !data.is_empty() && data.peek() == Some(&'*') {
    data.next();
    base = Rep(~base);
  };
  (base, data)
}

fn parse_base<'a>(mut data: CharIter<'a>) -> (Regex, CharIter<'a>) {
  match data.next() {
    Some('(') => {
      let (nested, mut newdata) = parse_regex(data);
      newdata.next(); // consume the ')'
      (nested, newdata)
    }
    Some('\\') => {
      let c = data.next().unwrap();
      (Char(c), data)
    }
    Some(c) => (Char(c), data),
    None => fail!("bad regex")
  }
}

// Convenience wrappers
fn regex(str: &str) -> Regex {
  let (r, _) = parse_regex(str.chars().peekable());
  r
}

fn matches(expr: &str, data: &str) -> bool {
  let r = regex(expr);
  do_match(r, data)
}

#[test]
fn parsing_tests() {
  let abcstar = regex("abc**");
  assert!(do_match(abcstar.clone(), "abccccc"));
  assert!(!do_match(abcstar, "abbbcc"));
  assert!(matches("((a)*)", "aaaaa"));
  assert!(matches("(a|b)*", "aaaabaabbbaa"));
}
