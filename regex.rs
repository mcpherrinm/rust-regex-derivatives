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
fn matches(mut re: Regex, data: &str) -> bool {
  for c in data.chars() {
    re = derive(re, c);
  }
  regex_empty(&re)
}

#[test]
fn tests() {
  assert_eq!(derive(Char('a'), 'a'), Blank);
  assert_eq!(derive(Char('a'), 'b'), Null);
  assert_eq!(derive(Seq(~Char('f'), ~Char('b')), 'f'), Char('b'));
  assert_eq!(derive(Alt(~Seq(~Char('f'), ~Char('b')),
                        ~Seq(~Char('f'), ~Rep(~Char('z')))),
                    'f'),
             Alt(~Char('b'), ~Rep(~Char('z'))));
  assert!(matches(Seq(~Char('b'), ~Rep(~Char('o'))), "boooo"));
  assert!(!matches(Seq(~Char('b'), ~Rep(~Char('o'))), "bozo"));

  assert!(matches(Seq(~Char('f'), ~Rep(~Alt(~Char('b'), ~Char('z')))),
                  "fbzbb"));
}

