use combine::{
    error::Commit,
    parser::{
        char::{digit, spaces, string},
        choice::or,
    },
    token, ParseError, Parser, Stream, *,
};
use std::collections::HashMap;
#[derive(PartialEq, Debug)]
pub enum Json {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
    Object(HashMap<String, Json>),
    Array(Vec<Json>),
}
#[derive(PartialEq, Debug)]
pub struct ParseResult {
    pub data: Json,
}
impl ParseResult {
    fn new(data: Json) -> ParseResult {
        ParseResult { data }
    }
}
fn json_string<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('"'),
        token('"'),
        many(parser(|input: &mut Input| {
            let (c, committed) = any().parse_lazy(input).into_result()?;
            let mut back_slash_char = satisfy_map(|c| {
                Some(match c {
                    '"' => '"',
                    '\\' => '\\',
                    '/' => '/',
                    'b' => '\u{0008}',
                    'f' => '\u{000c}',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => return None,
                })
            });
            match c {
                '\\' => committed.combine(|_| back_slash_char.parse_stream(input).into_result()),
                '"' => Err(Commit::Peek(Input::Error::empty(input.position()).into())),
                _ => Ok((c, committed)),
            }
        })),
    )
}
fn json_number<Input>() -> impl Parser<Input, Output = f64>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let natural_num = || {
        token('0').map(|_| 0.0).or(one_of("123456789".chars())
            .map(|x| f64::from(x as i32 - 48))
            .and(many(digit().map(|x| f64::from(x as i32 - 48))))
            .map(|(x, xs): (_, Vec<_>)| xs.iter().fold(x, |sum, x| sum * 10.0 + x)))
    };
    let fraction = || {
        token('.')
            .with(many1(digit().map(|x| f64::from(x as i32 - 48))))
            .map(|x: Vec<_>| x.iter().rev().fold(0.0, |sum, x| (sum + x) * 0.1))
    };
    let exponent = || {
        one_of("eE".chars())
            .with(optional(one_of("+-".chars())))
            .and(many1(digit().map(|x| x as i32 - 48)))
            .map(|(sign, x): (_, Vec<_>)| {
                if sign.unwrap_or('+') == '+' {
                    x.iter().fold(0, |sum, x| sum * 10 + x)
                } else {
                    x.iter().fold(0, |sum, x| sum * 10 - x)
                }
            })
    };
    or(
        token('-')
            .with(natural_num())
            .and(optional(fraction()).map(|x| x.unwrap_or(0.0)))
            .map(|(int, frac)| -(int + frac)),
        natural_num()
            .and(optional(fraction()).map(|x| x.unwrap_or(0.0)))
            .map(|(int, frac)| int + frac)
            .and(optional(exponent()).map(|x| x.unwrap_or(0)))
            .map(|(int, exp)| int * 10f64.powi(exp)),
    )
}
fn json_array<Input>() -> impl Parser<Input, Output = Vec<Json>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('[').skip(spaces()),
        token(']'),
        json_value()
            .and(many(token(',').and(spaces()).with(json_value())))
            .map(|(x, xs): (_, Vec<_>)| {
                let mut v = vec![x];
                v.append(&mut xs.into_iter().collect());
                v
            }),
    )
}
fn json_object<Input>() -> impl Parser<Input, Output = HashMap<String, Json>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let key_value = || {
        spaces()
            .with(json_string())
            .skip(spaces())
            .skip(token(':'))
            .and(json_value())
    };
    between(
        token('{').skip(spaces()),
        token('}').skip(spaces()),
        key_value().and(many(token(',').with(key_value()))).map(
            |((key, value), xs): (_, Vec<_>)| {
                let mut map = HashMap::new();
                map.insert(key, value);
                for (key, value) in xs.into_iter() {
                    map.insert(key, value);
                }
                map
            },
        ),
    )
}
fn json_value_<Input>() -> impl Parser<Input, Output = Json>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    spaces()
        .with(choice!(
            json_string().map(Json::String),
            json_number().map(Json::Number),
            string("true").map(|_| Json::Boolean(true)),
            string("false").map(|_| Json::Boolean(false)),
            string("null").map(|_| Json::Null),
            json_array().map(Json::Array),
            json_object().map(Json::Object)
        ))
        .skip(spaces())
}
parser! {
    fn json_value[Input]()(Input) -> Json
    where [Input: Stream<Token = char>]
    {
        json_value_()
    }
}

pub fn json_parser<Input>() -> impl Parser<Input, Output = ParseResult>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    json_value().map(ParseResult::new)
}

#[test]
fn string_ok() {
    let result = json_parser().parse(r#""yes""#).map(|t| t.0);
    assert_eq!(
        result,
        Ok(ParseResult::new(Json::String("yes".to_string())))
    )
}
#[test]
fn escaped_char_tab_ok() {
    let result = json_parser().parse(r#""yes \t \r""#).map(|t| t.0);
    assert_eq!(
        result,
        Ok(ParseResult::new(Json::String("yes \t \r".to_string())))
    )
}
#[test]
fn escaped_char_newline_ok() {
    let result = json_parser().parse(r#""\n""#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::String("\n".to_string()))))
}
#[test]
fn number_ok() {
    let result = json_parser().parse(r#"0"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(0.0))));

    let result = json_parser().parse(r#"3"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(3.0))));

    let result = json_parser().parse(r#"42"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(42.0))));

    let result = json_parser().parse(r#"-42"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(-42.0))));

    let result = json_parser().parse(r#"-0"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(0.0))))
}
#[test]
fn fraction_ok() {
    let text = r#"3.1415"#;
    let expected = ParseResult::new(Json::Number(3.1415));

    let result = json_parser().parse(text).map(|t| t.0);
    assert_eq!(result, Ok(expected))
}
#[test]
fn exponent_ok() {
    let result = json_parser().parse(r#"1e4"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(10000.0))));

    let result = json_parser().parse(r#"1e+4"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(10000.0))));

    let result = json_parser().parse(r#"31415e-4"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Number(3.1415))))
}
#[test]
fn null_ok() {
    let result = json_parser().parse(r#"null"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Null)))
}
#[test]
fn bool_ok() {
    let result = json_parser().parse(r#"true"#).map(|t| t.0);
    assert_eq!(result, Ok(ParseResult::new(Json::Boolean(true))))
}
#[test]
fn array_ok() {
    let result = json_parser().parse(r#"[true, 42, "yes"]"#).map(|t| t.0);
    assert_eq!(
        result,
        Ok(ParseResult::new(Json::Array(vec![
            Json::Boolean(true),
            Json::Number(42.0),
            Json::String("yes".to_string()),
        ])))
    )
}
#[test]
fn nested_array_ok() {
    let text = r#"[[[true, 42, "yes"]]]"#;
    let expected = ParseResult::new(Json::Array(vec![Json::Array(vec![Json::Array(vec![
        Json::Boolean(true),
        Json::Number(42.0),
        Json::String("yes".to_string()),
    ])])]));

    let result = json_parser().parse(text).map(|t| t.0);
    assert_eq!(result, Ok(expected))
}
#[test]
fn object_ok() {
    let text = r#"{"number": 42, "string": "yes"}"#;
    let expected = ParseResult::new(Json::Object({
        let mut map = HashMap::new();
        map.insert("number".to_string(), Json::Number(42.0));
        map.insert("string".to_string(), Json::String("yes".to_string()));
        map
    }));

    let result = json_parser().parse(text).map(|t| t.0);
    assert_eq!(result, Ok(expected))
}
#[test]
fn complex_example_ok() {
    let text = r#"{"number": 42, "string": "yes", "array": [42, true, "yes"], "object": { "nested": null } }"#;
    let expected = ParseResult::new(Json::Object({
        let mut map = HashMap::new();
        map.insert("number".to_string(), Json::Number(42.0));
        map.insert("string".to_string(), Json::String("yes".to_string()));
        map.insert(
            "array".to_string(),
            Json::Array(vec![
                Json::Number(42.0),
                Json::Boolean(true),
                Json::String("yes".to_string()),
            ]),
        );
        let mut nested_map = HashMap::new();
        nested_map.insert("nested".to_string(), Json::Null);
        map.insert("object".to_string(), Json::Object(nested_map));
        map
    }));

    let result = json_parser().parse(text).map(|t| t.0);
    assert_eq!(result, Ok(expected))
}
#[test]
fn whitespace_ok() {
    let text = r#"
      [
      true, 42, "yes", null
      ]"#;
    let expected = ParseResult::new(Json::Array(vec![
        Json::Boolean(true),
        Json::Number(42.0),
        Json::String("yes".to_string()),
        Json::Null,
    ]));

    let result = json_parser().parse(text).map(|t| t.0);
    assert_eq!(result, Ok(expected))
}
