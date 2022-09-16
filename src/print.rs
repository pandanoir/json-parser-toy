use crate::parser::Json;

fn stringify_json(json: Json) -> String {
    match json {
        Json::Number(n) => n.to_string(),
        Json::String(s) => "\"".to_owned() + &s + "\"",
        Json::Boolean(b) => b.to_string(),
        Json::Null => "null".to_string(),
        Json::Array(arr) => ("[".to_owned()
            + &arr
                .into_iter()
                .map(stringify_json)
                .collect::<Vec<_>>()
                .join(",")
            + "]")
            .to_string(),
        Json::Object(obj) => ("{".to_owned()
            + &obj
                .into_iter()
                .map(|(key, val)| "\"".to_owned() + &key + "\":" + &stringify_json(val))
                .collect::<Vec<_>>()
                .join(",")
            + "}")
            .to_string(),
    }
}
pub fn print_json(json: Json) {
    println!("{}", stringify_json(json))
}
