// TODO: This is all copy-pasted from graph-node. Needs to move to a common lib.
use graphql_parser::query as q;
use serde::{self, Deserialize, Deserializer};
use std::collections::{BTreeMap, HashMap};

// TODO: (Performance) may want to do zero-copy here later.
type StaticValue = q::Value<'static, String>;

/// Variable value for a GraphQL query.
#[derive(Clone, Debug, Deserialize)]
pub struct DeserializableGraphQlValue(#[serde(with = "GraphQLValue")] StaticValue);

fn deserialize_variables<'de, D>(deserializer: D) -> Result<HashMap<String, StaticValue>, D::Error>
where
    D: Deserializer<'de>,
{
    let pairs: BTreeMap<String, DeserializableGraphQlValue> =
        Deserialize::deserialize(deserializer)?;
    Ok(pairs.into_iter().map(|(k, v)| (k, v.0)).collect())
}

/// Variable values for a GraphQL query.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub struct QueryVariables(
    #[serde(deserialize_with = "deserialize_variables")] HashMap<String, StaticValue>,
);

impl QueryVariables {
    pub fn new() -> Self {
        QueryVariables(HashMap::new())
    }

    pub fn get(&self, name: &str) -> Option<&StaticValue> {
        self.0.get(name)
    }
}

#[derive(Deserialize)]
#[serde(untagged, remote = "StaticValue")]
enum GraphQLValue {
    #[serde(deserialize_with = "deserialize_number")]
    Int(q::Number),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
    Enum(String),
    #[serde(deserialize_with = "deserialize_list")]
    List(Vec<StaticValue>),
    #[serde(deserialize_with = "deserialize_object")]
    Object(BTreeMap<String, StaticValue>),
}

fn deserialize_number<'de, D>(deserializer: D) -> Result<q::Number, D::Error>
where
    D: Deserializer<'de>,
{
    let i: i32 = Deserialize::deserialize(deserializer)?;
    Ok(q::Number::from(i))
}

fn deserialize_list<'de, D>(deserializer: D) -> Result<Vec<StaticValue>, D::Error>
where
    D: Deserializer<'de>,
{
    let values: Vec<DeserializableGraphQlValue> = Deserialize::deserialize(deserializer)?;
    Ok(values.into_iter().map(|v| v.0).collect())
}

fn deserialize_object<'de, D>(deserializer: D) -> Result<BTreeMap<String, StaticValue>, D::Error>
where
    D: Deserializer<'de>,
{
    let pairs: BTreeMap<String, DeserializableGraphQlValue> =
        Deserialize::deserialize(deserializer)?;
    Ok(pairs.into_iter().map(|(k, v)| (k, v.0)).collect())
}
