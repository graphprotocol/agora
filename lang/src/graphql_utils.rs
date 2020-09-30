// TODO: This is all copy-pasted from graph-node. Needs to move to a common lib.
use graphql_parser::query as q;
use serde::{self, Deserialize, Deserializer};
use std::collections::{BTreeMap, HashMap};

// TODO: (Performance) may want to do zero-copy here later.
pub type StaticValue = q::Value<'static, String>;

/// Variable value for a GraphQL query.
#[derive(Clone, Debug, Deserialize)]
struct DeserializableGraphQlValue(#[serde(with = "GraphQLValue")] StaticValue);

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

pub trait IntoStaticValue {
    fn to_graphql(self) -> StaticValue;
}

impl IntoStaticValue for i32 {
    fn to_graphql(self) -> StaticValue {
        StaticValue::Int(self.into())
    }
}

impl IntoStaticValue for bool {
    fn to_graphql(self) -> StaticValue {
        StaticValue::Boolean(self)
    }
}

impl IntoStaticValue for StaticValue {
    fn to_graphql(self) -> StaticValue {
        self
    }
}

impl IntoStaticValue for String {
    fn to_graphql(self) -> StaticValue {
        StaticValue::String(self)
    }
}

impl<'a, T: q::Text<'a>> IntoStaticValue for &'_ q::Value<'a, T> {
    fn to_graphql(self) -> StaticValue {
        match self {
            q::Value::Boolean(b) => StaticValue::Boolean(*b),
            q::Value::Enum(t) => StaticValue::Enum(t.as_ref().to_string()),
            q::Value::Float(f) => StaticValue::Float(*f),
            q::Value::Int(i) => StaticValue::Int(i.clone()),
            q::Value::Variable(v) => StaticValue::Variable(v.as_ref().to_string()),
            q::Value::Object(o) => StaticValue::Object(
                o.iter()
                    .map(|(k, v)| (k.as_ref().to_string(), v.to_graphql()))
                    .collect(),
            ),
            q::Value::List(l) => {
                StaticValue::List(l.iter().map(IntoStaticValue::to_graphql).collect())
            }
            q::Value::String(s) => StaticValue::String(s.to_string()),
            q::Value::Null => StaticValue::Null,
        }
    }
}
