use fraction::BigFraction;
use graphql::graphql_parser::query as q;
use q::Value::*;

/// This is like TryInto, but more liberal
pub trait Coerce<T> {
    type Error;
    fn coerce(&self) -> Result<T, Self::Error>;
}

// TODO: Consider that coercing to a bool and int from a "BigInt" style string may have surprising results
// For the case "0", this coerces to 0, which would coerce to false. But, coercing the string "0" to bool directly
// yields true. This is JSON's fault. It may be necessary to address this with docs
// Eg: An int < i32::MAX should never be sent as a str
// Or - possibly strangely, require that there are never leading '0s' including the case of 0 which would be "".

impl<'t, Text: q::Text<'t>> Coerce<bool> for q::Value<'t, Text> {
    type Error = ();
    fn coerce(&self) -> Result<bool, Self::Error> {
        match self {
            Boolean(b) => Ok(*b),
            Null => Ok(false),
            Int(i) => Ok(*i != q::Number::from(0)),
            String(s) => Ok(!s.is_empty()),
            List(l) => Ok(!l.is_empty()),
            Object(_) => Ok(true),
            Variable(_) | Float(_) | Enum(_) => Err(()),
        }
    }
}

impl<'t, Text: q::Text<'t>> Coerce<BigFraction> for q::Value<'t, Text> {
    type Error = ();
    fn coerce(&self) -> Result<BigFraction, Self::Error> {
        match self {
            Boolean(b) => Ok(if *b { 1.into() } else { 0.into() }),
            Null => Ok(0.into()),
            Int(i) => Ok(i.as_i64().ok_or(())?.into()),
            String(s) => crate::parse_real(s).map_err(|_| ()),
            List(_) | Object(_) | Variable(_) | Float(_) | Enum(_) => Err(()),
        }
    }
}
