use super::*;

macro_rules! comparisons {
    ($($Name:ident: $op:tt $T:ident,)+) => {
        $(
            pub struct $Name;

            impl<T: $T> BinaryOperator<T> for $Name {
                type Type = bool;
                fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()> {
                    Ok(lhs $op rhs)
                }
            }
        )+
    }
}

comparisons![
    Eq: == PartialEq,
    Ne: != PartialEq,
    Gt: > PartialOrd,
    Lt: < PartialOrd,
    Ge: >= PartialOrd,
    Le: <= PartialOrd,
];
