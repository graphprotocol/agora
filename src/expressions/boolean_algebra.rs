//! Boolean Algebra operators

use super::*;

// TODO: Basic is going to be LinearExpr Op LinearExpr
// With ||, && to join Conditions with left to right precedence
// Also parenthesis

macro_rules! boolean_op {
    ($($Name:ident: $op:tt,)+) => {
        $(
            #[derive(Debug, PartialEq, Eq, Clone)]
            pub struct $Name;

            impl BinaryOperator<bool> for $Name {
                type Type = bool;
                fn exec(&self, lhs: bool, rhs: bool) -> Result<bool, ()> {
                    Ok(lhs $op rhs)
                }
            }

            impl From<$Name> for AnyBooleanOp {
                #[inline(always)]
                fn from(_op: $Name) -> Self {
                    AnyBooleanOp::$Name
                }
            }
        )+

        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum AnyBooleanOp {
            $(
                $Name,
            )+
        }

        impl BinaryOperator<bool> for AnyBooleanOp {
            type Type = bool;
            fn exec(&self, lhs: bool, rhs: bool) -> Result<bool, ()> {
                match self {
                    $(
                        Self::$Name => $Name.exec(lhs, rhs),
                    )+
                }
            }
        }
    }
}

boolean_op![
    And: &&,
    Or: ||,
];
