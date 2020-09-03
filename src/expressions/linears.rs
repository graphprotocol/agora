use super::*;
use std::ops;

macro_rules! linear_op {
    ($($Name:ident: $op:tt,)+) => {
        $(
            #[derive(Debug, PartialEq, Eq, Copy, Clone)]
            pub struct $Name;

            impl<T: ops::$Name> BinaryOperator<T> for $Name {
                type Type = T::Output;
                #[inline(always)]
                fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()> {
                    Ok(lhs $op rhs)
                }
            }

            impl From<$Name> for AnyLinearOperator {
                #[inline(always)]
                fn from(_op: $Name) -> Self {
                    AnyLinearOperator::$Name
                }
            }
        )+

        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum AnyLinearOperator {
            $(
                $Name,
            )+
        }

        impl<T> BinaryOperator<T> for AnyLinearOperator where
            $(T: ops::$Name<Output=T>,)+
        {
            type Type = T;
            fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()> {
                match self {
                    $(Self::$Name => $Name.exec(lhs, rhs),)+
                }
            }
        }

    }
}

linear_op![
    Add: +,
    Sub: -,
    Mul: *,
    Div: /,
];
