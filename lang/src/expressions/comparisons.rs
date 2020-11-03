use super::*;

macro_rules! comparisons {
    ($($Name:ident: $op:tt $T:ident,)+) => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq)]
            pub struct $Name;

            impl<T: $T> BinaryOperator<T> for $Name {
                type Type = bool;
                fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()> {
                    Ok(lhs $op rhs)
                }
            }

            impl From<$Name> for AnyComparison {
                #[inline(always)]
                fn from(_op: $Name) -> Self {
                    AnyComparison::$Name
                }
            }
        )+

        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum AnyComparison {
            $(
                $Name,
            )+
        }

        impl<T> BinaryOperator<T> for AnyComparison where
            T: PartialEq + PartialOrd
        {
            type Type = bool;
            fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()> {
                match self {
                    $(Self::$Name => $Name.exec(lhs, rhs),)+
                }
            }
        }
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
