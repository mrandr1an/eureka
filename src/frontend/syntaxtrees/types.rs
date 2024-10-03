use std::fmt::{Debug, Display};

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    Real,
    Char,
    Ptr(Box<Type>),
    Aggregate(Vec<Type>),
    Variant(Vec<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char => write!(f, "ΧΑΡΑΚΤΗΡΑΣ"),
            Self::Real => write!(f, "ΠΡΑΓΜΑΤΙΚΟΣ"),
            Self::Ptr(t) => write!(f, "<ΔΕΙΚΤΗΣ {}>", t),
            Self::Aggregate(t) => {
                let _ = write!(f, "<ΣΥΝΕΝΩΣΗ ");
                for ty in t {
                    let _ = write!(f, "{} ", ty);
                }
                write!(f, ">")
            }
            Self::Variant(t) => {
                let _ = write!(f, "<ΑΘΡΟΙΣΜΑ");
                for ty in t {
                    let _ = write!(f, "{} ", ty);
                }
                write!(f, ">")
            }
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char => write!(f, "ΧΑΡΑΚΤΗΡΑΣ"),
            Self::Real => write!(f, "ΠΡΑΓΜΑΤΙΚΟΣ"),
            Self::Ptr(t) => write!(f, "<ΔΕΙΚΤΗΣ {}>", t),
            Self::Aggregate(t) => {
                let _ = write!(f, "<ΣΥΝΕΝΩΣΗ ");
                for ty in t {
                    let _ = write!(f, "{} ", ty);
                }
                write!(f, ">")
            }
            Self::Variant(t) => {
                let _ = write!(f, "<ΑΘΡΟΙΣΜΑ");
                for ty in t {
                    let _ = write!(f, "{} ", ty);
                }
                write!(f, ">")
            }
        }
    }
}
