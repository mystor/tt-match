//! Not public API. Runtime structs, re-exports, traits, and functions used by
//! macros in this crate.

use proc_macro2::Spacing;
use std::marker::PhantomData;
use syn::parse::ParseBuffer;

// re-exports
pub use std::mem::drop;
pub use std::option::Option::{self, None, Some};
pub use std::result::Result::{Err, Ok};
pub use std::vec::Vec;

pub use syn::parse::discouraged::Speculative;
pub use syn::parse::{ParseStream, Parser};
pub use syn::{Result, Error};
pub use syn::{
    Item,
    Block,
    Stmt,
    Pat,
    Expr,
    Type,
    Ident,
    Path,
    Lifetime,
    Visibility,
    Lit,
};

pub use proc_macro2::{TokenStream, TokenTree};

// Container which can be the parent of `Pending` or `Active`.
pub trait ParentContainer<T> {
    fn insert(&mut self, value: T);
}

// Pending represents a value which hasn't been fully initialized yet.
pub struct Pending<T, P = ()> {
    parent: P,
    marker: PhantomData<T>,
}

impl<T> Pending<T, ()> {
    pub fn new() -> Self {
        Pending {
            parent: (),
            marker: PhantomData,
        }
    }
}

impl<T, P> Pending<T, P> {
    pub fn set(self, value: T) -> Active<T, P> {
        Active {
            parent: self.parent,
            value,
        }
    }
}

impl<T, P> Pending<T, P>
where
    T: Default,
{
    pub fn set_default(self) -> Active<T, P> {
        Active {
            parent: self.parent,
            value: Default::default(),
        }
    }
}

// Active represents a value which has already been fully initialized.
pub struct Active<T, P = ()> {
    parent: P,
    value: T,
}

impl<T> Active<T, ()> {
    pub fn finish(self) -> T {
        self.value
    }
}

impl<'a, T, P> Active<T, &'a mut P>
where
    P: ParentContainer<T>,
{
    pub fn exit(self) {
        self.parent.insert(self.value);
    }
}

// Container helper methods for entering, inserting, and exiting values.
impl<T, P> Active<Vec<T>, P> {
    pub fn enter(&mut self) -> Pending<T, &mut Self> {
        Pending {
            parent: self,
            marker: PhantomData,
        }
    }
}

impl<T, P> ParentContainer<T> for Active<Vec<T>, P> {
    fn insert(&mut self, value: T) {
        self.value.push(value);
    }
}

impl<T, P> Active<Option<T>, P> {
    pub fn enter(&mut self) -> Pending<T, &mut Self> {
        Pending {
            parent: self,
            marker: PhantomData,
        }
    }
}

impl<T, P> ParentContainer<T> for Active<Option<T>, P> {
    fn insert(&mut self, value: T) {
        assert!(self.value.is_none());
        self.value = Some(value);
    }
}

pub trait TokenSource {
    fn run_parser<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(ParseStream) -> Result<R>;
}

impl TokenSource for TokenStream {
    fn run_parser<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(ParseStream) -> Result<R>,
    {
        f.parse2(self.clone())
    }
}

impl<'a> TokenSource for ParseBuffer<'a> {
    fn run_parser<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(ParseStream) -> Result<R>,
    {
        f(self)
    }
}

impl<'a, T> TokenSource for &'a T
where
    T: TokenSource,
{
    fn run_parser<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(ParseStream) -> Result<R>,
    {
        (**self).run_parser(f)
    }
}

// Parse an input from expected.
pub fn match_parse(input: ParseStream, expected: &str) -> Result<()> {
    let stream = expected.parse::<TokenStream>().unwrap();
    for expect in stream {
        input.step(|cursor| {
            let (tt, next) = cursor
                .token_tree()
                .ok_or_else(|| cursor.error(format_args!("Expected `{}`", expect)))?;

            let equal = match (&tt, &expect) {
                // Allow the case where `b.spacing() == Spacing::Alone` yet
                // `a.spacing() == Spacing::Joint`. In that scenario we're
                // matching the prefix of an operator, which should be allowed.
                (TokenTree::Punct(a), TokenTree::Punct(b)) => {
                    a.as_char() == b.as_char()
                        && (b.spacing() == Spacing::Alone || a.spacing() == b.spacing())
                }

                (TokenTree::Ident(a), TokenTree::Ident(b)) => a == b,
                (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),

                // A group may be able to show up if an `$expr` or similar was
                // injected into the pattern.
                //
                // FIXME: Should we call `match_parse` recursively, or is it OK
                // to just `to_string()`.
                (TokenTree::Group(a), TokenTree::Group(b)) => a.to_string() == b.to_string(),
                _ => false,
            };
            if !equal {
                return Err(cursor.error(format_args!("Expected `{}`", expect)))?;
            }
            Ok(((), next))
        })?;
    }

    Ok(())
}
