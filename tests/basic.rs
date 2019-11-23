use tt_match::tt_matcher;
use quote::{format_ident, quote};
use syn::parse::Parser;

macro_rules! check_expected {
    ($matcher:tt $quote:tt Err) => {{
        tt_matcher! {
            match fn matcher $matcher -> () { }
        }

        let is_err = Parser::parse2(&matcher, quote! $quote).is_err();
        assert!(is_err, stringify!($quote));
    }};

    ($matcher:tt $quote:tt { $($e:tt)* }) => {{
        tt_matcher! {
            match fn matcher $matcher -> () { $($e)* }
        }

        Parser::parse2(&matcher, quote! $quote).expect(stringify!($quote));
    }};
}

macro_rules! test_matcher {
    (match $matcher:tt; $($quote:tt => $e:tt;)*) => {
        $( check_expected!($matcher $quote $e); )*
    };
}

#[test]
fn test_star() {
    test_matcher! {
        match (prefix #(nested #_id:ident)*);
        (prefix nested apple nested pear) => {
            assert_eq!(_id, &[format_ident!("apple"), format_ident!("pear")]);
        };
        (prefix nested) => Err;
        (prefix nested {}) => Err;
        (prefix) => { assert!(_id.is_empty()); };
    }
}

#[test]
fn test_plus() {
    test_matcher! {
        match (#(#_id:ident),+);
        (a, b, c) => { assert_eq!(_id, &["a", "b", "c"]); };
        (a) => { assert_eq!(_id, &["a"]); };
        () => Err;
        (a b) => Err;
        (a, b, c,) => Err;
    }
}

#[test]
fn test_opt() {
    test_matcher! {
        match (#(#_id:ident)?);
        () => { assert_eq!(_id, None); };
        (a) => { assert_eq!(_id, Some(format_ident!("a"))); };
        (a b) => Err;
    }
}
