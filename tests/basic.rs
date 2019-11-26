use tt_match::tt_match;
use quote::{format_ident, quote};

macro_rules! check_expected {
    ($matcher:tt $quote:tt Err) => {{
        let is_err = tt_match!(quote! $quote => $matcher {
            panic!("match should have failed");
        }).is_err();
        assert!(is_err, stringify!($quote));
    }};

    ($matcher:tt $quote:tt { $($e:tt)* }) => {{
        tt_match!(quote! $quote => $matcher { $($e)* })
            .expect(stringify!($quote));
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
