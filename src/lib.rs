#[doc(hidden)]
pub use syn::{
    braced as _syn_braced, bracketed as _syn_bracketed, parenthesized as _syn_parenthesized,
};

// Not public API.
#[doc(hidden)]
#[path = "runtime.rs"]
pub mod __rt;

/// The whole point.
///
/// Pattern match against an input `TokenStream`, and evaluate an expression.
/// This acts like `macro_rules!` style matchers, but within a proc_macro.
///
/// # Supported Matchers
///
/// * Repetitions:
///   * `#(...)*`, `#(...),*`, `#(...)+` and `#(...),+`
/// * Optionals:
///   * `#(...)?`
/// * `#var:KIND`, where `KIND` is one of:
///   * `item` ([`syn::Item`])
///   * `block` ([`syn::Block`])
///   * `stmt` ([`syn::Stmt`])
///   * `pat` ([`syn::Pat`])
///   * `expr` ([`syn::Expr`])
///   * `ty` ([`syn::Type`])
///   * `ident` ([`proc_macro2::Ident`])
///   * `path` ([`syn::Path`])
///   * `tt` ([`proc_macro2::TokenTree`])
///   * `lifetime` ([`syn::Lifetime`](`struct@syn::Lifetime`))
///   * `vis` ([`syn::Visibility`])
///
/// # Examples
///
/// ```
/// # use tt_match::tt_match;
/// # use quote::quote;
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let input = quote! {
///     static ref MY_NAME: MyType = MyType::new();
/// };
/// let (name, ty, value) = tt_match!(input=> (
///     static ref #name:ident : #ty:ty = #value:expr;
/// ) {
///     (name, ty, value)
/// })?;
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! tt_match {
    ($input:expr => ( $($t:tt)* ) { $($e:tt)* }) => {
        $crate::__rt::Parser::parse2(
            |input: $crate::__rt::ParseStream| -> $crate::__rt::Result<_> {
                // let var = Pending::new();
                $crate::match_each_cap!(new $($t)*);
                // -- parsing --
                $crate::match_each_token!([match_token_cb! input] $($t)*);
                if !input.is_empty() {
                    return $crate::__rt::Err(input.error("unexpected token"));
                }
                // let var = var.finish();
                $crate::match_each_cap!(finish $($t)*);

                // Pattern Code
                $crate::__rt::Ok((|| { $($e)* })())
            },
            $input.into(),
        )
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! match_subparser {
    ($input:ident $($inner:tt)*) => {{
        // let var = var.enter();
        $crate::match_each_cap!(enter $($inner)*);
        // -- parsing --
        $crate::match_each_token!([match_token_cb! $input] $($inner)*);
        // var.exit();
        $crate::match_each_cap!(exit $($inner)*);
    }};
}

// Tries to run a subparser. Expands to an expression evaluating to `true` on
// success, and `false` on failure.
//
// FIXME: This produces really bad error messages. It should not try to parse
// the entirety of `$inner`, but rather only the first token, to decide whether
// or not to recurse.
//
// FIXME: Figure out a good way to implement a better parser here (ideally
// without `Speculative`).
#[macro_export]
#[doc(hidden)]
macro_rules! match_try_parse {
    ($input:ident $($inner:tt)*) => {
        (|| -> $crate::__rt::Result<()> {
            let fork = $input.fork();
            $crate::match_subparser!(fork $($inner)*);

            use $crate::__rt::Speculative;
            $input.advance_to(&fork);
            $crate::__rt::Ok(())
        })()
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! match_token_cb {
    // NOTE: These use `drop(content)`, and check for `ParseBuffer` emptiness
    // explicitly, rather than introducing a scope, as logic depends on a flat
    // namespace while matching non-{rep,opt} tokens.
    ($input:ident Tok ( $($inner:tt)* )) => {
        let content;
        $crate::_syn_parenthesized!(content in $input);
        $crate::match_each_token!([match_token_cb! content] $($inner)*);
        if !content.is_empty() {
            return Err(content.error("unexpected token"));
        }
        $crate::__rt::drop(content);  // Scope `content` var.
    };
    ($input:ident Tok [ $($inner:tt)* ]) => {
        let content;
        $crate::_syn_bracketed!(content in $input);
        $crate::match_each_token!([match_token_cb! content] $($inner)*);
        if !content.is_empty() {
            return Err(content.error("unexpected token"));
        }
        $crate::__rt::drop(content);  // Scope `content` var.
    };
    ($input:ident Tok { $($inner:tt)* }) => {
        let content;
        $crate::_syn_braced!(content in $input);
        $crate::match_each_token!([match_token_cb! content] $($inner)*);
        if !content.is_empty() {
            return Err(content.error("unexpected token"));
        }
        $crate::__rt::drop(content);  // Scope `content` var.
    };

    // FIXME: Consider matching certain `$other` values, such as idents &
    // operators, in a more efficient manner.
    ($input:ident Tok $other:tt) => {
        $crate::__rt::match_parse(&$input, stringify!($other))?;
    };

    // #(...)*
    ($input:ident Rep $($inner:tt)+) => {
        $crate::match_each_cap!(set_default $($inner)*);
        while $crate::match_try_parse!($input $($inner)*).is_ok() { }
    };
    // #(...),*
    ($input:ident RepSep $sep:tt $($inner:tt)+) => {
        $crate::match_each_cap!(set_default $($inner)*);
        if $crate::match_try_parse!($input $($inner)*).is_ok() {
            // Repeatedly try to parse with a leading `sep`.
            while $crate::match_try_parse!($input $sep $($inner)*).is_ok() { }
        }
    };
    // #(...)+
    ($input:ident Rep1 $($inner:tt)+) => {
        $crate::match_each_cap!(set_default $($inner)*);

        $crate::match_subparser!($input $($inner)*);
        while $crate::match_try_parse!($input $($inner)*).is_ok() { }
    };
    // #(...),+
    ($input:ident RepSep1 $sep:tt $($inner:tt)+) => {
        $crate::match_each_cap!(set_default $($inner)*);

        $crate::match_subparser!($input $($inner)*);
        // Repeatedly try to parse with a leading `sep`.
        while $crate::match_try_parse!($input $sep $($inner)*).is_ok() { }
    };
    // #(...)?
    ($input:ident Opt $($inner:tt)+) => {
        $crate::match_each_cap!(set_default $($inner)*);
        let _ = $crate::match_try_parse!($input $($inner)*);
    };

    // Default `Parse` handling for a single capture.
    ($input:ident Cap $var:ident : $kind:ident) => {
        let $var = $var.set($input.parse()?);
    };
}

// match_each_cap!(cb!(...) #foo:ident #(opt #(#bar:ident)*)?) =>
//   cb!(... foo: MatchCapTy![[] ident]);
//   cb!(... foo: MatchCapTy![[Option Vec] ident]);
#[macro_export]
#[doc(hidden)]
macro_rules! match_each_cap {
    ($cb:tt $($t:tt)*) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb []] $($t)*
        );
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! match_each_cap_cb {
    // non-repeating `Group`s
    ($cb:tt $wr:tt Tok ( $($inner:tt)* )) => {
        $crate::match_each_token!([match_each_cap_cb! $cb $wr] $($inner)*);
    };
    ($cb:tt $wr:tt Tok [ $($inner:tt)* ]) => {
        $crate::match_each_token!([match_each_cap_cb! $cb $wr] $($inner)*);
    };
    ($cb:tt $wr:tt Tok { $($inner:tt)* }) => {
        $crate::match_each_token!([match_each_cap_cb! $cb $wr] $($inner)*);
    };
    ($cb:tt $wr:tt Tok $t:tt) => {};

    // complex non-terminals
    ($cb:tt [$($wr:ident)*] Rep $($inner:tt)+) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb [$($wr)* Vec]] $($inner)*
        );
    };
    ($cb:tt [$($wr:ident)*] RepSep $sep:tt $($inner:tt)+) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb [$($wr)* Vec]] $($inner)*
        );
    };
    ($cb:tt [$($wr:ident)*] Rep1 $($inner:tt)+) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb [$($wr)* Vec]] $($inner)*
        );
    };
    ($cb:tt [$($wr:ident)*] RepSep1 $sep:tt $($inner:tt)+) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb [$($wr)* Vec]] $($inner)*
        );
    };
    ($cb:tt [$($wr:ident)*] Opt $($inner:tt)+) => {
        $crate::match_each_token!(
            [match_each_cap_cb! $cb [$($wr)* Option]] $($inner)*
        );
    };

    // captures
    (new $wr:tt Cap $var:ident : $kind:ident) => {
        let $var = <$crate::__rt::Pending<$crate::MatchCapTy![$wr $kind]>>::new();
    };
    (finish $wr:tt Cap $var:ident : $kind:ident) => {
        let $var = $var.finish();
    };
    (set_default $wr:tt Cap $var:ident : $kind:ident) => {
        let mut $var = $var.set_default();
    };
    (enter $wr:tt Cap $var:ident : $kind:ident) => {
        let $var = $var.enter();
    };
    (exit $wr:tt Cap $var:ident : $kind:ident) => {
        $var.exit();
    };
}

// MatchCapTy![[Vec Option] tt] => Vec<Option<TokenTree>>
#[macro_export]
#[doc(hidden)]
macro_rules! MatchCapTy {
    ([] item) => { $crate::__rt::Item };
    ([] block) => { $crate::__rt::Block };
    ([] stmt) => { $crate::__rt::Stmt };
    ([] pat) => { $crate::__rt::Pat };
    ([] expr) => { $crate::__rt::Expr };
    ([] ty) => { $crate::__rt::Type };
    ([] ident) => { $crate::__rt::Ident };
    ([] path) => { $crate::__rt::Path };
    ([] tt) => { $crate::__rt::TokenTree };
    ([] lifetime) => { $crate::__rt::Lifetime };
    ([] vis) => { $crate::__rt::Visibility };

    // FIXME: This doesn't handle negative numbers, which should be handled to
    // match with `macro_rules!`.
    ([] literal) => { $crate::__rt::Lit };

    // TODO: Figure out what is going on with `:meta`
    // We don't want to use `Meta`, as that won't match `path (...anything...)`,
    // which is matched by `macro_rules!`.
    // ([] meta) => { $crate::__rt::?? };

    ([$T:ident $($wr:ident)*] $kind:ident) => {
        $crate::__rt::$T::<$crate::MatchCapTy![[$($wr)*] $kind]>
    };
}

// Call a callback for each "token" in the stream, after classifying it. Does
// not recurse into groups.
//
// match_each_token!([cb! ...] a #b:ident [group]) =>
//   cb!(... Tok a);
//   cb!(... Cap b : ident);
//   cb!(... Tok [group]);
#[macro_export]
#[doc(hidden)]
macro_rules! match_each_token {
    ($cfg:tt $($tts:tt)*) => {
        $crate::match_each_token_with_context!(
            $cfg
            (@ @ @ @ @ @ $($tts)*)
            (@ @ @ @ @ $($tts)* @)
            (@ @ @ @ $($tts)* @ @)
            (@ @ @ $(($tts))* @ @ @)
            (@ @ $($tts)* @ @ @ @)
            (@ $($tts)* @ @ @ @ @)
            ($($tts)* @ @ @ @ @ @)
        );
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! match_each_token_with_context {
    (
        $cfg:tt
        ($($b3:tt)*) ($($b2:tt)*) ($($b1:tt)*)
        ($($curr:tt)*)
        ($($a1:tt)*) ($($a2:tt)*) ($($a3:tt)*)
    ) => {
        $(
            $crate::match_token_with_context!($cfg $b3 $b2 $b1 $curr $a1 $a2 $a3);
        )*
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! match_token_with_context {
    // leading & trailing '@'s
    ($cb:tt $b3:tt $b2:tt $b1:tt @ $a1:tt $a2:tt $a3:tt) => {};

    // #(...)* => cb!(Rep ...)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) * $a3:tt) => {
        $crate::match_call_cb!($cb Rep $($inner)*);
    };
    ($cb:tt $b3:tt $b2:tt # (( $($inner:tt)* )) * $a2:tt $a3:tt) => {};
    ($cb:tt $b3:tt # ( $($inner:tt)* ) (*) $a1:tt $a2:tt $a3:tt) => {};

    // #(...),* => cb!(RepSep , ...)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) $sep:tt *) => {
        $crate::match_call_cb!($cb RepSep $sep $($inner)*);
    };
    ($cb:tt $b3:tt $b2:tt # (( $($inner:tt)* )) $sep:tt * $a3:tt) => {};
    ($cb:tt $b3:tt # ( $($inner:tt)* ) ($sep:tt) * $a2:tt $a3:tt) => {};
    ($cb:tt # ( $($inner:tt)* ) $sep:tt (*) $a1:tt $a2:tt $a3:tt) => {};

    // #(...)* => cb!(Rep1 ...)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) + $a3:tt) => {
        $crate::match_call_cb!($cb Rep1 $($inner)*);
    };
    ($cb:tt $b3:tt $b2:tt # (( $($inner:tt)* )) + $a2:tt $a3:tt) => {};
    ($cb:tt $b3:tt # ( $($inner:tt)* ) (+) $a1:tt $a2:tt $a3:tt) => {};

    // #(...),* => cb!(RepSep1 , ...)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) $sep:tt +) => {
        $crate::match_call_cb!($cb RepSep1 $sep $($inner)*);
    };
    ($cb:tt $b3:tt $b2:tt # (( $($inner:tt)* )) $sep:tt + $a3:tt) => {};
    ($cb:tt $b3:tt # ( $($inner:tt)* ) ($sep:tt) + $a2:tt $a3:tt) => {};
    ($cb:tt # ( $($inner:tt)* ) $sep:tt (+) $a1:tt $a2:tt $a3:tt) => {};

    // #(...)? => cb!(Opt ...)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) ? $a3:tt) => {
        $crate::match_call_cb!($cb Opt $($inner)*);
    };
    ($cb:tt $b3:tt $b2:tt # (( $($inner:tt)* )) ? $a2:tt $a3:tt) => {};
    ($cb:tt $b3:tt # ( $($inner:tt)* ) (?) $a1:tt $a2:tt $a3:tt) => {};

    // #var:kind => cb!(Cap var : kind)
    ($cb:tt $b3:tt $b2:tt $b1:tt (#) $var:ident : $kind:ident) => {
        $crate::match_call_cb!($cb Cap $var : $kind);
    };
    ($cb:tt $b3:tt $b2:tt # ($var:ident) : $kind:ident $a3:tt) => {};
    ($cb:tt $b3:tt # $var:ident (:) $kind:ident $a2:tt $a3:tt) => {};
    ($cb:tt # $var:ident : ($kind:ident) $a1:tt $a2:tt $a3:tt) => {};

    // ?? => cb!(Tok ??)
    ($cb:tt $b3:tt $b2:tt $b1:tt ($curr:tt) $a1:tt $a2:tt $a3:tt) => {
        $crate::match_call_cb!($cb Tok $curr);
    };
}

// Call single-tt callback descriptor
//
// match_call_cb!([cb! ..extra] ..args) =>
//   cb!(..extra ..args)
#[macro_export]
#[doc(hidden)]
macro_rules! match_call_cb {
    ([$call:ident! $($extra:tt)*] $($arg:tt)*) => {
        $crate::$call!($($extra)* $($arg)*)
    }
}
