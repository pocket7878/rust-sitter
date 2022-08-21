use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameValueExpr {
    pub path: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
}

impl Parse for NameValueExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(NameValueExpr {
            path: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldThenParams {
    pub field: Field,
    pub comma: Option<Token![,]>,
    pub params: Punctuated<NameValueExpr, Token![,]>,
}

impl Parse for FieldThenParams {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let field = Field::parse_unnamed(input)?;
        let comma: Option<Token![,]> = input.parse()?;
        let params = if comma.is_some() {
            input.parse_terminated(NameValueExpr::parse)?
        } else {
            Punctuated::new()
        };

        Ok(FieldThenParams {
            field,
            comma,
            params,
        })
    }
}

pub fn try_extract_inner_type(ty: &Type, inner_of: &str) -> (Type, bool) {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.first().unwrap();
        if type_segment.ident == inner_of {
            let leaf_type = if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    t
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            };

            (leaf_type, true)
        } else {
            (ty.clone(), false)
        }
    } else {
        (ty.clone(), false)
    }
}

pub fn try_extract_inner_type_from_smart_pointer_type(typ: &Type) -> (Type, syn::Expr) {
    let smart_pointer_type_names = vec!["Box", "Rc", "Arc", "RefCell", "Cell"];
    let smart_pointer_type_quotes = vec![
        quote::quote!(Box),
        quote::quote!(Rc),
        quote::quote!(Arc),
        quote::quote!(RefCell),
        quote::quote!(Cell),
    ];

    let mut wrapper_types = vec![];
    let mut typ = typ.clone();

    loop {
        let mut inner_type = None;
        let mut wrapper_type_idx = None;
        for (i, typ_name) in smart_pointer_type_names.iter().enumerate() {
            let (wrapped_type, wrapped) = try_extract_inner_type(&typ, typ_name);
            if wrapped {
                wrapper_type_idx = Some(i);
                inner_type = Some(wrapped_type);
            }
        }

        if inner_type.is_some() {
            typ = inner_type.unwrap();
            let wrapper_typ = smart_pointer_type_quotes[wrapper_type_idx.unwrap()].clone();
            dbg!(wrapper_typ.clone());
            wrapper_types.push(wrapper_typ);
        } else {
            break;
        }
    }

    let mut wrapped_type = typ.clone();
    let mut wrapper_fn = syn::parse_quote!((|x: #wrapped_type| { x })(x));
    for wrapper_typ in wrapper_types.iter().rev() {
        wrapper_fn = syn::parse_quote! {
            (|x: #wrapped_type| {
                #wrapper_typ::new(x)
            })(#wrapper_fn)
        };
        wrapped_type = syn::parse_quote!(#wrapper_typ<#wrapped_type>);
    }
    wrapper_fn = syn::parse_quote! {
        (|x: #typ| { #wrapper_fn })
    };

    return (typ, wrapper_fn);
}
