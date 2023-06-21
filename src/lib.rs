use convert_case::{Case, Casing};
use darling::FromDeriveInput;
use proc_macro::TokenStream;
use quote::quote;
use syn::{self, Data, DataEnum, Fields, FieldsNamed, Ident, Variant};

type TokenStream2 = proc_macro2::TokenStream;

#[derive(FromDeriveInput, Default)]
#[darling(attributes(handler))]
struct HandlerOpts {
    returns: Option<String>,
    trait_name: Option<String>,
    method: Option<String>,
}

impl HandlerOpts {
    fn get_returns(&self) -> TokenStream2 {
        str_to_tok(self.returns.as_ref().map_or("()", |s| s.as_str()))
    }

    fn get_trait_name(&self, ast: &syn::DeriveInput) -> TokenStream2 {
        if let Some(name) = &self.trait_name {
            return str_to_tok(name);
        }
        let name = &ast.ident;
        str_to_tok(&format!("{name}Handler"))
    }

    fn get_handler_method(&self, ast: &syn::DeriveInput) -> TokenStream2 {
        if let Some(method) = &self.method {
            return str_to_tok(method);
        }
        let name = ast.ident.to_string().to_case(Case::Snake);
        str_to_tok(&format!("handle_{name}"))
    }
}


fn str_to_tok(arg: &str) -> TokenStream2 {
    arg.parse().unwrap()
}

fn enum_variant_to_handle_ident(var: &Variant) -> Ident {
    let ident = &var.ident;
    let name = ident.to_string().to_case(Case::Snake);
    Ident::new(&format!("handle_{name}"), ident.span())
}

fn enum_variant_to_handle_arguments(var: &Variant) -> TokenStream2 {
    if let Fields::Named(fields) = &var.fields {
        return arguments_from_named_fields(fields);
    }
    quote! { &self, myself: ractor::ActorRef<<Self as ractor::Actor>::Msg>, state: &mut <Self as ractor::Actor>::State }
}

fn arguments_from_named_fields(fields: &FieldsNamed) -> TokenStream2 {
    let args = fields.named.iter().filter_map(|field| {
        let ident = field.ident.clone()?;
        let ty = &field.ty;
        Some(quote! {#ident: #ty})
    });
    quote! { &self, myself: ractor::ActorRef<<Self as ractor::Actor>::Msg>, state: &mut <Self as ractor::Actor>::State, #(#args),* }
}

fn get_field_name_list(fields: &Fields) -> TokenStream2 {
    match &fields {
        Fields::Named(fields) => get_named_fields_name_list(fields),
        _ => quote! { },
    }
}

fn get_named_fields_name_list(fields: &FieldsNamed) -> TokenStream2 {
    let names = get_idents_of_named_fields(fields);
    quote! { #(#names),* }
}

fn get_idents_of_named_fields(fields: &FieldsNamed) -> impl Iterator<Item = &Ident> + '_ {
    fields.named.iter().filter_map(|field| field.ident.as_ref())
}

struct ActorHandlerMacroGenerator {
    opts: HandlerOpts,
    ast: syn::DeriveInput,
}

impl ActorHandlerMacroGenerator {
    fn new(ast: syn::DeriveInput, opts: HandlerOpts) -> ActorHandlerMacroGenerator {
        ActorHandlerMacroGenerator { ast, opts }
    }

    fn get_data_enum(&self) -> &DataEnum {
        if let Data::Enum(data) = &self.ast.data {
            return data;
        }
        panic!("ActorHandler derive target must be an enum.");
    }

    fn generate(&self) -> TokenStream {
        let trait_name = self.opts.get_trait_name(&self.ast);
        let handles = self.get_handles();
        let handler_function = self.get_handler_function();
        quote! {
            #[async_trait::async_trait]
            trait #trait_name : ractor::Actor {
                #(#handles)*

                #handler_function
            }
        }
        .into()
    }

    fn get_handles(&self) -> impl Iterator<Item = TokenStream2> + '_ {
        self.get_data_enum()
            .variants
            .iter()
            .map(|var| self.enum_variant_to_handle(var))
    }

    fn enum_variant_to_handle(&self, var: &Variant) -> TokenStream2 {
        let ident = enum_variant_to_handle_ident(var);
        let arguments = enum_variant_to_handle_arguments(var);
        let returns = self.opts.get_returns();
        quote! { async fn #ident(#arguments) -> #returns; }
    }

    fn get_handler_function(&self) -> TokenStream2 {
        let handler_method = self.opts.get_handler_method(&self.ast);
        let enum_name = &self.ast.ident;
        let handler_arms = self.get_handler_arms();
        let returns = self.opts.get_returns();

        quote! {
            async fn #handler_method(&self, handled_enum: #enum_name, myself: ractor::ActorRef<<Self as ractor::Actor>::Msg>, state: &mut <Self as ractor::Actor>::State) -> #returns {
                match handled_enum {
                    #(#handler_arms)*
                }
            }
        }
    }

    fn get_handler_arms(&self) -> impl Iterator<Item = TokenStream2> + '_ {
        self.get_data_enum()
            .variants
            .iter()
            .map(|var| self.enum_variant_to_match_arm(var))
    }

    fn enum_variant_to_match_arm(&self, variant: &Variant) -> TokenStream2 {
        let enum_name = &self.ast.ident;
        let variant_name = &variant.ident;
        let variant_handle_name = enum_variant_to_handle_ident(variant);
        let field_name_list = get_field_name_list(&variant.fields);
        let arguments_list = quote! { myself, state, #field_name_list };

        quote! {
            #enum_name::#variant_name { #field_name_list } => {
                self.#variant_handle_name(#arguments_list).await
            }
        }
    }
}

#[proc_macro_derive(ActorHandler, attributes(handler))]
pub fn targets_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();
    let opts = HandlerOpts::from_derive_input(&ast).expect("Wrong options for 'handler'.");
    ActorHandlerMacroGenerator::new(ast, opts).generate()
}
