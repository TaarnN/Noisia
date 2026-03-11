use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_item(&mut self) -> ParseResult<Item> {
        let attributes = self.parse_attributes();

        let mut visibility = None;
        if self.peek().token_type == TokenType::Keyword {
            let kw = self.peek().lexeme.as_str();
            if matches!(
                kw,
                "public" | "private" | "protected" | "internal" | "package"
            ) {
                visibility = Some(self.parse_visibility());
            }
        }

        if self.peek().token_type != TokenType::Keyword {
            if !attributes.is_empty()
                && visibility.is_none()
                && self.peek().token_type == TokenType::LeftBrace
            {
                let config = self.parse_named_record_literal("config")?;
                return Ok(Item::AttributeConfig(AttributeConfigDecl {
                    attributes,
                    config,
                }));
            }
            return Err(self.error_here(format!(
                "Expected keyword after attributes, found {}",
                self.peek()
            )));
        }

        let kw = self.peek().lexeme.clone();
        if visibility.is_some() && !self.is_function_start() {
            return Err(self.error_here(format!("Visibility modifier not allowed before {}", kw)));
        }

        if self.is_function_start() {
            let vis = visibility.unwrap_or(Visibility::Public);
            let func = self.parse_function_with(attributes, vis)?;
            return Ok(Item::Function(func));
        }

        match kw.as_str() {
            "module" => {
                self.advance();
                let path = self.parse_module_path_string()?;
                Ok(Item::ModuleDecl(ModuleDecl { attributes, path }))
            }
            "import" => {
                self.advance();
                let (path, symbols) = self.parse_import()?;
                Ok(Item::Import(ImportDecl {
                    attributes,
                    path,
                    symbols,
                }))
            }
            "using" => {
                self.advance();
                let target = self.parse_expression(0)?;
                Ok(Item::Using(UsingDecl { attributes, target }))
            }
            "macro" => {
                let m = self.parse_macro(attributes)?;
                Ok(Item::MacroDecl(m))
            }
            "extension" => {
                let e = self.parse_extension(attributes)?;
                Ok(Item::ExtensionDecl(e))
            }
            "igm" => {
                let i = self.parse_igm(attributes)?;
                Ok(Item::IGMDecl(i))
            }
            "plugin" => {
                let p = self.parse_plugin(attributes)?;
                Ok(Item::PluginDecl(p))
            }
            "struct" => {
                let s = self.parse_struct(attributes)?;
                Ok(Item::Struct(s))
            }
            "enum" => {
                let e = self.parse_enum(attributes)?;
                Ok(Item::Enum(e))
            }
            "mixin" => {
                let m = self.parse_mixin(attributes)?;
                Ok(Item::MixinDecl(m))
            }
            "trait" => {
                let t = self.parse_trait(attributes)?;
                Ok(Item::Trait(t))
            }
            "interface" => {
                let i = self.parse_interface(attributes)?;
                Ok(Item::InterfaceDecl(i))
            }
            "protocol" => {
                let p = self.parse_protocol(attributes)?;
                Ok(Item::ProtocolDecl(p))
            }
            "impl" => {
                let i = self.parse_impl(attributes)?;
                Ok(Item::Impl(i))
            }
            "class" => {
                let c = self.parse_class(attributes)?;
                Ok(Item::Class(c))
            }
            _ => Err(self.error_here(format!("Unsupported top-level keyword: {}", kw))),
        }
    }

    // note: read extends/with/implements
}
