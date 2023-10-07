use term_ast::*;
use term_common::span::{SourceId, Span, Spanned};

use ustr::Ustr;

peg::parser! {
    pub grammar parser(source_id: SourceId) for str {
        rule whitespace() = quiet!{[' ' | '\t']}
        rule line_end() = quiet!{['\r' | '\n']}
        rule comment() = quiet!{"#" (!line_end() [_])*}
        rule eol() = quiet!{whitespace()* comment()? line_end()}

        rule _ = quiet!{whitespace()*}
        rule __ = quiet!{whitespace()+}
        rule ___ = quiet!{eol() _ / whitespace()+}

        rule int_base<T, U>(base: u32, prefix: rule<T>, digits: rule<U>) -> LitKind =
            p:prefix() d:$(digits() ("_"? d:digits(){d})*) {
                let v = u64::from_str_radix(&String::from(d).replace("_", ""), base).unwrap();
                LitKind::Int(v)
            }

        rule multiline<T>(r: rule<T>) -> Vec<T> =
            x:(_ "|" _ x:r() {x})
            xs:(!(eol()? _ ";") eol() _ "|" _ x:r() {x})* eol()? _ ";" {
                let mut v = vec![x];
                v.extend(xs);
                v
            }

        rule data_multiline<T>(r: rule<T>) -> Vec<T> =
            eol()? _ "|"? xs:((_ x:r() {x}) ++ (eol()? _ "|")) eol()? _ ";" {xs}

        rule node<T>(r: rule<T>) -> Node<T> =
            s:position!() k:r() e:position!() {
                Node::new(k, Span::new(source_id, s, e))
            }

        rule span<T: Spanned>(r: rule<T>) -> T =
            s:position!() v:r() e:position!() {
                v.with_span(Span::new(source_id, s, e))
            }

        rule params<T>(r: rule<T>) -> Vec<T> =
            ps:((_ p:r() {p}) ++ __) {ps}

        rule maybe_params<T>(r: rule<T>) -> Vec<T> =
            ps:((_ p:r() {p}) ** __) {ps}

        rule ustr<'a>(string: rule<&'a str>) -> Ustr = s:string() { Ustr::from(s) }

        // parses a keyword
        rule kw<T>(r: rule<T>) -> T = x:r() !ident_char_rest() {x}

        rule single_char() -> char =
            c:['\u{20}'..='\u{26}' | '\u{28}'..='\u{5B}' | '\u{5D}'..='\u{FFFF}'] { c }

        rule escaped_char() -> char =
            c:$['n' | 'r' | 't' | 'b' | 'e' | '\"' | '\"' | '\\'] {
                match c {
                    "n" => '\n',
                    "r" => '\r',
                    "t" => '\t',
                    "b" => '\u{8}',
                    "e" => '\u{1B}',
                    "\"" => '\"',
                    "'" => '\'',
                    "\\" => '\\',
                    _ => panic!("invalid escape sequence"),
                }
            }

        rule single_or_escaped_char() -> char =
            quiet!{single_char() / ("\\" c:escaped_char() {c})}

        rule ident_char_start() -> char =
            quiet!{['_'|'A'..='Z'|'a'..='z'|'\u{7F}'..='\u{FF}'|'\u{100}'..='\u{FFFF}']}

        rule ident_char_rest() -> char =
            quiet!{ident_char_start() / ['0'..='9' | '\'']}

        rule strict_ident_char_upper() -> char =
            quiet!{['A'..='Z']}

        rule strict_ident_char_lower() -> char =
            quiet!{['a'..='z']}

        rule strict_ident_char_rest() -> char =
            quiet!{['a'..='z' | 'A'..='Z']}

        rule ident_operator() -> &'input str =
            quiet!{
                $(['$'|'+'|'-'|'*'|'/'|'%']) /
                $("==" / "!=" / "<=" / ">=" / "<" / ">" /
                "&&" / "||" / "!")
            } /
            expected!("operator")

        rule bool_lit() -> LitKind = quiet!{
            kw(<"true">) { LitKind::Bool(true) } /
            kw(<"false">) { LitKind::Bool(false) }} /
            expected!("boolean")

        rule int_lit() -> LitKind = quiet!{
            v:int_base(2, <"0b">, <['0'|'1']>) {v} /
            v:int_base(8, <"0o">, <['0'..='7']>) {v} /
            v:int_base(10, <>, <['0'..='9']>) {v} /
            v:int_base(16, <"0x">, <['0'..='9'|'a'..='f'|'A'..='F']>) {v}} /
            expected!("integer")

        rule float_lit() -> LitKind = quiet!{
            d:$("." ['0'..='9']+) { LitKind::Float(d.parse::<f64>().unwrap()) } /
            d:$(['0'..='9']+ ".") { LitKind::Float(d.parse::<f64>().unwrap()) }} /
            expected!("float")

        rule char_lit() -> LitKind =
            quiet!{"'" c:single_or_escaped_char() "'" { LitKind::Char(c) }} /
            expected!("character")

        rule string_lit() -> LitKind =
            quiet!{"\"" s:$((!"\"" [_])* ) "\"" { LitKind::String(s.to_string()) }} /
            expected!("string")

        rule lit() -> Lit =
            node(<k:(bool_lit() / int_lit() / float_lit() / char_lit() / string_lit()) { k }>)

        rule ident() -> Ident = quiet!{
            "`" ident:span(<op:ident_operator() { Ident::from(op) }>) "`" {ident} /
            span(<n:$(ident_char_start() ident_char_rest()*) {?
                match n {
                    "case" | "class" | "data" | "default" | "effect" | "else" | "false" | "for" |
                    "handler" | "handle" | "if" | "instance" | "of" | "then" | "true" | "with" => Err("reserved keyword"),
                    _ => Ok(Ident::from(n)),
                }
            }>)} /
            expected!("identifier")

        rule ty_ident() -> Ident =
            span(<s:$(['a'..='z' | 'A'..='Z']+) { Ident::from(s) }>)

        rule ty_ident_upper() -> Ident =
            span(<s:$(strict_ident_char_upper() strict_ident_char_rest()*) { Ident::from(s) }>)

        rule ty_ident_lower() -> Ident =
            span(<s:$(strict_ident_char_lower()+) { Ident::from(s) }>)

        rule ty_arg_list() -> Vec<P<Ty>> = ts:(ty() ++ __) {ts}

        rule paren_ty_arg_list() -> Vec<P<Ty>> =
            "(" _ ts:ty_arg_list() _ ")" { ts } /
            t:ty() { vec![t] }

        rule ef_arg_list() -> Vec<P<Ty>> =
            ts:("'" _ ts:paren_ty_arg_list() {ts})? {ts.unwrap_or(vec![])}

        rule constraint() -> Constraint =
            node(<n:ty_ident() _ "'" _ ps:paren_ty_arg_list() { ConstraintKind::Class(n, ps) }>)

        rule constraints() -> Vec<Constraint> =
            "|" cs:((_ c:constraint() _ {c}) ** (_ "," _)) "|" {cs}

        #[cache_left_rec]
        pub rule ef() -> P<Ef> = precedence!{
            s:position!() k:@ e:position!() { Box::new(Ef::new(k, Span::new(source_id, s, e))) }
            --

            // union effect `a + b`
            a:@ _ "+" _ b:(@) { EfKind::Union(vec![a, b]) }
            // parenthesized effect `(a)`
            "(" e:ef() ")" { (*e).kind }

            kw(<"()">) { EfKind::Pure }
            kw(<"_">) { EfKind::Infer }
            n:ty_ident() ps:ef_arg_list() { EfKind::Name(n, ps) }
        }

        #[cache_left_rec]
        pub rule ty() -> P<Ty> = precedence!{
            s:position!() k:@ e:position!() { Box::new(Ty::new(k, Span::new(source_id, s, e))) }
            --
            // forall quantified type
            &((ty_ident() _)+ "|") ps:(p:ty_ident() _ {p})+ cs:constraints() _ t:ty() {
                TyKind::Forall(TyParams::new(ps, cs), t)
            }
            --

            // effectful type `a ~ e`
            a:ty() _ "~" _ e:ef() { TyKind::Effect(a, e) }
            --

            // parenthesized type `(a)`
            "(" t:ty() ")" { (*t).kind }
            // tuple type `(a, b, c)`
            "(" ts:((_ t:ty() _ {t}) ++ ",") ")" { TyKind::Tuple(ts) }
            // list type `[a]`
            "[" _ t:ty() _ "]" { TyKind::List(t) }
            // function type `a -> b`
            at:ty() _ "->" _ bt:ty() { TyKind::Func(at, bt) }

            "_" { TyKind::Infer }
            "never" { TyKind::Never }
            "Int" { TyKind::Int }
            "Float" { TyKind::Float }
            "Bool" { TyKind::Bool }
            "Char" { TyKind::Char }
            "String" { TyKind::String }
            "()" { TyKind::Unit }

            n:ty_ident() _ "'" _ ts:ty_arg_list() { TyKind::Inst(n, ts) }
            n:ty_ident() { TyKind::Name(n) }
        }

        #[cache_left_rec]
        pub rule pat() -> P<Pat> = precedence!{
            s:position!() k:@ e:position!() { Box::new(Pat::new(k, Span::new(source_id, s, e))) }
            --

            "(" _ ")" { PatKind::Unit }
            // parenthesized pattern `(a)`
            "(" p:pat() ")" { (*p).kind }
            // tuple pattern `(a, b, c)`
            "(" ps:((_ p:pat() _ {p}) ++ ",") ")" { PatKind::Tuple(ps) }
            // list pattern `[a, b, c]`
            "[" _ ps:((_ p:pat() _ {p}) ** ",") "]" { PatKind::List(ps) }
            // data constructor pattern `Cons a b`
            c:ty_ident_upper() !ident_char_start() ps:((_ p:pat() {p}) ++ __) { PatKind::DataCon(c, ps) }
            c:ty_ident_upper() !ident_char_start() { PatKind::DataCon(c, vec![]) }

            kw(<"_">) { PatKind::Wildcard }
            l:lit() { PatKind::Lit(l) }
            n:ident() { PatKind::Ident(n) }
        }

        rule case_alt() -> CaseAlt =
            span(<p:pat() _ "->" _ e:expr() { CaseAlt::new(p, e) }>)

        rule case() -> Case =
            span(<kw(<"case">) _ e:expr() eol()? alts:multiline(<case_alt()>) { Case::new(e, alts) }>)

        rule handle_alt() -> HandleAlt =
            span(<f:ef() _ "~>" _ e:expr() { HandleAlt::new(f, e) }>)

        rule handle() -> Handle = span(<
            kw(<"handle">) _ e:expr() eol()? alts:multiline(<handle_alt()>) {
                Handle::new(e, alts)
            }
        >)

        rule block() -> Do =
            span(<kw(<"do">) eol()? es:multiline(<expr()>) { Do::new(es) }>)

        rule if_else() -> If =
            span(<
                kw(<"if">) c:expr() __ kw(<"then">) t:expr() __
                kw(<"else">) f:expr() {
                    If::new(c, t, f)
                }
            >)

        rule param() -> P<Pat> =
            "(" _ p:pat()? _ ")" { p.unwrap_or(Pat::from(PatKind::Unit).into()) } /
            n:ident() { Pat::from(PatKind::Ident(n)).into() }

        #[cache_left_rec]
        pub rule expr() -> P<Expr> = precedence!{
            s:position!() n:@ e:position!() { Box::new(Expr::new(n, Span::new(source_id, s, e))) }
            --

            // case expression
            c:case() { ExprKind::Case(c.into()) }
            // handle expression
            h:handle() { ExprKind::Handle(h.into()) }
            // do expression
            d:block() { ExprKind::Do(d) }
            // if expression
            i:if_else() { ExprKind::If(i.into()) }
            // function expression
            n:ident() __ ps:params(<param()>) _ "=" _ e:expr() { ExprKind::Func(n, ps, e) }
            // variable expression
            p:pat() _ "=" _ e:expr() { ExprKind::Var(p, e) }
            // lambda expression
            ps:params(<param()>) _ "=>" _ e:expr() { ExprKind::Lambda(ps, e) }
            --

            // lowest priority right assoc. application
            x:@ _ "$" _ y:(@) { ExprKind::Apply(x, y) }
            --
            x:(@) _ "+" _ y:@ { ExprKind::Binary(BinOp::Add, x, y) }
            x:(@) _ "-" _ y:@ { ExprKind::Binary(BinOp::Sub, x, y) }
            --
            x:(@) _ "*" _ y:@ { ExprKind::Binary(BinOp::Mul, x, y) }
            x:(@) _ "/" _ y:@ { ExprKind::Binary(BinOp::Div, x, y) }
            --
            "+" x:(@) { ExprKind::Unary(UnOp::Pos, x) }
            "-" x:(@) { ExprKind::Unary(UnOp::Neg, x) }
            --
            // application (with space)
            x:(@) __ y:@ { ExprKind::Apply(x, y) }
            --
            // application with tuple/parenthesized
            x:(@) &("(") y:@ { ExprKind::Apply(x, y) }
            --

            // list expression
            "[" es:((_ e:expr() _ {e}) ** ",") "]" { ExprKind::List(es) }

            // record expression
            "{" fs:((_ n:ident() _ "=" _ e:expr() { (n, e) }) ** (eol()? "," eol()?)) "}" { ExprKind::Record(fs) }

            l:node(<"(" _ ")" {LitKind::Unit}>) { ExprKind::Lit(l) }
            l:lit() { ExprKind::Lit(l) }
            n:ident() { ExprKind::Ident(n) }
            "(" _ e:expr() _ ")" { e.kind }

            // tuple expression
            "(" es:((_ e:expr() _ {e}) ++ ",") ")" { ExprKind::Tuple(es) }
        }

        rule ty_params() -> TyParams = ps:span(<
            ":" _ ps:(ty_ident() ++ __) _ cs:constraints()? {
                TyParams::new(ps, cs.unwrap_or(vec![]))
            }
        >)? { ps.unwrap_or(TyParams::default()) }

        rule ty_args() -> Vec<P<Ty>> =
            ts:("'" _ ts:ty_arg_list() {ts})? {ts.unwrap_or(vec![])}

        rule data_con() -> DataConDecl = span(<
            n:ident() !ident_char_start() fs:((_ t:ty() {t}) ** __) {
                DataConDecl::new(n, fs)
            }
        >)

        pub rule data_decl() -> DataDecl = span(<
            kw(<"data">) __ n:ty_ident() _ ps:ty_params() _ "=" _ cs:data_multiline(<data_con()>) {
                DataDecl::new(n, ps, cs)
            }
        >)

        rule effect_op_decl() -> EffectOpDecl =
            span(<n:ident() _ ":" _ t:ty() { EffectOpDecl::new(n, t) }>)

        pub rule effect_decl() -> EffectDecl = span(<
            kw(<"effect">) _ n:ty_ident() _ ps:ty_params() _ eol()
            ops:multiline(<effect_op_decl()>) {
                EffectDecl::new(n, ps, ops)
            }
        >)

        rule effect_op_impl() -> EffectOpImpl = span(<
            n:ident() __ ps:maybe_params(<param()>) _ "=" _ e:expr() {
                EffectOpImpl::new(n, ps, e)
            }
        >)

        pub rule effect_handler() -> EffectHandler = span(<
            d:kw(<"default">)? _ kw(<"handler">) __ n:ident() __ kw(<"for">) __ ef:ty_ident() _ ts:ty_args()
            _ ps:ty_params() eol() ops:multiline(<effect_op_impl()>) {
                let gs = TyArgs::new(ts, ps.params, ps.constraints);
                EffectHandler::new(n, ef, gs, ops, d.is_some())
            }
        >)

        rule associated_ty_decl() -> Ident =
            span(<kw(<"type">) __ n:ty_ident() { n }>)

        rule method_decl() -> MethodDecl = span(<
            n:ident() _ ":" _ ps:ty_params() _ t:ty() { MethodDecl::new(n, ps, t) }
        >)

        rule member_decl() -> MemberDecl = node(<
            c:associated_ty_decl() { MemberDeclKind::AssocTy(c) } /
            m:method_decl() { MemberDeclKind::Method(m) }
        >)

        pub rule class_decl() -> ClassDecl = span(<
            kw(<"class">) __ n:ty_ident() _ ps:ty_params() eol()
            ms:multiline(<member_decl()>) {
                ClassDecl::new(n, ps, ms)
            }
        >)

        rule method_impl() -> MethodImpl =
            span(<n:ident() __ ps:maybe_params(<param()>) _ "=" _ e:expr() {
                MethodImpl::new(n, ps, e)
            }>)

        rule member_impl() -> MemberImpl = node(<
            m:method_impl() { MemberImplKind::Method(m) }
        >)

        pub rule class_inst() -> ClassInst = span(<
            kw(<"instance">) __ n:ty_ident() _ ts:ty_args() _ ps:ty_params() eol()
            ms:multiline(<member_impl()>) {
                let gs = TyArgs::new(ts, ps.params, ps.constraints);
                ClassInst::new(n, gs, ms)
            }
        >)

        pub rule var_decl() -> VarDecl =
            span(<n:ident() _ ":" _ t:ty() { VarDecl::new(n, t) }>)

        rule command() -> (Ustr, String) =
            ":" cmd:$(['a'..='z'|'A'..='Z'|'_']+) rest:(__ x:$(![_]) {x}) {
                (Ustr::from(cmd), rest.to_owned())
            }

        pub rule item() -> Item = node(<
            c:command() { ItemKind::Command(c.0, c.1) } /
            d:data_decl() { ItemKind::DataDecl(d.into()) } /
            e:effect_decl() { ItemKind::EffectDecl(e.into()) } /
            h:effect_handler() { ItemKind::EffectHandler(h.into()) } /
            c:class_decl() { ItemKind::ClassDecl(c.into()) } /
            i:class_inst() { ItemKind::ClassInst(i.into()) } /
            v:var_decl() { ItemKind::VarDecl(Left(v.into())) } /
            e:expr() { ItemKind::Expr(e) }
        >)

        rule items() -> Vec<Item> =
            _ i:item()? _ comment()? ![_] { i.map(|i| vec![i]).unwrap_or(vec![]) } /
            _ i:item()? eol() is:items() {
                let mut v = is;
                if let Some(i) = i {
                    v.insert(0, i)
                }
                v
            }

        pub rule module() -> Module =
            items:items() { Module { items } }
    }
}
