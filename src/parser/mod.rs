use crate::syntax::abstraction::Abstraction;
use std::ops::Range;

use crate::syntax::pattern::Pattern;
use crate::syntax::term::Infix;
use crate::syntax::term::Prefix;
use crate::syntax::term::Term;
use indexmap::IndexMap;
use pest::pratt_parser::Assoc;
use pest::pratt_parser::Op;
use pest::Parser as _;
use pest::{iterators::Pair, pratt_parser::PrattParser};
use pest_derive::Parser;
use std::sync::Arc;

use crate::syntax::{
    program::{Declaration, Program},
    r#type::Type,
};

#[derive(Clone, PartialEq, Default, Debug)]
pub struct Span {
    pub source: String,
    pub range: Range<usize>,
}

impl Span {
    pub fn new(source: &str, range: Range<usize>) -> Self {
        Self {
            source: source.to_owned(),
            range,
        }
    }

    pub fn from_pest(source: &str, span: pest::Span) -> Self {
        Self::new(source, span.start()..span.end())
    }

    pub fn join(&self, other: &Self) -> Option<Self> {
        let start = self.range.start;
        let end = self.range.end;
        (self.source == other.source && start <= end).then_some(Self::new(&self.source, start..end))
    }
}

impl ariadne::Span for Span {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.range.start
    }

    fn end(&self) -> usize {
        self.range.end
    }
}

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct Parser {
    source_name: String,
    type_parser: PrattParser<Rule>,
    expr_parser: PrattParser<Rule>,
}

impl Parser {
    pub fn new(source_name: String) -> Self {
        Parser {
            source_name,
            ..Default::default()
        }
    }

    pub fn parse_source(&self, source: &str) -> anyhow::Result<Program> {
        let declarations = Parser::parse(Rule::program, source)?.next().unwrap();
        let declarations = declarations
            .into_inner()
            .map(|pair| self.parse_declaration(pair))
            .collect();
        Ok(Program(declarations))
    }

    fn parse_declaration(&self, pair: Pair<Rule>) -> Declaration {
        match pair.as_rule() {
            Rule::let_decl | Rule::let_box_decl => {
                let mut inner_rules = pair.into_inner();
                let pattern = self.parse_pattern(inner_rules.next().unwrap());
                let term = self.parse_term(inner_rules.next().unwrap());
                Declaration::Term(pattern, term)
            }
            Rule::type_decl => {
                let mut inner_rules = pair.into_inner();
                let name = inner_rules.next().unwrap().as_str().to_string();
                let r#type = self.parse_type(inner_rules.next().unwrap());
                Declaration::Type(name, r#type)
            }
            _ => unreachable!("{pair}"),
        }
    }

    fn parse_type(&self, pair: Pair<Rule>) -> Type {
        self.type_parser
            .map_primary(|pair| {
                let span = self.span_from(&pair);
                match pair.as_rule() {
                    Rule::tuple_type => {
                        let inner_rules = pair.into_inner();
                        let mut types = inner_rules
                            .map(|pair| self.parse_type(pair))
                            .collect::<Vec<_>>();
                        if types.len() == 1 {
                            types.remove(0)
                        } else {
                            Type::Tuple(span, types)
                        }
                    }
                    Rule::variant_type => {
                        let inner_rules = pair.into_inner();
                        let variants: IndexMap<String, Type> = inner_rules
                            .map(|pair| {
                                let mut inner_rules = pair.into_inner();
                                let label = inner_rules.next().unwrap().as_str().to_string();
                                let variant_type = self.parse_type(inner_rules.next().unwrap());
                                (label, variant_type)
                            })
                            .collect();
                        Type::Variant(span, variants)
                    }
                    Rule::ident => Type::Variable(span, pair.as_str().to_string()),
                    _ => unreachable!("{pair}"),
                }
            })
            .map_prefix(|op, right| {
                let span = right.span().join(&self.span_from(&op)).unwrap();

                match op.as_rule() {
                    Rule::box_type => Type::Modal(span, Box::new(right)),
                    _ => unreachable!("{op}"),
                }
            })
            .map_infix(|left, op, right| {
                let span = left.span().join(&self.span_from(&op)).unwrap();

                match op.as_rule() {
                    Rule::type_arrow => Type::Abstraction(span, Box::new(left), Box::new(right)),
                    _ => unreachable!(),
                }
            })
            .parse(pair.into_inner())
    }

    fn parse_term(&self, pair: Pair<Rule>) -> Term {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::abs_term => {
                let mut inner_rules = pair.into_inner();
                let param = self.parse_pattern(inner_rules.next().unwrap());
                let param_type = self.parse_type(inner_rules.next().unwrap());
                let body = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Abstraction(
                    Abstraction {
                        param,
                        param_type,
                        body,
                    },
                    span,
                )
            }
            Rule::box_term => {
                let mut inner_rules = pair.into_inner();
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Box(value, span)
            }
            Rule::fix_term => {
                let mut inner_rules = pair.into_inner();
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Fix(value, span)
            }
            Rule::if_term => {
                let mut inner_rules = pair.into_inner();
                let condition = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                let consequent = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                let alternative = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::If(condition, consequent, alternative, span)
            }
            Rule::let_term => {
                let mut inner_rules = pair.into_inner();
                let pattern = self.parse_pattern(inner_rules.next().unwrap());
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                let body = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Let(pattern, value, body, span)
            }
            Rule::let_box_term => {
                let mut inner_rules = pair.into_inner();
                let pattern = self.parse_pattern(inner_rules.next().unwrap());
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                let body = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::LetBox(pattern, value, body, span)
            }
            Rule::match_term => {
                let mut inner_rules = pair.into_inner();
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                let arms = inner_rules.map(|pair| self.parse_match_arm(pair)).collect();
                Term::Match(value, arms, span)
            }
            Rule::mfix_term => {
                let mut inner_rules = pair.into_inner();
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::MFix(value, span)
            }
            Rule::expr_term => self.parse_expr(pair),
            Rule::unit_term => Term::Unit(span),
            _ => unreachable!(),
        }
    }

    fn parse_match_arm(&self, pair: Pair<Rule>) -> (String, (Pattern, Term)) {
        let mut inner_rules = pair.into_inner();
        let label = inner_rules.next().unwrap().as_str().to_owned();
        let pattern = self.parse_pattern(inner_rules.next().unwrap());
        let body = self.parse_term(inner_rules.next().unwrap());
        (label, (pattern, body))
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> Term {
        self.expr_parser
            .map_primary(|pair| {
                let inner_rules = pair.into_inner();
                inner_rules
                    .map(|pair| {
                        let mut inner_rules = pair.into_inner();
                        let primary = self.parse_primary(inner_rules.next().unwrap());

                        inner_rules.fold(primary, |term, pair| match pair.as_rule() {
                            Rule::nat => {
                                let span = self.span_from(&pair);
                                let index = pair.as_str().parse().unwrap();
                                Term::TupleProjection(Arc::new(term), index, span)
                            }
                            _ => unreachable!(),
                        })
                    })
                    .reduce(|f, a| {
                        let span = f.span().join(a.span()).unwrap();
                        Term::Application(Arc::new(f), Arc::new(a), span)
                    })
                    .unwrap()
            })
            .map_prefix(|op, right| {
                let span = self.span_from(&op).join(right.span()).unwrap();

                match op.as_rule() {
                    Rule::neg_op => Term::Prefix(Prefix::Neg, Arc::new(right), span),
                    Rule::not_op => Term::Prefix(Prefix::Not, Arc::new(right), span),
                    _ => unreachable!(),
                }
            })
            .map_postfix(|left, op| {
                let span = left.span().join(&self.span_from(&op)).unwrap();

                match op.as_rule() {
                    Rule::r#as => {
                        let as_type = self.parse_type(op.into_inner().next().unwrap());
                        Term::Ascription(Arc::new(left), as_type, span)
                    }
                    _ => unreachable!(),
                }
            })
            .map_infix(|left, op, right| {
                let span = left
                    .span()
                    .join(&self.span_from(&op))
                    .unwrap()
                    .join(right.span())
                    .unwrap();

                Term::Infix(
                    Arc::new(left),
                    match op.as_rule() {
                        Rule::or_op => Infix::Or,
                        Rule::and_op => Infix::And,
                        Rule::eq_op => Infix::Eq,
                        Rule::ne_op => Infix::NtEq,
                        Rule::ge_op => Infix::GtEq,
                        Rule::gt_op => Infix::Gt,
                        Rule::le_op => Infix::LtEq,
                        Rule::lt_op => Infix::Lt,
                        Rule::add_op => Infix::Add,
                        Rule::sub_op => Infix::Sub,
                        Rule::mul_op => Infix::Mul,
                        Rule::div_op => Infix::Div,
                        _ => unreachable!(),
                    },
                    Arc::new(right),
                    span,
                )
            })
            .parse(pair.into_inner())
    }

    fn parse_primary(&self, pair: Pair<Rule>) -> Term {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::tuple_term => {
                let inner_rules = pair.into_inner();
                let mut values = inner_rules
                    .map(|pair| self.parse_term(pair))
                    .collect::<Vec<_>>();
                if values.len() == 1 {
                    values.remove(0)
                } else {
                    Term::Tuple(values, span)
                }
            }
            Rule::variant_term => {
                let mut inner_rules = pair.into_inner();
                let label = inner_rules.next().unwrap().as_str().to_string();
                let value = Arc::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Variant(label, value, span)
            }
            Rule::bool => match pair.as_str() {
                "true" => Term::Bool(true, span),
                "false" => Term::Bool(false, span),
                _ => unreachable!(),
            },
            Rule::ident => Term::Variable(pair.as_str().into(), span),
            Rule::nat => Term::Int(pair.as_str().parse().unwrap(), span),

            _ => unreachable!(),
        }
    }

    fn parse_pattern(&self, pair: Pair<Rule>) -> Pattern {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::tuple_pat => Pattern::Tuple(
                span,
                pair.into_inner()
                    .map(|pair| self.parse_pattern(pair))
                    .collect(),
            ),
            Rule::wild_pat => Pattern::Wildcard(span),
            Rule::ident => {
                let name = pair.as_str().to_owned();
                Pattern::Variable(span, name)
            }
            _ => unreachable!(),
        }
    }

    fn span_from(&self, pair: &Pair<Rule>) -> Span {
        Span::from_pest(&self.source_name, pair.as_span())
    }
}

impl Default for Parser {
    fn default() -> Self {
        let type_parser = PrattParser::new()
            .op(Op::infix(Rule::type_arrow, Assoc::Right))
            .op(Op::prefix(Rule::box_type));
        let expr_parser = PrattParser::new()
            .op(Op::infix(Rule::or_op, Assoc::Left))
            .op(Op::infix(Rule::and_op, Assoc::Left))
            .op(Op::infix(Rule::eq_op, Assoc::Left) | Op::infix(Rule::ne_op, Assoc::Left))
            .op(Op::infix(Rule::ge_op, Assoc::Left)
                | Op::infix(Rule::gt_op, Assoc::Left)
                | Op::infix(Rule::le_op, Assoc::Left)
                | Op::infix(Rule::lt_op, Assoc::Left))
            .op(Op::infix(Rule::add_op, Assoc::Left) | Op::infix(Rule::sub_op, Assoc::Left))
            .op(Op::infix(Rule::mul_op, Assoc::Left) | Op::infix(Rule::div_op, Assoc::Left))
            .op(Op::postfix(Rule::r#as))
            .op(Op::prefix(Rule::neg_op) | Op::prefix(Rule::not_op));
        Self {
            source_name: "".to_owned(),
            type_parser,
            expr_parser,
        }
    }
}
