use super::Expr;

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt {
    Expr(Expr),
    Print(Expr),
}
