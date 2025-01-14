use super::Expr;

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt {
    Expr(Expr),
    Print(Expr),
}

impl Stmt {
    pub fn eval(&self) -> miette::Result<()> {
        match self {
            Stmt::Expr(expr) => expr.evalute().map(|_| ()),
            Stmt::Print(expr) => {
                println!("{}", expr.evalute()?);
                Ok(())
            }
        }
    }

    pub fn execute(stmts: &[Stmt]) -> miette::Result<()> {
        for stmt in stmts {
            stmt.eval()?;
        }

        Ok(())
    }
}
