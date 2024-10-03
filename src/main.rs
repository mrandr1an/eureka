mod frontend;
use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("daaamn!")]
#[diagnostic(code(wow::wow::bad), help("Skill issue you dumb fucking idiot"))]
struct ParserVariableError {
    #[source_code]
    src: NamedSource<String>,
    #[label("This bit here")]
    bad: SourceSpan,
}

fn main() -> std::result::Result<(), miette::Report> {
    fails()
}

fn fails() -> Result<()> {
    let src = "This is a very interesting function".to_string();
    let len = src.len();

    Err(ParserVariableError {
        src: NamedSource::new("what", src),
        bad: (3, 5).into(),
    })?;

    Ok(())
}
