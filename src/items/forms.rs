use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, HyphenSymbol, OpenParenSymbol,
};

#[derive(Debug, Clone)]
pub enum Form {
    Define(DefineDirective),
}

#[derive(Debug, Clone)]
pub struct DefineDirective {
    hyphen: HyphenSymbol,
    //define: AttrNameDefine,
    open: OpenParenSymbol,
    //macro_name: MacroName,
    //variables: Maybe<Parenthesized<Items<VariableToken, NeedRightSpace<Comma>>>>,
    comma: CommaSymbol,
    //replacement: Replacement,
    close: CloseParenSymbol,
    dot: DotSymbol,
}
