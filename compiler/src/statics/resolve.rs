#[derive(Clone, Debug)]
pub enum NameResolveError<Ann> {
    DuplicateDeclaration {
        name: String,
        ann: Ann,
    },
    UnknownIdentifier {
        name: String,
        ann: Ann,
    },
}