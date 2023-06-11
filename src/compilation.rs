use crate::ast::TopLevelElement;

struct GlobalEnvironment {
    pub globals: Vec<TopLevelElement>,
}