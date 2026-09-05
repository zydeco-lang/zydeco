//! Static resolution of second-class value-function definitions.
//!
//! Checking confines a value function to its definition and its applications,
//! so every application head reduces to a known abstraction by chasing
//! definition bindings, aliases, erased type binders, and erased type
//! arguments. Lowering unfolds applications with this resolution, and the
//! demand analysis flows caller demand into callee bodies through it.

use std::collections::HashMap;
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_syntax::{Abs, Alias};
use zydeco_utils::prelude::ArenaAccess;

/// Value-function definitions seen so far, mapped to the right-hand side
/// their applications unfold to. Entries follow program order, so a
/// right-hand side only chases definitions recorded before it.
pub(super) type Definitions = HashMap<ss::DefId, ss::ValueId>;

/// Whether the recorded classifier of one typed value is a `val pi`.
pub(super) fn is_value_function(statics: &StaticsArena, value: ss::ValueId) -> bool {
    statics
        .annotations_value
        .get(&value)
        .and_then(|ty| statics.normalized_at(*ty))
        .is_some_and(|ty| matches!(ty, ss::Type::ValPi(_)))
}

/// The next runtime binder of a value function, after chasing definitions,
/// aliases, erased type binders, and erased type arguments.
pub(super) fn next_abstraction(
    statics: &StaticsArena, definitions: &Definitions, node: ss::ValueId,
) -> Option<(ss::VPatId, ss::ValueId)> {
    match &statics.values[&node] {
        | ss::Value::ValAbs(Abs(ss::ValBinder::Value(param), body)) => Some((*param, *body)),
        | ss::Value::ValAbs(Abs(ss::ValBinder::Type(_), body)) => {
            next_abstraction(statics, definitions, *body)
        }
        | ss::Value::ValApp(ss::App(function, ss::ValArgument::Type(_))) => {
            next_abstraction(statics, definitions, *function)
        }
        | ss::Value::Var(def) => {
            definitions.get(def).and_then(|rhs| next_abstraction(statics, definitions, *rhs))
        }
        | _ => None,
    }
}

/// Split an application into its runtime arguments and its head, chasing
/// definitions so a bound partial application contributes its own arguments.
/// Arguments come back in application order.
pub(super) fn application_spine(
    statics: &StaticsArena, definitions: &Definitions, node: ss::ValueId,
) -> (Vec<ss::ValueId>, ss::ValueId) {
    let mut arguments = Vec::new();
    let mut head = node;
    loop {
        match &statics.values[&head] {
            | ss::Value::ValApp(ss::App(function, ss::ValArgument::Value(argument))) => {
                arguments.push(*argument);
                head = *function;
            }
            | ss::Value::Var(def) => match definitions.get(def) {
                | Some(rhs) => head = *rhs,
                | None => break,
            },
            | _ => break,
        }
    }
    arguments.reverse();
    (arguments, head)
}

/// Whether reducing one application spine against the abstractions under
/// `head` never gets stuck before the arguments are consumed.
pub(super) fn spine_reduces(
    statics: &StaticsArena, definitions: &Definitions, head: ss::ValueId, arguments: &[ss::ValueId],
) -> bool {
    let mut cursor = head;
    for _ in arguments {
        let Some((_, body)) = next_abstraction(statics, definitions, cursor) else {
            return false;
        };
        cursor = body;
    }
    true
}

/// Whether applications of `bindee` unfold to lexical bindings.
pub(super) fn is_unfoldable_definition(
    statics: &StaticsArena, definitions: &Definitions, bindee: ss::ValueId,
) -> bool {
    if next_abstraction(statics, definitions, bindee).is_some() {
        return true;
    }
    if matches!(&statics.values[&bindee], ss::Value::ValApp(ss::App(_, ss::ValArgument::Value(_))))
    {
        let (arguments, head) = application_spine(statics, definitions, bindee);
        return spine_reduces(statics, definitions, head, &arguments);
    }
    false
}

/// The definitions a pattern binds, when every one of them is a plain
/// variable or wildcard position.
pub(super) fn bound_definitions(
    statics: &StaticsArena, pattern: ss::VPatId,
) -> Option<Vec<ss::DefId>> {
    match statics.vpats[&pattern].clone() {
        | ss::ValuePattern::Var(def) => Some(vec![def]),
        | ss::ValuePattern::Hole(_) => Some(Vec::new()),
        | ss::ValuePattern::Alias(Alias(patterns)) => {
            let mut definitions = Vec::new();
            for pattern in patterns.iter() {
                definitions.extend(bound_definitions(statics, *pattern)?);
            }
            Some(definitions)
        }
        | _ => None,
    }
}

/// Record the value-function definitions bound by one binding, so later
/// applications resolve their heads through them.
pub(super) fn record_binding(
    statics: &StaticsArena, definitions: &mut Definitions, binder: ss::VPatId, bindee: ss::ValueId,
) {
    if !is_value_function(statics, bindee) {
        return;
    }
    if let Some(definitions_bound) = bound_definitions(statics, binder) {
        definitions_bound.into_iter().for_each(|def| {
            definitions.insert(def, bindee);
        });
    }
}

/// Statically reduce one application: peel the spine ending at `function`,
/// consume `outer_argument` after it, and pair every runtime argument with
/// the parameter pattern its cut binds. Returns those patterns in
/// application order together with the residual body.
pub(super) fn reduce_application(
    statics: &StaticsArena, definitions: &Definitions, function: ss::ValueId,
    outer_argument: &ss::ValArgument,
) -> Option<(Vec<ss::VPatId>, ss::ValueId)> {
    let (arguments, head) = application_spine(statics, definitions, function);
    let mut parameters = Vec::new();
    let mut cursor = head;
    for _ in arguments {
        let (param, body) = next_abstraction(statics, definitions, cursor)?;
        parameters.push(param);
        cursor = body;
    }
    if matches!(outer_argument, ss::ValArgument::Value(_)) {
        let (param, body) = next_abstraction(statics, definitions, cursor)?;
        parameters.push(param);
        cursor = body;
    }
    Some((parameters, cursor))
}
