/**
 * @file Zydeco grammar for Tree-sitter
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  binding: 1,
  quantifier: 2,
  pipeline_backward: 3,
  pipeline_forward: 4,
  arrow: 5,
  product: 6,
  application: 7,
  projection: 8,
};

const KEYWORDS = [
  'end',
  'begin',
  'data',
  'codata',
  'as',
  'def',
  'define',
  'let',
  'param',
  'val',
  'in',
  'that',
  'do',
  'ret',
  'fn',
  'pi',
  'fix',
  'match',
  'comatch',
  'forall',
  'exists',
  'sigma',
  'pack',
  'where',
  'is',
];

module.exports = grammar({
  name: 'zydeco',

  extras: $ => [
    /[\s\uFEFF\u2060\u200B]/,
    $.documentation_comment,
    $.line_comment,
    $.block_comment,
  ],

  externals: $ => [
    $.block_comment,
  ],

  word: $ => $.lower_identifier,

  supertypes: $ => [
    $._term,
    $._pattern,
    $._literal,
  ],

  rules: {
    // An empty source file is accepted so editor features remain available
    // before the user has entered the compiler's required root term.
    source_file: $ => optional(field('body', $._term)),

    _term: $ => choice(
      $._atomic_term,
      $.projection_expression,
      $.application_expression,
      $.forward_pipeline_expression,
      $.backward_pipeline_expression,
      $.destructor_expression,
      $.product_type,
      $.function_type,
      $.pi_type,
      $.value_pi_type,
      $.forall_type,
      $.sigma_type,
      $.existential_type,
      $.lambda_expression,
      $.value_lambda_expression,
      $.fix_expression,
      $.do_expression,
      $.parameter_expression,
      $.context_binding,
      $.metadata_expression,
    ),

    _atomic_term: $ => choice(
      $.parenthesized_term,
      $.hole,
      $.variable,
      $.thunk_expression,
      $.force_expression,
      $.return_expression,
      $.block_expression,
      $.block_abstraction,
      $.data_type,
      $.codata_type,
      $.constructor_expression,
      $.match_expression,
      $.comatch_expression,
      $.pack_expression,
      $._literal,
    ),

    _term_item: $ => choice(
      $._term,
      $.term_annotation,
      $.punned_term,
      $.named_term,
      $.labeled_term,
    ),

    parenthesized_term: $ => seq(
      '(',
      commaSep($._term_item),
      ')',
    ),

    hole: _ => '_',

    variable: $ => field('name', $._variable_name),

    thunk_expression: $ => seq(
      '{',
      field('body', $._term),
      '}',
    ),

    force_expression: $ => prec.right(PREC.projection, seq(
      '!',
      field('value', $._atomic_term),
    )),

    return_expression: $ => prec.right(PREC.projection, seq(
      'ret',
      field('value', $._atomic_term),
    )),

    block_expression: $ => seq(
      'begin',
      field('body', $._term_item),
      'end',
    ),

    block_abstraction: $ => seq(
      'comatch',
      field('parameters', $.copattern),
      '=>',
      field('body', $._term),
      'end',
    ),

    data_type: $ => seq(
      'data',
      repeat(field('arm', $.data_arm)),
      'end',
    ),

    data_arm: $ => seq(
      '|',
      field('name', $.constructor_identifier),
      ':',
      field('parameter', $._term),
    ),

    codata_type: $ => seq(
      'codata',
      repeat(field('arm', $.codata_arm)),
      'end',
    ),

    codata_arm: $ => seq(
      '|',
      field('name', $.destructor_identifier),
      optional(field('parameters', $.copattern)),
      ':',
      field('result', $._term),
    ),

    constructor_expression: $ => prec.right(PREC.projection, seq(
      field('constructor', $.constructor_identifier),
      field('argument', $._atomic_term),
    )),

    match_expression: $ => seq(
      'match',
      field('scrutinee', $._term),
      repeat(field('arm', $.match_arm)),
      'end',
    ),

    match_arm: $ => seq(
      '|',
      field('pattern', $._pattern),
      '=>',
      field('body', $._term),
    ),

    comatch_expression: $ => seq(
      'comatch',
      repeat(field('arm', $.comatch_arm)),
      'end',
    ),

    comatch_arm: $ => seq(
      '|',
      field('parameters', $.copattern),
      '=>',
      field('body', $._term),
    ),

    projection_expression: $ => prec.left(PREC.projection, seq(
      field('value', $._term),
      '/',
      field('field', $._variable_name),
    )),

    application_expression: $ => prec.left(PREC.application, seq(
      field('function', $._term),
      field('argument', $._atomic_term),
    )),

    destructor_expression: $ => prec.left(PREC.application, seq(
      field('value', $._term),
      field('destructor', $.destructor_identifier),
    )),

    forward_pipeline_expression: $ => prec.left(PREC.pipeline_forward, seq(
      field('subject', $._term),
      '|>',
      field('function', $._term),
    )),

    backward_pipeline_expression: $ => prec.right(PREC.pipeline_backward, seq(
      field('function', $._term),
      '<|',
      field('subject', $._term),
    )),

    product_type: $ => prec.right(PREC.product, seq(
      field('left', $._term),
      '*',
      field('right', $._term),
    )),

    function_type: $ => prec.right(PREC.arrow, seq(
      field('parameter', $._term),
      '->',
      field('result', $._term),
    )),

    pi_type: $ => prec.right(PREC.quantifier, seq(
      'pi',
      field('parameters', $.copattern),
      '.',
      field('body', $._term),
    )),

    value_pi_type: $ => prec.right(PREC.quantifier, seq(
      'val',
      'pi',
      field('parameters', $.copattern),
      '.',
      field('body', $._term),
    )),

    forall_type: $ => prec.right(PREC.quantifier, seq(
      'forall',
      field('parameters', $.copattern),
      '.',
      field('body', $._term),
    )),

    sigma_type: $ => prec.right(PREC.quantifier, seq(
      'sigma',
      field('parameters', $.copattern),
      '.',
      field('body', $._term),
    )),

    existential_type: $ => prec.right(PREC.quantifier, seq(
      'exists',
      repeat1(field('parameter', $.existential_parameter)),
      '.',
      field('body', $._term),
    )),

    existential_parameter: $ => seq(
      repeat(field('metadata', $.metadata)),
      '(',
      field('binder', $._pattern_item),
      optional(seq(
        'as',
        field('definition', $._term),
        optional(seq(
          ':',
          field('classifier', $._term),
        )),
      )),
      ')',
    ),

    // Package introduction; the payload is one nonempty comma sequence
    // between `where` and `end`, sharing tuple item syntax. A parameter
    // may seal its witness as atomic evidence after `is`.
    pack_expression: $ => seq(
      'pack',
      repeat1(field('parameter', $.pack_parameter)),
      'where',
      field('component', $._term_item),
      repeat(seq(',', field('component', $._term_item))),
      optional(','),
      'end',
    ),

    pack_parameter: $ => seq(
      field('parameter', $.existential_parameter),
      optional(seq(
        'is',
        field('evidence', $._atomic_term),
      )),
    ),

    lambda_expression: $ => prec.right(PREC.binding, seq(
      'fn',
      field('parameters', $.copattern),
      '=>',
      field('body', $._term),
    )),

    value_lambda_expression: $ => prec.right(PREC.binding, seq(
      'val',
      field('parameters', $.copattern),
      '=>',
      field('body', $._term),
    )),

    fix_expression: $ => prec.right(PREC.binding, seq(
      'fix',
      field('binder', $._pattern),
      '=>',
      field('body', $._term),
    )),

    do_expression: $ => prec.right(PREC.binding, seq(
      'do',
      field('binder', $._pattern),
      '<-',
      field('value', $._term),
      ';',
      field('body', $._term),
    )),

    parameter_expression: $ => prec.right(PREC.binding, seq(
      'param',
      field('binder', $._pattern_item),
      field('placement', $.placement),
      field('body', $._term),
    )),

    context_binding: $ => prec.right(PREC.binding, seq(
      field('kind', $.binding_kind),
      field('binding', $.general_binding),
      field('placement', $.placement),
      field('body', $._term),
    )),

    // Binding flavors are alternatives, not freely composable modifiers.
    // Value-function sugar requires a nonempty parameter telescope, matching
    // the compiler grammar; the other flavors retain optional parameters.
    general_binding: $ => choice(
      seq(
        field('value_function', $.value_modifier),
        generalBindingTail($, field('parameters', $.copattern)),
      ),
      seq(
        optional(choice(
          field('computation', $.computation_modifier),
          field('recursive', $.recursion_modifier),
        )),
        generalBindingTail($, optional(field('parameters', $.copattern))),
      ),
    ),

    binding_kind: _ => choice('let', 'def', 'define'),

    placement: _ => choice('in', 'that'),

    computation_modifier: _ => '!',

    value_modifier: _ => 'val',

    recursion_modifier: _ => 'fix',

    metadata_expression: $ => prec.right(PREC.binding, choice(
      seq(
        field('metadata', $.metadata),
        field('body', $._term),
      ),
      field('metadata', $.metadata_sugar),
    )),

    metadata: $ => seq(
      '@',
      '[',
      field('value', $._meta),
      ']',
    ),

    metadata_sugar: $ => seq(
      '@',
      '(',
      field('value', $._meta),
      ')',
    ),

    _meta: $ => choice(
      $.string_literal,
      $.meta_identifier,
      $.meta_application,
    ),

    meta_application: $ => seq(
      field('callee', $.meta_identifier),
      field('arguments', $.meta_arguments),
    ),

    meta_arguments: $ => seq(
      '(',
      commaSep($._meta),
      ')',
    ),

    meta_identifier: $ => choice(
      $._variable_name,
      alias(choice(...KEYWORDS), $.keyword_identifier),
    ),

    term_annotation: $ => prec.right(seq(
      field('term', $._term),
      ':',
      field('type', $._term),
    )),

    punned_term: $ => prec.right(seq(
      '=',
      field('name', $._variable_name),
      optional(seq(
        ':',
        field('type', $._term),
      )),
    )),

    named_term: $ => prec.right(seq(
      field('name', $.field_identifier),
      '=',
      field('value', $._term_item),
    )),

    labeled_term: $ => prec.right(seq(
      field('name', $.field_identifier),
      '::',
      field('value', $._term_item),
    )),

    _pattern: $ => choice(
      $.hole_pattern,
      $.variable_pattern,
      $.constructor_pattern,
      $.manifest_pattern,
      $.alias_pattern,
      $.parenthesized_pattern,
      $.view_pattern,
    ),

    _pattern_item: $ => choice(
      $._pattern,
      $.pattern_annotation,
      $.punned_pattern,
      $.named_pattern,
      $.punned_projection_pattern,
      $.projection_pattern,
    ),

    hole_pattern: _ => '_',

    variable_pattern: $ => field('name', $._variable_name),

    constructor_pattern: $ => prec.right(seq(
      field('constructor', $.constructor_identifier),
      field('argument', $._pattern),
    )),

    manifest_pattern: $ => seq(
      '(',
      field('binder', $._pattern_item),
      'as',
      field('definition', $._term),
      ')',
    ),

    alias_pattern: $ => seq(
      '(',
      field('member', $._pattern_item),
      repeat1(seq(
        ';',
        field('member', $._pattern_item),
      )),
      ')',
    ),

    view_pattern: $ => prec.right(seq(
      field('function', $._variable_name),
      optional(seq(
        '[',
        commaSep1(field('argument', $._term)),
        ']',
      )),
      '~>',
      field('pattern', $._pattern),
    )),

    parenthesized_pattern: $ => seq(
      '(',
      commaSep($._pattern_item),
      ')',
    ),

    pattern_annotation: $ => prec.right(seq(
      field('pattern', $._pattern),
      ':',
      field('type', $._term),
    )),

    punned_pattern: $ => prec.right(seq(
      '=',
      field('name', $._variable_name),
      optional(seq(
        ':',
        field('type', $._term),
      )),
    )),

    named_pattern: $ => prec.right(seq(
      field('name', $.field_identifier),
      '=',
      field('pattern', $._pattern_item),
    )),

    punned_projection_pattern: $ => prec.right(seq(
      '/',
      field('name', $._variable_name),
      optional(seq(
        ':',
        field('type', $._term),
      )),
    )),

    projection_pattern: $ => prec.right(seq(
      '/',
      field('name', $._variable_name),
      '=',
      field('pattern', $._pattern_item),
    )),

    copattern: $ => prec.left(repeat1($._copattern_atom)),

    _copattern_atom: $ => choice(
      $._pattern,
      $.destructor_identifier,
    ),

    _literal: $ => choice(
      $.float_literal,
      $.integer_literal,
      $.string_literal,
      $.character_literal,
    ),

    float_literal: _ => token(/[+-]?([0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)/),

    integer_literal: _ => token(/[+-]?[0-9]+/),

    string_literal: _ => token(seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        /\\./,
      )),
      '"',
    )),

    character_literal: _ => token(seq(
      "'",
      choice(
        /[ -~]/,
        seq('\\', /[nrt'\\]/),
      ),
      "'",
    )),

    _variable_name: $ => choice(
      $.upper_identifier,
      $.lower_identifier,
    ),

    upper_identifier: _ => token(/[A-Z][a-zA-Z0-9_'?+*\-=~]*/),

    lower_identifier: _ => token(choice(
      /[a-z][a-zA-Z0-9_'?+*\-=~]*/,
      /_[a-zA-Z0-9_'?+*\-=~]+/,
    )),

    constructor_identifier: _ => token(/\+[A-Z][a-zA-Z0-9_'?+*\-=~]*/),

    destructor_identifier: _ => token(/\.[a-z][a-zA-Z0-9_'?+*\-=~]*/),

    field_identifier: _ => token(choice(
      /#[A-Za-z][a-zA-Z0-9_'?+*\-=~]*/,
      /#_[a-zA-Z0-9_'?+*\-=~]+/,
    )),

    documentation_comment: _ => token(prec(2, seq('--|', /[^\n]*/))),

    line_comment: _ => token(prec(1, seq('--', /[^\n]*/))),
  },
});

/**
 * Parse a comma-separated sequence, including the empty sequence and an
 * optional trailing comma.
 *
 * @param {RuleOrLiteral} rule
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
  return optional(seq(rule, repeat(seq(',', rule)), optional(',')));
}

/**
 * Parse a nonempty comma-separated sequence with an optional trailing comma.
 *
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}

/**
 * Parse the portion shared by every general binding flavor.
 *
 * @param {GrammarSymbols<any>} $
 * @param {RuleOrLiteral} parameters
 * @returns {SeqRule}
 */
function generalBindingTail($, parameters) {
  return seq(
    field('binder', $._pattern),
    parameters,
    optional(seq(
      ':',
      field('classifier', $._term),
    )),
    '=',
    field('value', $._term),
  );
}
