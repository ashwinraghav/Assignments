{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSfEvalTokens;

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSiEvaluator;

function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary;

implementation

uses
  SysUtils,
  SilScEvalTokens, SilSmEvalOperators;

function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary;
begin
  with Dictionary.Definitions, SilLexer.Condition do
  begin
    Add(TOKEN_UNKNOWN,        'unknown',
      Empty );
    
    Add(TOKEN_EOF,            'eof',
      CharInSet([^Z]));

    Add(TOKEN_BLANKS,           'blanks',
      Repetitions(
        CharInSet([' ', #13, #10, #9])));

    Add(TOKEN_ALPHA,           'alpha',
      AnyOf([
        Literal('_'),
        CharBetween('a', 'z'),
        CharBetween('A', 'Z')
      ]));

    Add(TOKEN_DIGIT,           'digit',
      CharBetween( '0', '9' ) );

    Add(TOKEN_BIT,             'bit',
      CharBetween( '0', '1' ) );

    Add(TOKEN_OCT,             'oct',
      CharBetween( '0', '7' ) );

    Add(TOKEN_SQ,              'sq',
      Literal( '''' ) );

    Add(TOKEN_DQ,              'dq',
      Literal( '"' ) );

    Add(TOKEN_HEXDIGIT,        'hexdigit',
      AnyOf([
        CondByName['digit'],
        AnyOf([
          CharBetween('a', 'f'),
          CharBetween('A', 'F')
        ])
      ]));

    Add(TOKEN_ALPHANUM,        'alphanum',
      AnyOf([
        CondByName['alpha'],
        CondByName['digit']
      ]));

    Add(TOKEN_STRING,          'string',
      AnyOf([
        Multiple([
          CondByName['sq'],
          Repetitions(
            AnyOf([
              Negate(CondByName['sq']),
              Multiple([
                CondByName['sq'],
                CondByName['sq']
              ])
            ]), 0),
            CondByName['sq']
          ]),
        Multiple([
          CondByName['dq'],
          Repetitions(
            AnyOf([
              Negate(CondByName['dq']),
              Multiple([
                CondByName['dq'],
                CondByName['dq']
              ])
            ]), 0),
            CondByName['dq']
          ])
        ]));

    Add(TOKEN_DIGITS,         'digits',
      Repetitions(
        CondByName['digit']));

    Add(TOKEN_DECIMAL,         'decimal',
      Multiple([
        CondByName['digits'],
        Optional(CharInSet('dD'))
      ]));

    Add(TOKEN_SIGN,            'sign',
      CharInSet('+-'));

    Add(TOKEN_HEXA,            'hexa',
      AnyOf([
        Multiple([
          AnyOf([
            Literal('$'),
            Literal('0x')
            ]),
          Repetitions(
            CondByName['hexdigit']
            )]),
        Multiple([
          Repetitions(
            CondByName['hexdigit']),
          CharInSet('hH')
          ])
        ]));

    Add(TOKEN_BINARY,          'binary',
      Multiple([
        Repetitions(CondByName['bit']),
        CharInSet('bB')
      ]));

    Add(TOKEN_OCTAL,           'octal',
      AnyOf([
        Multiple([
          Literal('0'),
          Repetitions(CondByName['oct'])
        ]),
        Multiple([
          Repetitions(CondByName['oct']),
          CharInSet('oO')
        ])
      ]));

    Add(TOKEN_MANTISSA,         'mantissa',
      Multiple([
        Literal('.'),
        CondByName['digits']
      ]));

    Add(TOKEN_FIXED,            'fixed',
      Multiple([
        Optional(CondByName['sign']),
        CondByName['digits'],
        CondByName['mantissa']
      ]));

    Add(TOKEN_EXPONENT,         'exponent',
      Multiple([
        CharInSet('eE'),
        CondByName['fixed']
      ]));

    Add(TOKEN_FLOAT,            'float',
      Multiple([
        CondByName['fixed'],
        Optional(CondByName['exponent'])
      ]));

    Add(TOKEN_OP_LOR,               'op_lor',
      Literal('||'));

    Add(TOKEN_OP_LAND,              'op_land',
      Literal('&&'));

    Add(TOKEN_OP_OR,                'op_or',
      Literal('|'));

    Add(TOKEN_OP_XOR,               'op_xor',
      Literal('^'));

    Add(TOKEN_OP_AND,               'op_and',
      Literal('&'));

    Add(TOKEN_OP_EQ,                'op_eq',
      Literal('=='));

    Add(TOKEN_OP_NEQ,               'op_neq',
      AnyOf([
        Literal('!='),
        Literal('<>')
      ]));

    Add(TOKEN_OP_LESS,              'op_less',
      Literal('<'));

    Add(TOKEN_OP_GREATER,           'op_greater',
      Literal('>'));

    Add(TOKEN_OP_LESSEQ,            'op_lesseq',
      Literal('<='));

    Add(TOKEN_OP_GREATEREQ,         'op_greatereq',
      Literal('>='));

    Add(TOKEN_OP_SHR,               'op_shr',
      Literal('>>'));

    Add(TOKEN_OP_SHL,               'op_shl',
      Literal('<<'));

    Add(TOKEN_PLUS,                 'plus',
      Literal('+'));

    Add(TOKEN_MINUS,                'minus',
      Literal('-'));

    Add(TOKEN_MUL,                  'mul',
      Literal('*'));

    Add(TOKEN_DIV,                  'div',
      Literal('/'));

    Add(TOKEN_MOD,                  'mod',
      Literal('%'));

    Add(TOKEN_OP_NEG,                  'neg',
      Literal('~'));

    Add(TOKEN_OP_NOT,                  'not',
      Literal('!'));

    Add(TOKEN_OP_INC,                  'inc',
      Literal('++'));

    Add(TOKEN_OP_DEC,                  'dec',
      Literal('--'));

    Add(TOKEN_ASSIGN,               'assign',
      AnyOf([
        Literal(':='),
        Literal('=')
      ]));

    Add(TOKEN_ASSIGN_MUL,           'assign_mul'  ,
      Literal('*='));

    Add(TOKEN_ASSIGN_DIV,           'assign_div'  ,
      Literal('/='));

    Add(TOKEN_ASSIGN_MOD,           'assign_mod'  ,
      Literal('%='));

    Add(TOKEN_ASSIGN_PLUS,          'assign_plus' ,
      Literal('+='));

    Add(TOKEN_ASSIGN_MINUS,         'assign_minus',
      Literal('-='));

    Add(TOKEN_ASSIGN_SHR,           'assign_shr'  ,
      Literal('>>='));

    Add(TOKEN_ASSIGN_SHL,           'assign_shl'  ,
      Literal('<<='));

    Add(TOKEN_ASSIGN_AND,           'assign_and'  ,
      Literal('&='));

    Add(TOKEN_ASSIGN_XOR,           'assign_xor'  ,
      Literal('^='));

    Add(TOKEN_ASSIGN_OR,            'assign_or'   ,
      Literal('|='));

    Add(TOKEN_ARROW_OP,             'arrow',
      Literal('->'));

    Add(TOKEN_DOT_OP,               'dot',
      Literal('.'));

    Add(TOKEN_DOLLAR,               'dollar',
      Literal('$'));

    Add(TOKEN_LPAREN,               'lparen',
      Literal('('));

    Add(TOKEN_RPAREN,               'rparen',
      Literal(')'));

    Add(TOKEN_SEMICOLON,            'semicolon',
      Literal(';'));

    Add(TOKEN_COLON,                'colon',
      Literal(':'));

    Add(TOKEN_COMMA,                'comma',
      Literal(','));

    Add(TOKEN_LCURLY,               'lcurly',
      Literal('{'));

    Add(TOKEN_RCURLY,               'rcurly',
      Literal('}'));

    Add(TOKEN_LBRACKET,             'lbracket',
      Literal('['));

    Add(TOKEN_RBRACKET,             'rbracket',
      Literal(']'));

    Add(TOKEN_AT,                   'at',
      Literal('@'));

    Add(TOKEN_QUESTION,             'question',
      Literal('?'));

    Add(TOKEN_ASSIGN_OP,            'assign_op',
      AnyOf([
        CondByName['assign'],
        CondByName['assign_mul'  ],
        CondByName['assign_div'  ],
        CondByName['assign_mod'  ],
        CondByName['assign_plus' ],
        CondByName['assign_minus'],
        CondByName['assign_shr'  ],
        CondByName['assign_shl'  ],
        CondByName['assign_and'  ],
        CondByName['assign_xor'  ],
        CondByName['assign_or'   ]
      ]));


    Add(TOKEN_SUM_OP,               'sum_op',
      AnyOf([
        CondByName['plus'],
        CondByName['minus']
      ]));

    Add(TOKEN_MULT_OP,              'mult_op',
      AnyOf([
        CondByName['mul'],
        CondByName['div'],
        CondByName['mod']
      ]));

    Add(TOKEN_EQUALITY_OP,          'eq_op',
      AnyOf([
        CondByName['op_eq'],
        CondByName['op_neq']
      ]));

    Add(TOKEN_RELATIONAL_OP,               'rel_op',
      AnyOf([
        CondByName['op_less'],
        CondByName['op_greater'],
        CondByName['op_lesseq'],
        CondByName['op_greatereq']
      ]));

    Add(TOKEN_SHIFT_OP,                'sh_op',
      AnyOf([ // one of  (op_shr, shl-op)
        CondByName['op_shr'],
        CondByName['op_shl']
      ]));

    Add(TOKEN_INCR_OP,              'incr_op',
      AnyOf([ // one of  (inc, dec)
        CondByName['inc'],
        CondByName['dec']
      ]));

    Add(TOKEN_UNARY_OP,             'unary_op',
      AnyOf([ // one of  ( *, &, not, neg)
        CondByName['not'],
        CondByName['neg']
      ]));

    Add(TOKEN_BOOLEAN,         'boolean',
      AnyOf([
        Literal('True'),
        Literal('true'),
        Literal('False'),
        Literal('false')
      ]));

    Add(TOKEN_LITERAL,              'literal',
      AnyOf([
        CondByName['boolean'],
        CondByName['float'],
        CondByName['string'],
        CondByName['decimal'],
        CondByName['hexa'],
        CondByName['octal'],
        CondByName['binary']
        ]));

    Add(TOKEN_IDENTIFIER,      'identifier',
      Multiple([
        Optional(
          AnyOf([
            Literal('@'),
            Literal(':'),
            Literal('%'),
            Literal('&'),
            Literal('$')
          ])),
        NotConectedTo(CondByName['literal']),
        CondByName['alpha'],
        Repetitions(CondByName['alphanum'], 0)
      ]));

  end;
  
  Result := Dictionary;
end;

end.
