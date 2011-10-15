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

unit SilSfHttpTokens;

interface

uses
  Sil,
  SilTokens,
  SilLexer;

function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary;

implementation

uses
  SilScHttpTokens;

function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary;
var
  Definitions: ILexDefinitionList;
begin
  Definitions := Dictionary.Definitions;

  Definitions.Add(TOKEN_OCTET,            'OCTET',
    SilLexer.Condition.CharBetween(#0, #255));

  Definitions.Add(TOKEN_CHAR,             'CHAR',
    SilLexer.Condition.CharBetween(#0, #127));

  Definitions.Add(TOKEN_UPALPHA,          'UPPER',
    SilLexer.Condition.CharBetween('A', 'Z'));

  Definitions.Add(TOKEN_LOALPHA,          'LOWER',
    SilLexer.Condition.CharBetween('a', 'z'));

  Definitions.Add(TOKEN_ALPHA,            'ALPHA',
    SilLexer.Condition.AnyOf([
      Definitions.CondByName['UPPER'],
      Definitions.CondByName['LOWER']]));

  Definitions.Add(TOKEN_DIGIT,            'DIGIT',
    SilLexer.Condition.CharBetween('0', '9'));

  Definitions.Add(TOKEN_1DIGIT,           '1DIGIT',
    SilLexer.Condition.Repetitions(Definitions.CondByName['DIGIT'], 1, 1));

  Definitions.Add(TOKEN_2DIGIT,           '2DIGIT',
    SilLexer.Condition.Repetitions(Definitions.CondByName['DIGIT'], 1, 2));

  Definitions.Add(TOKEN_3DIGIT,           '3DIGIT',
    SilLexer.Condition.Repetitions(Definitions.CondByName['DIGIT'], 1, 3));

  Definitions.Add(TOKEN_4DIGIT,           '4DIGIT',
    SilLexer.Condition.Repetitions(Definitions.CondByName['DIGIT'], 1, 4));

  Definitions.Add(TOKEN_DIGITS,           'DIGITS',
    SilLexer.Condition.Repetitions(
      Definitions.CondByName['DIGIT']
      ));

  Definitions.Add(TOKEN_CTL,              'CTL',
    SilLexer.Condition.AnyOf([
      SilLexer.Condition.CharBetween(#0, #31),
      SilLexer.Condition.CharInSet([#127])]));

  Definitions.Add(TOKEN_CR,               'CR',
    SilLexer.Condition.CharInSet([#13]));

  Definitions.Add(TOKEN_LF,               'LF',
    SilLexer.Condition.CharInSet([#10]));

  Definitions.Add(TOKEN_SP,               'SP',
    SilLexer.Condition.CharInSet([#32]));

  Definitions.Add(TOKEN_HT,               'HT',
    SilLexer.Condition.CharInSet([#9]));

  Definitions.Add(TOKEN_DQ,               'DQ',
    SilLexer.Condition.CharInSet(['"']));

  Definitions.Add(TOKEN_CRLF,             'CRLF',
    SilLexer.Condition.Multiple([
      Definitions.CondByName['CR'],
      Definitions.CondByName['LF']]));

  Definitions.Add(TOKEN_LWS,              'LWS',
    SilLexer.Condition.Multiple([
      SilLexer.Condition.Optional(Definitions.CondByName['CRLF']),
      SilLexer.Condition.Repetitions(
        SilLexer.Condition.AnyOf([
          Definitions.CondByName['SP'],
          Definitions.CondByName['HT']]))
      ]));

  Definitions.Add(TOKEN_HEX,              'HEX',
    SilLexer.Condition.AnyOf([
      Definitions.CondByName['DIGIT'],
      SilLexer.Condition.AnyOf([
        SilLexer.Condition.CharBetween('a', 'f'),
        SilLexer.Condition.CharBetween('A', 'F')
        ])
      ]));

  Definitions.Add(TOKEN_SEP_LPAREN,       'LPAREN',
    SilLexer.Condition.Literal('('));
      
  Definitions.Add(TOKEN_SEP_RPAREN,       'RPAREN',
    SilLexer.Condition.Literal(')'));
      
  Definitions.Add(TOKEN_SEP_MINOR,        'MINOR',
    SilLexer.Condition.Literal('<'));
      
  Definitions.Add(TOKEN_SEP_MAJOR,        'MAJOR',
    SilLexer.Condition.Literal('>'));
      
  Definitions.Add(TOKEN_SEP_AT,           'AT',
    SilLexer.Condition.Literal('@'));
      
  Definitions.Add(TOKEN_SEP_COMMA,        'COMMA',
    SilLexer.Condition.Literal(','));
      
  Definitions.Add(TOKEN_SEP_SEMICOLON,    'SEMICOLON',
    SilLexer.Condition.Literal(';'));
      
  Definitions.Add(TOKEN_SEP_COLON,        'COLON',
    SilLexer.Condition.Literal(':'));
      
  Definitions.Add(TOKEN_SEP_BACKSLASH,    'BACKSLASH',
    SilLexer.Condition.Literal('\'));
      
  Definitions.Add(TOKEN_SEP_SLASH,        'SLASH',
    SilLexer.Condition.Literal('/'));
      
  Definitions.Add(TOKEN_SEP_LBRACKET,     'LBRACKET',
    SilLexer.Condition.Literal('['));
      
  Definitions.Add(TOKEN_SEP_RBRACKET,     'RBRACKET',
    SilLexer.Condition.Literal(']'));
      
  Definitions.Add(TOKEN_SEP_QUESTION,     'QUESTION',
    SilLexer.Condition.Literal('?'));
      
  Definitions.Add(TOKEN_SEP_EQUAL,        'EQUAL',
    SilLexer.Condition.Literal('='));
      
  Definitions.Add(TOKEN_SEP_LCURLY,       'LCURLY',
    SilLexer.Condition.Literal('{'));
      
  Definitions.Add(TOKEN_SEP_RCURLY,       'RCURLY',
    SilLexer.Condition.Literal('}'));
      
  Definitions.Add(TOKEN_SEP_PERIOD,       'PERIOD',
    SilLexer.Condition.Literal('.'));
      
  Definitions.Add(TOKEN_SEP_STAR,         'STAR',
    SilLexer.Condition.Literal('.'));
      
  Definitions.Add(TOKEN_SEPARATORS,       'SEPARATORS',
    SilLexer.Condition.AnyOf([
      Definitions.CondByName['LPAREN'],
      Definitions.CondByName['RPAREN'],
      Definitions.CondByName['MINOR'],
      Definitions.CondByName['MAJOR'],
      Definitions.CondByName['AT'],
      Definitions.CondByName['COMMA'],
      Definitions.CondByName['SEMICOLON'],
      Definitions.CondByName['COLON'],
      Definitions.CondByName['BACKSLASH'],
      Definitions.CondByName['SLASH'],
      Definitions.CondByName['LBRACKET'],
      Definitions.CondByName['RBRACKET'],
      Definitions.CondByName['QUESTION'],
      Definitions.CondByName['EQUAL'],
      Definitions.CondByName['LCURLY'],
      Definitions.CondByName['RCURLY'],
      Definitions.CondByName['PERIOD'],
      Definitions.CondByName['STAR'],
      Definitions.CondByName['SP'],
      Definitions.CondByName['HT'],
      Definitions.CondByName['DQ']
      ]));

  Definitions.Add(TOKEN_TOKEN,            'TOKEN',
    SilLexer.Condition.Repetitions(
      SilLexer.Condition.Multiple([
        SilLexer.Condition.NotConectedTo(
          SilLexer.Condition.AnyOf([
            Definitions.CondByName['CTL'],
            Definitions.CondByName['SEPARATORS']
              ])),
        Definitions.CondByName['CHAR']
        ])));

  Definitions.Add(TOKEN_TEXT,             'TEXT',
    SilLexer.Condition.AnyOf([
      SilLexer.Condition.Multiple([
        SilLexer.Condition.ConectedTo(Definitions.CondByName['LWS']),
        SilLexer.Condition.NotConectedTo(Definitions.CondByName['CTL']),
        Definitions.CondByName['OCTET']
        ]),
      Definitions.CondByName['LWS']
      ]));

  Definitions.Add(TOKEN_QUOTED_PAIR,      'QUOTED_PAIR',
    SilLexer.Condition.Multiple([
      SilLexer.Condition.Literal('\'),
      Definitions.CondByName['CHAR']
      ]));

  Definitions.Add(TOKEN_CTEXT,            'CTEXT',
    SilLexer.Condition.Negate(SilLexer.Condition.CharInSet('()')));
      
  Definitions.Add(TOKEN_COMMENT,          'COMMENT',
    SilLexer.Condition.Multiple([
      SilLexer.Condition.Literal('('),
      SilLexer.Condition.Repetitions(
        SilLexer.Condition.AnyOf([
          Definitions.CondByName['CTEXT'],
          Definitions.CondByName['QUOTED_PAIR'],
          SilLexer.Condition.RefByName('COMMENT', Dictionary.Definitions)
          ])),
      SilLexer.Condition.Literal(')')]));
        
  Definitions.Add(TOKEN_QDTEXT,           'QDTEXT',
    SilLexer.Condition.Negate(SilLexer.Condition.Literal('"')));

  Definitions.Add(TOKEN_QUOTED_STRING,    'QUOTED_STRING',
    SilLexer.Condition.Multiple([
      SilLexer.Condition.Literal('"'),
      SilLexer.Condition.Repetitions(
        SilLexer.Condition.AnyOf([
          Definitions.CondByName['QDTEXT'],
          Definitions.CondByName['QUOTED_PAIR']
          ])),
      SilLexer.Condition.Literal('"')]));
  
  Result := Dictionary;
end;


end.