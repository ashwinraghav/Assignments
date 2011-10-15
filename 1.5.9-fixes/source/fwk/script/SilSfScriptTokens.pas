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

unit SilSfScriptTokens;

interface

uses
  Sil,
  SilTokens,
  SilLexer;

 
function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary; overload;
function DefineTokens(const Tokens: ITokenTable): ITokenTable; overload;

implementation

uses
  SysUtils,
  SilSfEvalTokens,
  SilScScriptTokens, SilStTokenLexer, SilSiToken;

  
function DefineTokens(const Dictionary: ILexDictionary): ILexDictionary;
begin
  Result := SilSfEvalTokens.DefineTokens(Dictionary);
    (*)


  with Dictionary.Definitions, SilLexer.Condition do
  begin
    Add(TOKEN_STMT_BEGIN,             'begin',
        Literal('begin'));

    Add(TOKEN_STMT_END,               'end',
        Literal('end'));

    Add(TOKEN_STMT_IF,                'if',
        Literal('if'));

    Add(TOKEN_STMT_ELSE,              'else',
        Literal('else'));

    Add(TOKEN_STMT_FOR,               'for',
        Literal('for'));

    Add(TOKEN_STMT_WHILE,             'while',
        Literal('while'));

    Add(TOKEN_STMT_DO,                'do',
        Literal('do'));

    Add(TOKEN_STMT_REPEAT,            'repeat',
        Literal('repeat'));

    Add(TOKEN_STMT_UNTIL,             'until',
        Literal('until'));

    Add(TOKEN_STMT_SWITCH,            'switch',
        Literal('switch'));

    Add(TOKEN_STMT_CASE,              'case',
        Literal('case'));

    Add(TOKEN_STMT_BREAK,             'break',
        Literal('break'));

    Add(TOKEN_STMT_CONTINUE,          'continue',
        Literal('continue'));

    Add(TOKEN_STMT_RETURN,            'return',
        Literal('return'));
        
    Add(TOKEN_STMT_DEFAULT,           'default',
        Literal('default'));

    Add(TOKEN_TYPEID_ANY,             'type_any',
        Literal('any'));

    Add(TOKEN_TYPEID_VOID,            'type_void',
        Literal('void'));

    Add(TOKEN_TYPEID_BOOLEAN,         'type_boolean',
      AnyOf([
        Literal('boolean'),
        Literal('bool')
      ]));

    Add(TOKEN_TYPEID_CHAR,            'type_char',
        Literal('char'));

    Add(TOKEN_TYPEID_WIDECHAR,        'type_widechar',
        Literal('widechar'));

    Add(TOKEN_TYPEID_INTEGER,         'type_int',
      AnyOf([
        Literal('int'),
        Literal('integer')
      ]));

    Add(TOKEN_TYPEID_FLOAT,           'type_float',
      AnyOf([
        Literal('float'),
        Literal('real')
      ]));

    Add(TOKEN_TYPEID_SINGLE,          'type_single',
        Literal('single'));

    Add(TOKEN_TYPEID_DOUBLE,          'type_double',
        Literal('double'));

    Add(TOKEN_TYPEID_STRING,          'type_string',
        Literal('string'));

    Add(TOKEN_TYPEID_WIDESTRING,      'type_widestring',
        Literal('widestring'));

    Add(TOKEN_TYPEID_DATETIME,        'type_datetime',
        Literal('datetime'));

    Add(TOKEN_TYPEID,                 'type_id',
      AnyOf([
        CondByName['type_any'],
        CondByName['type_void'],
        CondByName['type_boolean'],
        CondByName['type_char'],
        CondByName['type_widechar'],
        CondByName['type_int'],
        CondByName['type_float'],
        CondByName['type_single'],
        CondByName['type_double'],
        CondByName['type_string'],
        CondByName['type_widestring'],
        CondByName['type_datetime']
      ]));
  end;
    (*)

end;


function DefineTokens(const Tokens: ITokenTable): ITokenTable;
begin
  if Assigned(Tokens) then
    Result := Tokens else
    Result := Tool.TokenTable();

  with Result do
  begin
    Add(TOKEN_STMT_BEGIN,         'begin',            'begin'       );
    Add(TOKEN_STMT_END,           'end',              'end'         );
    Add(TOKEN_STMT_IF,            'if',               'if'          );
    Add(TOKEN_STMT_ELSE,          'else',             'else'        );
    Add(TOKEN_STMT_FOR,           'for',              'for'         );
    Add(TOKEN_STMT_WHILE,         'while',            'while'       );
    Add(TOKEN_STMT_DO,            'do',               'do'          );
    Add(TOKEN_STMT_REPEAT,        'repeat',           'repeat'      );
    Add(TOKEN_STMT_UNTIL,         'until',            'until'       );
    Add(TOKEN_STMT_SWITCH,        'switch',           'switch'      );
    Add(TOKEN_STMT_CASE,          'case',             'case'        );
    Add(TOKEN_STMT_BREAK,         'break',            'break'       );
    Add(TOKEN_STMT_CONTINUE,      'continue',         'continue'    );
    Add(TOKEN_STMT_RETURN,        'return',           'return'      );
    Add(TOKEN_STMT_DEFAULT,       'default',          'default'     );
    Add(TOKEN_TYPEID_ANY,         'type_any',         'any'         );
    Add(TOKEN_TYPEID_VOID,        'type_void',        'void'        );
    Add(TOKEN_TYPEID_BOOLEAN,     'type_boolean',     'boolean'     );
    Add(TOKEN_TYPEID_BOOLEAN,     'type_boolean',     'bool'        );
    Add(TOKEN_TYPEID_CHAR,        'type_char',        'char'        );
    Add(TOKEN_TYPEID_WIDECHAR,    'type_widechar',    'widechar'    );
    Add(TOKEN_TYPEID_INTEGER,     'type_int',         'int'         );
    Add(TOKEN_TYPEID_INTEGER,     'type_int',         'integer'     );
    Add(TOKEN_TYPEID_FLOAT,       'type_float',       'float'       );
    Add(TOKEN_TYPEID_FLOAT,       'type_float',       'real'        );
    Add(TOKEN_TYPEID_SINGLE,      'type_single',      'single'      );
    Add(TOKEN_TYPEID_DOUBLE,      'type_double',      'double'      );
    Add(TOKEN_TYPEID_STRING,      'type_string',      'string'      );
    Add(TOKEN_TYPEID_WIDESTRING,  'type_widestring',  'widestring'  );
    Add(TOKEN_TYPEID_DATETIME,    'type_datetime',    'datetime'    );
  end;
end;


end.
