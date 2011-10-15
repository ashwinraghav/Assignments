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

unit SilStTokenLexer;

interface

uses
  Sil,
  SilTokens,
  SilSiLexDictionary,
  SilSiTokenLexer;

type
  LexerTool = class
    class function TokenLexer(const Stream: IStream; const Table: ITokenTable; const Dictionary: ILexDictionary): ITokenLexer; overload; 
    class function TokenLexer(const Text: String; const Table: ITokenTable; const Dictionary: ILexDictionary): ITokenLexer; overload; 
    class function TokenTable: ITokenTable;
  end;

implementation

uses
  SilSmTokenLexer,
  SilSmTokenTable;

{ LexerTool }

class function LexerTool.TokenLexer(const Stream: IStream; const Table: ITokenTable; const Dictionary: ILexDictionary): ITokenLexer;
begin
  Result := TSilTokenLexer.Create(Stream, Table, Dictionary);
end;

class function LexerTool.TokenLexer(const Text: String; const Table: ITokenTable; const Dictionary: ILexDictionary): ITokenLexer;
begin
  Result := TSilTokenLexer.Create(Sil.MemoryStream.Create(Text + ^Z), Table, Dictionary);
end;

class function LexerTool.TokenTable: ITokenTable;
begin
  Result := TSilTokenTable.Create;
end;

end.
