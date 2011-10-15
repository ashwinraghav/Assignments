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

unit SilSmToken;

interface

uses
  Sil,
  SilSeTokens,
  SilSiToken;

type
  TSilToken = class(
    TSilObject,
    IReferenceable,
    IToken )
  private
    FIndex: Integer;
    FName: string;
    FSymbol: TToken;
    FLinked: TSilToken;
    FLexema: string;
    FData: IParameterList;
    FValue: Variant;
  protected // IToken
    function GetIndex: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetLinked: IToken;
    procedure SetLinked(const Value: IToken);
    function GetSymbol: TToken;
    procedure SetSymbol(const Value: TToken);
    function GetLexema: string;
    function GetData: IParameterList;
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
  public
    constructor Create(Symbol: TToken; const Name: string; const Lexema: string = ''; const Value: Integer = 0); overload;
    constructor Create(Symbol: TToken; const Name: string; const Lexema: string = ''; const Value: Double = 0.0); overload;
    constructor Create(Symbol: TToken; const Name: string; const Lexema: string = ''; const Value: string = ''); overload;
    constructor Create(Symbol: TToken; const Name: string; const Lexema: string; const Value: Variant {$IFNDEF D60} ; Dummy: Boolean {$ENDIF}); overload;
    destructor Destroy; override;
  public
    property Index: Integer read FIndex write FIndex;
    property Symbol: TToken read GetSymbol write SetSymbol;
    property Name: string read GetName write SetName;
    property Linked: IToken read GetLinked write SetLinked;
    property Lexema: string read GetLexema;
    property Data: IParameterList read GetData;
    property Value: Variant read GetValue write SetValue;
  end;

implementation

{ TSilToken }

constructor TSilToken.Create(Symbol: TToken; const Name: string; const Lexema: string; const Value: Double);
begin
  Create(Symbol, Name, Lexema, Variant(Value) {$IFNDEF D60} , true {$ENDIF})
end;

constructor TSilToken.Create(Symbol: TToken; const Name: string; const Lexema: string; const Value: Integer);
begin
  Create(Symbol, Name, Lexema, Variant(Value) {$IFNDEF D60} , true {$ENDIF})
end;

constructor TSilToken.Create(Symbol: TToken; const Name: string; const Lexema, Value: string);
begin
  Create(Symbol, Name, Lexema, Variant(Value) {$IFNDEF D60} , true {$ENDIF})
end;

constructor TSilToken.Create(Symbol: TToken; const Name: string; const Lexema: string; const Value: Variant {$IFNDEF D60} ; Dummy: Boolean {$ENDIF});
begin
  inherited Create;
  FData := Sil.List.Parameters();
  FName := Name;
  FSymbol := Symbol;
  FLexema := Lexema;
  FValue := Value;
end;

destructor TSilToken.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilToken.GetData: IParameterList;
begin
  Result := FData;
end;

function TSilToken.GetLexema: string;
begin
  Result := FLexema;
end;

function TSilToken.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TSilToken.GetName: string;
begin
  Result := FName;
end;

procedure TSilToken.SetName(const Value: string);
begin
  FName := Value;
end;

function TSilToken.GetLinked: IToken;
begin
  if Assigned(FLinked) then
    Result := FLinked else
    Result := Self;
end;

procedure TSilToken.SetLinked(const Value: IToken);
begin
  if Assigned(FLinked) then FLinked.FLinked := nil;
  FLinked := Sil.Ref.GetInstance(Value);
  if Assigned(FLinked) then FLinked.FLinked := Self;
end;

function TSilToken.GetSymbol: TToken;
begin
  Result := FSymbol;
end;

procedure TSilToken.SetSymbol(const Value: TToken);
begin
  FSymbol := Value;
end;

function TSilToken.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilToken.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

end.
 