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

unit SilSmLexCondition;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiLexDictionary;

type
  TSilLexCondition = class ( TSilObject, ILexCondition )
  protected // ILexCondition
    function Match( const AText: string; out ALen: integer ): boolean; virtual; abstract;
  end;

  TSilLexEmpty = class ( TSilLexCondition )
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  end;

  TSilLexAnything = class ( TSilLexCondition )
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  end;

  TSilLexCharInSet = class ( TSilLexCondition )
  private
    FSet: string;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create( const ASet: array of Char ); reintroduce; overload;
    constructor Create( const ASet: string ); reintroduce; overload;
  end;

  TSilLexCharBetween = class ( TSilLexCondition )
  private
    FFrom, FTo: Char;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create( AFrom, ATo: Char ); reintroduce;
  end;

  TSilLexMultiple = class ( TSilLexCondition, ILexMultipleCondition )
  private
    FChain: IInterfaceList;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  protected // ILexMultipleCondition
    procedure AddDef( const ACondition: ILexCondition );
  public
    constructor Create( const ASet: array of ILexCondition); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexAnyOf = class ( TSilLexCondition )
  private
    FList: IInterfaceList;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create( const ASet: array of ILexCondition); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexRefByName = class ( TSilLexCondition )
  private
    FList: Pointer;
    FRefName: string;
    //@ FCondition: ILexCondition;
  private 
    function GetList: ILexDefinitionList;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const AName: string; const AList: ILexDefinitionList); reintroduce;
    destructor Destroy; override; 
  public 
    property List: ILexDefinitionList read GetList;
  end;

  TSilLexLiteral = class ( TSilLexCondition )
  private
    FLiteral: string;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const ALiteral: string); reintroduce;
  end;

  TSilLexOptional = class ( TSilLexCondition )
  private
    FCondition: ILexCondition;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const ACondition: ILexCondition); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexRepetitions = class ( TSilLexCondition )
  private
    FCondition: ILexCondition;
    FMin, FMax: integer;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const ACondition: ILexCondition; Min, Max: integer); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexNegate = class ( TSilLexCondition )
  private
    FCondition: ILexCondition;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const ACondition: ILexCondition); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexConectedTo = class ( TSilLexCondition )
  private
    FCondition: ILexCondition;
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  public
    constructor Create(const ACondition: ILexCondition); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexNotConectedTo = class ( TSilLexConectedTo )
  protected
    function Match( const AText: string; out ALen: integer ): boolean; override;
  end;

implementation

uses
  SilSmLexMatches;
  
{ TSilLexCharInSet }

constructor TSilLexCharInSet.Create(const ASet: array of Char);
var
  i1: integer;
begin
  inherited Create;

  SetLength( FSet, 1 + High( ASet ) - Low( ASet ) );

  for i1 := Low( ASet ) to High( ASet ) do
    FSet[ 1 + i1 ] := ASet[ i1 ];
end;

constructor TSilLexCharInSet.Create(const ASet: string);
begin
  inherited Create;
  FSet := ASet;
end;

function TSilLexCharInSet.Match(const AText: string; out ALen: integer): boolean;
begin
  result := ( Length( AText ) > 0 ) and ( Sil.Str.Pos( AText[ 1 ], FSet ) > 0 );
  if result then ALen := 1 else ALen := 0;
end;

{ TSilLexCharBetween }

constructor TSilLexCharBetween.Create(AFrom, ATo: Char);
begin
  inherited Create;
  FFrom := AFrom;
  FTo := ATo;
end;

function TSilLexCharBetween.Match(const AText: string; out ALen: integer): boolean;
begin
  result := ( Length( AText ) > 0 ) and ( FFrom <= AText[ 1 ] ) and ( AText[ 1 ] <= FTo );
  if result then ALen := 1 else ALen := 0;
end;

{ TSilLexMultiple }

constructor TSilLexMultiple.Create(const ASet: array of ILexCondition);
var
  i1: integer;
begin
  inherited Create;

  for i1 := Low( ASet ) to High( ASet ) do
    AddDef( ASet[ i1 ] );
end;

destructor TSilLexMultiple.Destroy;
begin
  FChain := nil;
  inherited;
end;

procedure TSilLexMultiple.AddDef( const ACondition: ILexCondition );
begin
  if not Assigned( FChain ) then
    FChain := Sil.List.InterfaceList;

  FChain.Add( ACondition );
end;

function TSilLexMultiple.Match(const AText: string; out ALen: integer): boolean;
var
  i1, l1, lt: integer;
  s1: string;
begin
  i1 := 0;
  lt := 0;
  if Assigned( FChain ) then
  begin
    l1 := 0;
    s1 := AText;
    result := true;
    while result and ( i1 < FChain.Count ) do
    begin
      result := ILexCondition( FChain[ i1 ] ).Match( s1, l1 );
      if result then
      begin
        s1 := Sil.Str.Right( s1, Length( s1 ) - l1 );
        Inc( lt, l1 );
      end;
      Inc( i1 );
    end;
  end
  else
    result := false;

  if result then
    ALen := lt;
end;

{ TSilLexAnyOf }

constructor TSilLexAnyOf.Create(const ASet: array of ILexCondition);
var
  i1: integer;
begin
  inherited Create;
  FList := Sil.List.InterfaceList;
  for i1 := Low( ASet ) to High( ASet ) do
    FList.Add( ASet[ i1 ] );
end;

destructor TSilLexAnyOf.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilLexAnyOf.Match(const AText: string; out ALen: integer): boolean;
var
  i1, len, maxl, maxp: integer;
begin
  maxp := -1;
  maxl := 0;
  for i1 := 0 to FList.Count - 1 do
    if ILexCondition( FList[ i1 ] ).Match( AText, len ) then
      if ( maxp = -1 ) or ( len > maxl ) then
      begin
        maxp := i1;
        maxl := len;
      end;

  result := ( maxp >= 0 );
  ALen := maxl;
end;

{ TSilLexRefByName }

constructor TSilLexRefByName.Create(const AName: string; const AList: ILexDefinitionList);
begin
  inherited Create;
  FList := Pointer(AList);
  FRefName := AName;
end;

destructor TSilLexRefByName.Destroy;
begin
  //@ FCondition := nil;
  inherited;
end;

function TSilLexRefByName.GetList: ILexDefinitionList;
begin
  Result := ILexDefinitionList(FList);
end;

function TSilLexRefByName.Match(const AText: string; out ALen: integer): boolean;
var
  def: ILexDefinition;
  Condition: ILexCondition;
begin
  result := false;
  def := Self.List.DefByName[ FRefName ];
  if Assigned( def ) then
  try
    Condition := def.Condition;
    if Assigned( Condition ) then
    try
      result := Condition.Match( AText, ALen )
    finally
      Condition := nil;
    end;
  finally
    def := nil;
  end;
end;

{ TSilLexLiteral }

constructor TSilLexLiteral.Create(const ALiteral: string);
begin
  inherited Create;
  FLiteral := ALiteral;
end;

function TSilLexLiteral.Match(const AText: string; out ALen: integer): boolean;
begin
  result := ( Length( AText ) > 0 ) and
    ( Sil.Str.Compare( AText, FLiteral, Length( FLiteral ) ) = 0 );
  if result then
    ALen := Length( FLiteral ); 
end;

{ TSilLexOptional }

constructor TSilLexOptional.Create(const ACondition: ILexCondition);
begin
  inherited Create;
  FCondition := ACondition;
end;

destructor TSilLexOptional.Destroy;
begin
  FCondition := nil;
  inherited;
end;

function TSilLexOptional.Match(const AText: string; out ALen: integer): boolean;
begin
  if not FCondition.Match( AText, ALen ) then
    ALen := 0;

  result := true;
end;

{ TSilLexRepetitions }

constructor TSilLexRepetitions.Create(const ACondition: ILexCondition; Min,
  Max: integer);
begin
  inherited Create;
  FCondition := ACondition;
  FMin := Min;
  FMax := Max;
end;

destructor TSilLexRepetitions.Destroy;
begin
  FCondition := nil;
  inherited;
end;

function TSilLexRepetitions.Match(const AText: string; out ALen: integer): boolean;
var
  i1, l1, lt: integer;
  s1: string;
begin
  i1 := 0;
  l1 := 0;
  lt := 0;
  s1 := AText;
  while ( ( FMax = -1 ) or ( i1 < FMax ) ) and FCondition.Match( s1, l1 ) do
  begin
    Inc( lt, l1 );
    Inc( i1 );
    s1 := Sil.Str.Right( s1, Length( s1 ) - l1 );
  end;

  result := ( i1 >= FMin );
  if result then
    ALen := lt; 
end;

{ TSilLexNegate }

constructor TSilLexNegate.Create(const ACondition: ILexCondition);
begin
  inherited Create;
  FCondition := ACondition;
end;

destructor TSilLexNegate.Destroy;
begin
  FCondition := nil;
  inherited;
end;

function TSilLexNegate.Match(const AText: string; out ALen: integer): boolean;
begin
  result := ( Length( AText ) > 0 ) and not FCondition.Match( AText, ALen );
  if result then
    ALen := 1; 
end;

{ TSilLexEmpty }

function TSilLexEmpty.Match(const AText: string; out ALen: integer): boolean;
begin
  result := ( Length( AText ) = 0 );
  ALen := 0;
end;

{ TSilLexConectedTo }

constructor TSilLexConectedTo.Create(const ACondition: ILexCondition);
begin
  inherited Create;
  FCondition := ACondition;
end;

destructor TSilLexConectedTo.Destroy;
begin
  FCondition := nil;
  inherited;
end;

function TSilLexConectedTo.Match(const AText: string; out ALen: integer): boolean;
begin
  result := FCondition.Match( AText, ALen );
  ALen := 0;
end;

{ TSilLexNotConectedTo }

function TSilLexNotConectedTo.Match(const AText: string; out ALen: integer): boolean;
begin
  Result := not inherited Match(AText, ALen);
end;

{ TSilLexAnything }

function TSilLexAnything.Match(const AText: string; out ALen: integer): boolean;
begin
  result := true;
  ALen := Length( AText );
end;

end.

