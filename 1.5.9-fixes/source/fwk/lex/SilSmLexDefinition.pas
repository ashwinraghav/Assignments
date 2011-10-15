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

unit SilSmLexDefinition;

{$I Defines.inc}

interface

uses
  Sil,
  SilSeTokens, 
  SilSiLexDictionary;

type
  TSilLexDefinitionList = class(
    TSilObject,
    ILexDefinitionList )
  private
    FVisibleList: IInterfaceList;
    FInvisibleList: IInterfaceList;
  protected
    function DoGetDefByName(const AList: IInterfaceList; const AName: string; out ADef: ILexDefinition): boolean; 
  protected // ILexDefinitionList
    function GetDefByName( const AName: string ): ILexDefinition;
    function FindDefByName( const AName: string; out Def: ILexDefinition ): Boolean;   
    function DefExists( const AName: string ): Boolean; 
    function GetCondByName( const AName: string ): ILexCondition;
    procedure Add( const ADef: ILexDefinition; AVisible: boolean = true ); overload;
    procedure Add( const Symbol: TToken; const AName: string; const ACondition: ILexCondition; const Data: Pointer = nil; AVisible: boolean = true  ); overload;
    procedure Clear;
    function GetMatches( const AText: string; out AMatchList: ILexMatches; Widers: Boolean = False ): boolean;
  public // properties
    destructor Destroy; override; 
    property DefByName[ const AName: string ]: ILexDefinition read GetDefByName;
    property CondByName[ const AName: string ]: ILexCondition read GetCondByName;
  end;

  TSilLexDefinition = class(
    TSilObject,
    ILexDefinition )
  private
    FSymbol: TToken;
    FName: string;
    FCondition: ILexCondition;
    FData: Pointer;
  protected // ILexDefinition
    function GetSymbol: TToken;
    function GetName: string;
    function GetCondition: ILexCondition;
    procedure SetCondition( const Value: ILexCondition );
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
  public
    constructor Create( const Symbol: TToken; const AName: string; const ACondition: ILexCondition; const Data: Pointer ); reintroduce;
    destructor Destroy; override;     
  public
    property Symbol: TToken read GetSymbol;
    property Name: string read GetName;
    property Condition: ILexCondition read GetCondition write SetCondition;
    property Data: Pointer read GetData write SetData;
  end;

implementation

uses
  SilSmLexMatches;

{ TSilLexDefinitionList }

destructor TSilLexDefinitionList.Destroy;
begin
  FVisibleList := nil;
  FInvisibleList := nil;
  inherited;
end;

procedure TSilLexDefinitionList.Add(const Symbol: TToken; const AName: string; const ACondition: ILexCondition; const Data: Pointer; AVisible: boolean);
begin
  Add(TSilLexDefinition.Create(Symbol, AName, ACondition, Data), AVisible);
end;

procedure TSilLexDefinitionList.Add(const ADef: ILexDefinition; AVisible: boolean);
begin
  if DefExists( ADef.Name ) then
    raise Sil.Error.Create( 'Item duplicado: %s', [ADef.Name] );

  if AVisible then
  begin
    if not Assigned( FVisibleList ) then
      FVisibleList := Sil.List.InterfaceList;
    FVisibleList.Add( ADef );
  end
  else if not AVisible then
  begin
    if not Assigned( FInvisibleList ) then
      FInvisibleList := Sil.List.InterfaceList;
    FInvisibleList.Add( ADef );
  end;
end;

procedure TSilLexDefinitionList.Clear;
begin
  FVisibleList.Clear;
  FInvisibleList.Clear;
end;

function TSilLexDefinitionList.GetCondByName(const AName: string): ILexCondition;
var
  def: ILexDefinition;
begin
  def := DefByName[ AName ];
  if Assigned( def ) then
    result := def.Condition else
    raise Sil.Error.Create('Condition %s is unknown', [AName]);
end;

function TSilLexDefinitionList.GetDefByName(const AName: string): ILexDefinition;
begin
  if not FindDefByName(AName, Result) then
    raise Sil.Error.Create('Definition %s is unknown', [AName]);
end;

function TSilLexDefinitionList.DefExists(const AName: string): Boolean;
var
  Dummy: ILexDefinition;
begin
  Result := FindDefByName(AName, Dummy);
end;

function TSilLexDefinitionList.FindDefByName(const AName: string; out Def: ILexDefinition): Boolean;
begin
  Result := DoGetDefByName( FVisibleList, AName, Def ) or
            DoGetDefByName( FInvisibleList, AName, Def );
end;

function TSilLexDefinitionList.DoGetDefByName(const AList: IInterfaceList; const AName: string;
  out ADef: ILexDefinition): boolean;
var
  i1: integer;
begin
  result := false;
  if Assigned( AList ) then
  begin
    i1 := 0;
    while not result and ( i1 < AList.Count ) do
    begin
      ADef := ILexDefinition( AList[ i1 ] );
      result := ( ADef.Name = AName );
      Inc( i1 );
    end;
  end;
end;

function TSilLexDefinitionList.GetMatches(const AText: string; out AMatchList: ILexMatches; Widers: Boolean): boolean;
var
  i1, len, lmax: integer;
  def: ILexDefinition;
  Enum: IEnumerator;
  Item: ILexMatch;
begin
  i1 := 0;
  lmax := 0;
  if Assigned( AMatchList ) then
    AMatchList.Clear;

  if Assigned( FVisibleList ) then
    while ( i1 < FVisibleList.Count ) do
    begin
      def := ILexDefinition( FVisibleList[ i1 ] );
      if def.Condition.Match( AText, len ) and ( len >= lmax ) then
      begin

        if not Assigned( AMatchList ) then
          AMatchList := TSilLexMatches.Create;

        if Widers then
          lmax := len;
          
        AMatchList.AddMatch( TSilLexMatch.Create( def, Str.Copy(AText, 1, len) ) );
      end;
      Inc( i1 );
    end;

  if Widers and Assigned(AMatchList) then
    while AMatchList.Enumerate(Enum, Item) do
    begin
      if Item.Len < lmax then
        AMatchList.Delete(Enum.Iteration);
    end;

  result := Assigned( AMatchList ) and (AMatchList.Count > 0);
end;

{ TSilLexDefinition }

constructor TSilLexDefinition.Create(const Symbol: TToken; const AName: string; const ACondition: ILexCondition; const Data: Pointer);
begin
  inherited Create;
  FSymbol := Symbol;
  FName := AName;
  FCondition := ACondition;
  FData := Data;
end;

destructor TSilLexDefinition.Destroy;
begin
  FCondition := nil;
  inherited;
end;

function TSilLexDefinition.GetCondition: ILexCondition;
begin
  result := FCondition;
end;

function TSilLexDefinition.GetSymbol: TToken;
begin
  Result := FSymbol;
end;

function TSilLexDefinition.GetName: string;
begin
  result := FName;
end;

procedure TSilLexDefinition.SetCondition(const Value: ILexCondition);
begin
  FCondition := Value;
end;

function TSilLexDefinition.GetData: Pointer;
begin
  Result := FData;
end;

procedure TSilLexDefinition.SetData(const Value: Pointer);
begin
  FData := Value;
end;

end.
