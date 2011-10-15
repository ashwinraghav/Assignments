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

unit SilSmLexMatches;

{$I Defines.inc}

interface

uses
  Sil,

  SilLmInterfaceList,
  SilSiLexDictionary;

type
  TSilLexMatch = class ( TSilObject, ILexMatch )
  private
    FLexema: string;
    FDefinition: Pointer;
  protected // ILexMatch
    function GetLexema: string;
    function GetLen: integer;
    function GetDefinition: ILexDefinition;
  protected // properties
    property Len: integer read GetLen;
    property Lexema: string read GetLexema;
    property Definition: ILexDefinition read GetDefinition;
  public
    constructor Create( const ADefinition: ILexDefinition; const Lexema: string); reintroduce;
    destructor Destroy; override; 
  end;

  TSilLexMatches = class ( TSilInterfaceList, ILexMatches )
  protected // ILexMatches
    function GetMatch( Index: integer ): ILexMatch;
    procedure AddMatch( const AMatch: ILexMatch );
    function FindMatch( const AName: string ): ILexMatch;
    // properties
    property Match[ Index: integer ]: ILexMatch read GetMatch;
  public
    destructor Destroy; override; 
  end;

implementation

{ TSilLexMatch }

constructor TSilLexMatch.Create(const ADefinition: ILexDefinition; const Lexema: string);
begin
  inherited Create;
  FDefinition := Pointer(ADefinition);
  FLexema := Lexema;
end;

destructor TSilLexMatch.Destroy;
begin
  FDefinition := nil;
  inherited;
end;

function TSilLexMatch.GetDefinition: ILexDefinition;
begin
  result := ILexDefinition(FDefinition);
end;

function TSilLexMatch.GetLen: integer;
begin
  result := Length(FLexema);
end;

function TSilLexMatch.GetLexema: string;
begin
  Result := FLexema;
end;

{ TSilLexMatches }

destructor TSilLexMatches.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSilLexMatches.AddMatch(const AMatch: ILexMatch);
begin
  inherited Add( AMatch );
end;

function TSilLexMatches.FindMatch(const AName: string): ILexMatch;
var
  enum: IEnumerator;
begin
  result := nil;
  while not Assigned( result ) and Enumerate( enum, result ) do
    if ( result.Definition.Name <> AName ) then
      result := nil;
end;

function TSilLexMatches.GetMatch( Index: integer ): ILexMatch;
begin
  result := GetItem( Index ) as ILexMatch;
end;

end.
