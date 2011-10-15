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

unit SilSiLexDictionary;

{$I Defines.inc}

interface

uses
  Sil,
  SilSeTokens;

type
  ILexCondition = interface;
  ILexDefinition = interface;
  ILexDefinitionList = interface;
  ILexMultipleCondition = interface;
  ILexDictionary = interface;
  ILexMatch = interface;
  ILexMatches = interface;

  ILexDictionary = interface
    ['{9446A6F4-9660-400E-8DBB-272E109FC518}']
    function GetDefinitions: ILexDefinitionList;
    // properties
    property Definitions: ILexDefinitionList read GetDefinitions;
  end;

  ILexDefinitionList = interface
    ['{4D57DF03-F775-4759-9E76-DF3AA02BFE4E}']
    function GetDefByName( const AName: string ): ILexDefinition;
    function GetCondByName( const AName: string ): ILexCondition;
    function FindDefByName( const AName: string; out Def: ILexDefinition ): Boolean;   
    function DefExists( const AName: string ): Boolean; 
    procedure Add( const ADef: ILexDefinition; AVisible: boolean = true ); overload;
    procedure Add( const Symbol: TToken; const AName: string; const ACondition: ILexCondition; const Data: Pointer = nil; AVisible: boolean = true  ); overload;
    procedure Clear;
    function GetMatches( const AText: string; out AMatchList: ILexMatches; Widers: Boolean = False ): boolean;
    // properties
    property DefByName[ const AName: string ]: ILexDefinition read GetDefByName;
    property CondByName[ const AName: string ]: ILexCondition read GetCondByName;
  end;

  ILexDefinition = interface
    ['{4755288E-7594-44FE-8FF3-BD899F497BCD}']
    function GetSymbol: TToken;
    function GetName: string;
    function GetCondition: ILexCondition;
    procedure SetCondition( const Value: ILexCondition );
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    // properties
    property Symbol: TToken read GetSymbol;
    property Name: string read GetName;
    property Condition: ILexCondition read GetCondition write SetCondition;
    property Data: Pointer read GetData write SetData;
  end;

  ILexMultipleCondition = interface
    ['{E20EAD12-D8F3-4543-8326-EBAA8A80CB30}']
    procedure AddDef( const ACondition: ILexCondition );
  end;

  ILexCondition = interface
    ['{6A3F9180-1A15-47E6-82D9-52B85AF76495}']
    function Match( const AText: string; out ALen: integer ): boolean;
  end;

  ILexMatch = interface
    ['{2F5B44A2-0545-497C-BCC6-EF50A71E3E31}']
    function GetLexema: string;
    function GetLen: integer;
    function GetDefinition: ILexDefinition;
    // properties
    property Lexema: string read GetLexema;
    property Len: integer read GetLen;
    property Definition: ILexDefinition read GetDefinition;
  end;

  ILexMatches = interface ( IInterfaceList )
    ['{7F26FF67-5783-4A56-A485-2ED69BA6EC4D}']
    function GetMatch( Index: integer ): ILexMatch;
    procedure AddMatch( const AMatch: ILexMatch );
    function FindMatch( const AName: string ): ILexMatch;
    // properties
    property Match[ Index: integer ]: ILexMatch read GetMatch; default;
  end;

implementation

end.

