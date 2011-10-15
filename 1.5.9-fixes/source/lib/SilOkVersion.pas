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

unit SilOkVersion;

interface

{$INCLUDE Defines.inc}

uses
  SilLkInterfaced,
  SilBeTypes,
  SilLiFiler,

  SilOiVersion;

type
  TSilVersionInfo = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IVersionInfo )
  protected //- IVersionInfo
    function IVersionInfo.GetNumber = DoGetNumber;
    function IVersionInfo.Tags = DoGetTags;
    function GetFullName: string;
  protected
    procedure DoInitFileVersion(const FileName: string); virtual; abstract;
    function DoGetTagValue(const LCID: LongWord; const Tag: string): PChar; virtual; abstract;
    function DoGetNumber: IVersionNumber; virtual; abstract;
    function DoGetTags(LocaleID: LongWord = 0): IVersionTags; virtual; abstract; 
  public
    constructor Create(const FileName: string); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
  end;

  TSilVersionTags = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IVersionTags )
  private
    FOwner: TSilVersionInfo;
    FLCID: LongWord;
  protected //- IVersionTags
    function GetTag(const Tag: string): IValueReader;
    function GetStd: IStandardTags;
  public
    constructor Create(Owner: TSilVersionInfo; const LCID: LongWord); virtual;
    destructor Destroy; override;
    property Owner: TSilVersionInfo read FOwner;
    property LCID: LongWord read FLCID;
  end;

  TSilStandardTags = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IStandardTags )
  private
    FTags: IVersionTags;
  protected //- IVersionTags
		function GetProductName: string; virtual; abstract;
    function GetCompanyName: string; virtual; abstract;
		function GetFileDescription: string; virtual; abstract;
		function GetFileVersion: IVersionNumber; virtual; abstract;
		function GetInternalName: string; virtual; abstract;
		function GetLegalCopyright: string; virtual; abstract;
		function GetOriginalFilename: string; virtual; abstract;
		function GetProductVersion: IVersionNumber; virtual; abstract;
  public
    constructor Create(const Tags: IVersionTags); virtual;
    destructor Destroy; override;
    property Tags: IVersionTags read FTags; 
  end;

  TSilVersionNumber = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IVersionNumber )
  protected //- IVersionNumber
    function IVersionNumber.GetValue = DoGetValue;
    function IVersionNumber.GetMajor = DoGetMajor;
    function IVersionNumber.GetMinor = DoGetMinor;
    function IVersionNumber.GetRevision = DoGetRevision;
    function IVersionNumber.GetBuild = DoGetBuild;
    function GetItem(Index: TVersionItem): Word;
    function ToStr(const Formats: string = '%m.%0.2n'; const Default: string = ''): string;
  protected //- 
    function DoGetValue: LargeInt; virtual; abstract;
    function DoGetMajor: Word; virtual; abstract;
    function DoGetMinor: Word; virtual; abstract;
    function DoGetRevision: Word; virtual; abstract;
    function DoGetBuild: Word; virtual; abstract;
  public
    constructor Create(const Number: LargeInt); overload; virtual; abstract;
  end;

implementation

uses
  SilOsClasses,
  SilOdVersion,
  SilOtTool,
  SilBtError,
  SilLiField,
  SilLmFieldAccess;

{ TSilVersionInfo }

constructor TSilVersionInfo.Create(const FileName: string);
begin
  inherited Create;
  DoInitFileVersion(FileName);
end;

constructor TSilVersionInfo.Create;
begin
  inherited Create;
end;

destructor TSilVersionInfo.Destroy;
begin
  inherited;
end;

function TSilVersionInfo.GetFullName: string;
var
  Num: IVersionNumber;
  Tags: IVersionTags;
begin
  Num := DoGetNumber();
  Tags := DoGetTags();
  Result := Tags.Std.FileDescription +
            ' V'        + Num.ToStr(CShortVersion) +
            ' r'        + Num.ToStr(CRelease) +
            ' '#169' '  + Tags.Std.LegalCopyright       +
            ' by '      + Tags.Std.CompanyName;
end;

{ TSilVersionTags }

constructor TSilVersionTags.Create(Owner: TSilVersionInfo; const LCID: LongWord);
begin
  ASSERT(Owner <> nil, 'Owner <> nil');
  inherited Create;
  FOwner := Owner;
  FLCID := LCID;
  FOwner._AddRef;
end;

destructor TSilVersionTags.Destroy;
begin
  FOwner._Release;
  inherited;
end;

function TSilVersionTags.GetTag(const Tag: string): IValueReader;
var
  Field: IFieldAccess;
begin
  Field := TStringFieldAccess.Create(Tag, nil);
  Field.AsString := FOwner.DoGetTagValue(FLCID, Tag);
  Result := Field;
end;

function TSilVersionTags.GetStd: IStandardTags;
begin
  Result := TSilOsStandardTags.Create(Self);
end;

{ TSilStandardTags }

constructor TSilStandardTags.Create(const Tags: IVersionTags);
begin
  inherited Create;
  FTags := Tags;
end;

destructor TSilStandardTags.Destroy;
begin
  FTags := nil;
  inherited;
end;

{ TSilVersionNumber }

function TSilVersionNumber.GetItem(Index: TVersionItem): Word;
begin
  Result := 0;
  case Index of
    viMajor:      Result := DoGetMajor;
    viMinor:      Result := DoGetMinor;
    viRevision:   Result := DoGetRevision;
    viBuild:      Result := DoGetBuild;
  else
    Error.Throw(SInvalidVersionItem);
  end;
end;

function TSilVersionNumber.ToStr(const Formats, Default: string): string;
begin
  Result := OS.Version.ToStr(Self, Formats, Default);
end;

end.
