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

unit SilOmVersion;

{$INCLUDE Defines.inc}

interface

uses
  Windows,
  SilBeTypes,
  SilLiFiler,
  SilOiVersion,
  SilOkVersion;

type
  TTranslationList = array of LongWord;

  TSilWindowsVersionInfo = class (TSilVersionInfo)
  private
    FData: Pointer;
    FSize: LongWord;
    FFixed: PVSFixedFileInfo;
    FTrans: TTranslationList;
    procedure DoInitTranslation;
    function DoGetLCIDText(const LCID: LongWord): string;
  protected //- TSilVersionInfo 
    procedure DoInitFileVersion(const FileName: string); override;
    function DoGetTagValue(const LCID: LongWord; const Tag: string): PChar; override;
    function DoGetNumber: IVersionNumber; override;
    function DoGetTags(LocaleID: LongWord = 0): IVersionTags; override; 
  public
    destructor Destroy; override;
  end;

  TSilWindowsVersionTags = class (TSilVersionTags);

  TSilWindowsStandardTags = class (TSilStandardTags)
  protected //- IVersionTags
		function GetProductName: string; override;
    function GetCompanyName: string; override;
		function GetFileDescription: string; override;
		function GetFileVersion: IVersionNumber; override;
		function GetInternalName: string; override;
		function GetLegalCopyright: string; override;
		function GetOriginalFilename: string; override;
		function GetProductVersion: IVersionNumber; override;
  end;

  TSilWindowsVersionNumber = class (TSilVersionNumber)
  private
    FValue: LargeInt;
  protected //- IVersionNumber
    function DoGetValue: LargeInt; override;
    function DoGetMajor: Word; override;
    function DoGetMinor: Word; override;
    function DoGetRevision: Word; override;
    function DoGetBuild: Word; override;
  public
    constructor Create(const Number: LargeInt); override;
  end;

implementation

uses
  SilBtStr,
  SilBtMem,
  SilBtLarge,
  SilOsError,
  SilOtTool,
  SilOcVersion,
  SilOdVersion,
  SilLmFieldAccess;

{ TSilWindowsVersionInfo }

destructor TSilWindowsVersionInfo.Destroy;
begin
  Mem.Free(FData);
  inherited;
end;

procedure TSilWindowsVersionInfo.DoInitFileVersion(const FileName: string);
var
  Dummy: LongWord;
begin
  FSize := GetFileVersionInfoSize(PChar(FileName), Dummy);;
  if FSize <> 0 then
  begin
    FData := Mem.Alloc(FSize);
    try
      OsError.Check(GetFileVersionInfo(PChar(FileName), 0, FSize, FData));
      OsError.Check(VerQueryValue(FData, '\', Pointer(FFixed), Dummy));
    except
      Mem.Free(FData);
      raise;
    end;
  end;
  DoInitTranslation;
end;

procedure TSilWindowsVersionInfo.DoInitTranslation;
var
  TransPtr: Pointer;
  TransSz: LongWord;
begin
  if FData = nil then Exit;
  OsError.Check(VerQueryValue(FData, '\VarFileInfo\Translation', TransPtr, TransSz));
  if TransSz > 0 then
  begin
    SetLength(FTrans, TransSz div SizeOf(LongWord));
    Move(TransPtr^, FTrans[0], TransSz);
  end;
end;

function TSilWindowsVersionInfo.DoGetNumber: IVersionNumber;
var
  Number: LargeInt;
begin
  if FFixed <> nil then
    Number := Large.Make(FFixed.dwFileVersionLS, FFixed.dwFileVersionMS) else
    Number := 0;
  Result := OS.Version.AsVersion(Number);
end;

function TSilWindowsVersionInfo.DoGetTags(LocaleID: LongWord): IVersionTags;
begin
  if (LocaleID = 0) and (FFixed <> nil) then LocaleID := FTrans[0];
  Result := TSilWindowsVersionTags.Create(Self, LocaleID);
end;

function TSilWindowsVersionInfo.DoGetLCIDText(const LCID: LongWord): string;
begin
  Result := Str.Format('%.4x%.4x', [LongRec(LCID).Lo, LongRec(LCID).Hi]);
end;

function TSilWindowsVersionInfo.DoGetTagValue(const LCID: LongWord; const Tag: string): PChar;
var
  Dummy: LongWord;
begin
  if FData <> nil then
    OsError.Check(VerQueryValue(FData, PChar('\StringFileInfo\' + DoGetLCIDText(LCID) + '\' + Tag), Pointer(Result), Dummy)) else
    Result := '';
end;

{ TSilWindowsStandardTags }

function TSilWindowsStandardTags.GetCompanyName: string;
begin
  Result := Tags[CCompanyName].AsString;
end;

function TSilWindowsStandardTags.GetFileDescription: string;
begin
  Result := Tags[CFileDescription].AsString;
end;

function TSilWindowsStandardTags.GetFileVersion: IVersionNumber;
begin
  Result := OS.Version.FromStr(Tags[CFileVersion].AsString);
end;

function TSilWindowsStandardTags.GetInternalName: string;
begin
  Result := Tags[CInternalName].AsString;
end;

function TSilWindowsStandardTags.GetLegalCopyright: string;
begin
  Result := Tags[CLegalCopyright].AsString;
end;

function TSilWindowsStandardTags.GetOriginalFilename: string;
begin
  Result := Tags[COriginalFilename].AsString;
end;

function TSilWindowsStandardTags.GetProductName: string;
begin
  Result := Tags[CProductName].AsString;
end;

function TSilWindowsStandardTags.GetProductVersion: IVersionNumber;
begin
  Result := OS.Version.FromStr(Tags[CProductVersion].AsString);
end;

{ TSilWindowsVersionNumber }

constructor TSilWindowsVersionNumber.Create(const Number: LargeInt);
begin
  inherited Create;
  FValue := Number;
end;

function TSilWindowsVersionNumber.DoGetValue: LargeInt;
begin
  Result := FValue;
end;

function TSilWindowsVersionNumber.DoGetMajor: Word;
begin
  Result := Word(Large.High(FValue) shr 16);
end;

function TSilWindowsVersionNumber.DoGetMinor: Word;
begin
  Result := Word(Large.High(FValue) and $FFFF);
end;

function TSilWindowsVersionNumber.DoGetRevision: Word;
begin
  Result := Word(Large.Low(FValue) shr 16);
end;

function TSilWindowsVersionNumber.DoGetBuild: Word;
begin
  Result := Word(Large.Low(FValue) and $FFFF);
end;

end.
