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

{$IFNDEF FP20}

uses
(*)  Windows,(*)
  SilBeTypes,
  SilLiFiler,
  SilOiVersion,
  SilOkVersion;

type
  TTranslationList = array of LongWord;

  TSilLinuxVersionInfo = class (TSilVersionInfo)
  private
    FData: Pointer;
    FSize: LongWord;
    (*)FFixed: PVSFixedFileInfo;(*)
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

  TSilLinuxVersionTags = class (TSilVersionTags);

  TSilLinuxStandardTags = class (TSilStandardTags)
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

  TSilLinuxVersionNumber = class (TSilVersionNumber)
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

{$ENDIF}
implementation
{$IFNDEF FP20}

uses
  SysUtils,
  SilBtStr,
  SilBtMem,
  SilBtLarge,
  SilOsError,
  SilOtTool,
  SilOcVersion,
  SilOdVersion,
  SilLmFieldAccess;

{ TSilLinuxVersionInfo }

destructor TSilLinuxVersionInfo.Destroy;
begin
  Mem.Free(FData);
  inherited;
end;

procedure TSilLinuxVersionInfo.DoInitFileVersion(const FileName: string);
(*)var
  Dummy: LongWord;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxVersionInfo.DoInitFileVersion']);

(*)  FSize := GetFileVersionInfoSize(PChar(FileName), Dummy);;
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
  DoInitTranslation;(*)
end;

procedure TSilLinuxVersionInfo.DoInitTranslation;
(*)var
  TransPtr: Pointer;
  TransSz: LongWord;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxVersionInfo.DoInitTranslation']);
(*)  if FData = nil then Exit;
  OsError.Check(VerQueryValue(FData, '\VarFileInfo\Translation', TransPtr, TransSz));
  if TransSz > 0 then
  begin
    SetLength(FTrans, TransSz div SizeOf(LongWord));
    Move(TransPtr^, FTrans[0], TransSz);
  end;(*)
end;

function TSilLinuxVersionInfo.DoGetNumber: IVersionNumber;
(*)var
  Number: LargeInt;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxVersionInfo.DoGetNumber']);
(*)  if FFixed <> nil then
    Number := Large.Make(FFixed.dwFileVersionLS, FFixed.dwFileVersionMS) else
    Number := 0;
  Result := OS.Version.AsVersion(Number);(*)
end;

function TSilLinuxVersionInfo.DoGetTags(LocaleID: LongWord): IVersionTags;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxVersionInfo.DoGetTags']);
(*)  if (LocaleID = 0) and (FFixed <> nil) then LocaleID := FTrans[0];
  Result := TSilLinuxVersionTags.Create(Self, LocaleID);(*)
end;

function TSilLinuxVersionInfo.DoGetLCIDText(const LCID: LongWord): string;
begin
  Result := Str.Format('%.4x%.4x', [LongRec(LCID).Lo, LongRec(LCID).Hi]);
end;

function TSilLinuxVersionInfo.DoGetTagValue(const LCID: LongWord; const Tag: string): PChar;
(*)var
  Dummy: LongWord;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxVersionInfo.DoGetTagValue']);
(*)  if FData <> nil then
    OsError.Check(VerQueryValue(FData, PChar('\StringFileInfo\' + DoGetLCIDText(LCID) + '\' + Tag), Pointer(Result), Dummy)) else
    Result := '';(*)
end;

{ TSilLinuxStandardTags }

function TSilLinuxStandardTags.GetCompanyName: string;
begin
  Result := Tags[CCompanyName].AsString;
end;

function TSilLinuxStandardTags.GetFileDescription: string;
begin
  Result := Tags[CFileDescription].AsString;
end;

function TSilLinuxStandardTags.GetFileVersion: IVersionNumber;
begin
  Result := OS.Version.FromStr(Tags[CFileVersion].AsString);
end;

function TSilLinuxStandardTags.GetInternalName: string;
begin
  Result := Tags[CInternalName].AsString;
end;

function TSilLinuxStandardTags.GetLegalCopyright: string;
begin
  Result := Tags[CLegalCopyright].AsString;
end;

function TSilLinuxStandardTags.GetOriginalFilename: string;
begin
  Result := Tags[COriginalFilename].AsString;
end;

function TSilLinuxStandardTags.GetProductName: string;
begin
  Result := Tags[CProductName].AsString;
end;

function TSilLinuxStandardTags.GetProductVersion: IVersionNumber;
begin
  Result := OS.Version.FromStr(Tags[CProductVersion].AsString);
end;

{ TSilLinuxVersionNumber }

constructor TSilLinuxVersionNumber.Create(const Number: LargeInt);
begin
  inherited Create;
  FValue := Number;
end;

function TSilLinuxVersionNumber.DoGetValue: LargeInt;
begin
  Result := FValue;
end;

function TSilLinuxVersionNumber.DoGetMajor: Word;
begin
  Result := Word(Large.High(FValue) shr 16);
end;

function TSilLinuxVersionNumber.DoGetMinor: Word;
begin
  Result := Word(Large.High(FValue) and $FFFF);
end;

function TSilLinuxVersionNumber.DoGetRevision: Word;
begin
  Result := Word(Large.Low(FValue) shr 16);
end;

function TSilLinuxVersionNumber.DoGetBuild: Word;
begin
  Result := Word(Large.Low(FValue) and $FFFF);
end;
{$ENDIF}

end.
