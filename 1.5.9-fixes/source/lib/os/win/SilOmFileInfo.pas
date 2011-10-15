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

unit SilOmFileInfo;

{$I Defines.inc}

interface

uses
  SilOeTypes,
  SilOiHandle,
  SilOiFile,
  SilOkFileInfo;

type
  TSilWindowsFileInfo = class (TSilFileInfo)
  private
    FHandle: THandle;
  protected // IFileInfo
    function GetTime: TDateTime; override;
    function GetAttributes: TFileAttributes; override;
    function GetSize: LongWord; override;
  protected // IFileInfoDef
    procedure SetTime(Time: TDateTime); override;
    procedure SetAttributes(Value: TFileAttributes); override;
    procedure SetSize(Value: LongWord); override;
  public
    constructor Create(const Handle: IHandle; const FileName: String); override;
    constructor Create(const FileName: String = ''; const Time: TDateTime = 0; Attributes: TFileAttributes = []; Size: LongWord = 0); override;
  end;

implementation

uses
  SilOfFile;

{ TSilWindowsFileInfo }

constructor TSilWindowsFileInfo.Create(const Handle: IHandle; const FileName: String);
begin
  inherited Create(Handle, FileName);
  FHandle := Handle.Value;
end;

constructor TSilWindowsFileInfo.Create(const FileName: String; const Time: TDateTime; Attributes: TFileAttributes; Size: LongWord);
begin
  inherited;
  FHandle := 0;
end;

function TSilWindowsFileInfo.GetAttributes: TFileAttributes;
begin
  if FHandle > 0 then
    SilOfFile.GetAttributes(GetFullName, Result) else
    Result := FAttributes;
end;

function TSilWindowsFileInfo.GetSize: LongWord;
begin
  if FHandle > 0 then
    SilOfFile.GetFileSize(FHandle, Result) else
    Result := FSize;
end;

function TSilWindowsFileInfo.GetTime: TDateTime;
begin
  if FHandle > 0 then
    SilOfFile.GetFileTime(FHandle, Result) else
    Result := FTime;
end;

procedure TSilWindowsFileInfo.SetAttributes(Value: TFileAttributes);
begin
  SilOfFile.SetAttributes(GetFullName, Value);
end;

procedure TSilWindowsFileInfo.SetSize(Value: LongWord);
begin
end;

procedure TSilWindowsFileInfo.SetTime(Time: TDateTime);
begin
  if FHandle > 0 then
    SilOfFile.SetFileTime(FHandle, Time);
end;

end.
 