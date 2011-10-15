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

unit SilOfFile;

{$I Defines.inc}

interface

uses
  Windows,
  SilOcTypes,
  SilOiModule,
  SilOiFile;

function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function WindowsAttrToAttributes(Attr: Integer): TFileAttributes;
function AttributesToWindowsAttr(const Attr: TFileAttributes): Integer;
function GetFileInfo(Handle: THandle; const Info: IFileInfoDef): Boolean;

function SetAttributes(const FileName: String; const Attributes: TFileAttributes): Boolean;
function GetAttributes(const FileName: String; out Attributes: TFileAttributes): Boolean;
function SetFileTime(Handle: THandle; const Time: TDateTime): Boolean;
function GetFileTime(Handle: THandle; out Time: TDateTime): Boolean;
function GetFileSize(Handle: THandle; out Size: LongWord): Boolean;

function Exists(const FileName: String): Boolean;
function MoveFile(const OldName, NewName: String): Boolean;
function CopyFile(const Source, Dest: String): Boolean;
function CreateDirectory(const PathName: String): Boolean;
function RemoveDirectory(const PathName: String): Boolean;
function DeleteFile(const FileName: String): Boolean;
function TranslatePath(const Path: String; Old, New: Char): String;
function GetFilePath(const FileName: String): String;
function GetFileName(const FileName: String; IncludeExt: Boolean = true): String;
function GetFileExt(const FileName: String; IncludeDot: Boolean = true): String;
function ChangeFileExt(const FileName, Extension: string): string;
function ExpandPath(const FileName: String): String;
function DirectoryExists(const Path: String): Boolean;

implementation

uses
  SysUtils,

  SilLtReference,
  SilBtStr,
  SilOtTool;

const
  FileAttributes: array [TFileAttribute] of Integer = (
    FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_HIDDEN,
    FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_SYSTEM);

function GetFileInfo(Handle: THandle; const Info: IFileInfoDef): Boolean;
var
  FInfo: TByHandleFileInformation;
begin
  Result := GetFileInformationByHandle(Handle, FInfo);

  if Result then
  begin
    Info.Time := FileTimeToDateTime(FInfo.ftLastWriteTime);
    Info.Size := FInfo.nFileSizeLow;
    Info.Attributes := WindowsAttrToAttributes(FInfo.dwFileAttributes);
  end;
end;

function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
var
  FTime: TFileTime;
  STime: TSystemTime;
begin
  DateTimeToSystemTime(DateTime, STime);
  if not Windows.SystemTimeToFileTime(STime, FTime) or not Windows.LocalFileTimeToFileTime(FTime, Result) then
    FillChar(Result, SizeOf(TDateTime), 0);
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalTime: TFileTime;
  SystemTime: TSystemTime;
begin
  if Windows.FileTimeToLocalFileTime(FileTime, LocalTime) and
    Windows.FileTimeToSystemTime(LocalTime, SystemTime) then
    Result := SystemTimeToDateTime(SystemTime) else
    Result := 0;
end;

function WindowsAttrToAttributes(Attr: Integer): TFileAttributes;
var
  i: TFileAttribute;
begin
  Result := [];

  for i := Low(FileAttributes) to High(FileAttributes) do
    if FileAttributes[i] and Attr > 0 then
      Include(Result, i);
end;

function AttributesToWindowsAttr(const Attr: TFileAttributes): Integer;
var
  i: TFileAttribute;
begin
  Result := 0;

  for i := Low(FileAttributes) to High(FileAttributes) do
    if i in Attr then
      Result := Result or FileAttributes[i];
end;

function SetFileTime(Handle: THandle; const Time: TDateTime): Boolean;
var
  dtTime: TFileTime;
begin
  dtTime := DateTimeToFileTime(Time);

  if (dtTime.dwLowDateTime <> 0) and (dtTime.dwHighDateTime <> 0) then
    Result := Windows.SetFileTime(Handle, nil, nil, @dtTime) else
    Result := false;
end;

function GetFileTime(Handle: THandle; out Time: TDateTime): Boolean;
var
  dtTime: TFileTime;
begin
  Result := Windows.GetFileTime(Handle, nil, nil, @dtTime);
  if Result then Time := FileTimeToDateTime(dtTime);
end;

function SetAttributes(const FileName: String; const Attributes: TFileAttributes): Boolean;
begin
  Result := Windows.SetFileAttributes(PChar(FileName), AttributesToWindowsAttr(Attributes));
end;

function GetAttributes(const FileName: String; out Attributes: TFileAttributes): Boolean;
var
  Attr: DWORD;
  OldMode: LongWord;
begin
  OldMode := Windows.SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    Attr := Windows.GetFileAttributes(PChar(FileName));
    Result := Attr <> $FFFFFFFF;
    if Result then Attributes := WindowsAttrToAttributes(Attr);
  finally
    Windows.SetErrorMode(OldMode);
  end;
end;

function GetFileSize(Handle: THandle; out Size: LongWord): Boolean;
begin
  Size := Windows.GetFileSize(Handle, nil);
  Result := Size <> $FFFFFFFF;
end;

function CreateDirectory(const PathName: String): Boolean;
begin
  Result := Windows.CreateDirectory(PChar(PathName), nil);
end;

function DeleteFile(const FileName: String): Boolean;
begin
  Result := Windows.DeleteFile(PChar(FileName));
end;

function Exists(const FileName: String): Boolean;
begin
  Result := Windows.GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('.\:', FileName);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;

  Result := Str.Copy(FileName, 1, I - 1);

  if (Length(Extension) > 0) then
  begin
    if (Extension[1] <> '.') then
      Str.Add(Result, '.');
    Str.Add(Result, Extension);
  end;
end;

function GetFileName(const FileName: String; IncludeExt: Boolean): String;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('\:', FileName);
  Result := Str.Copy(FileName, I + 1);

  if not IncludeExt then
  begin
    I := Str.LastDelimiter('.\:', Result);
    if (I > 0) and (Result[I] = '.') then
      Result := Str.Copy(Result, 1, I - 1);
  end;
end;

function GetFileExt(const FileName: String; IncludeDot: Boolean): String;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('.\:', FileName);

  if (I > 0) and (FileName[I] = '.') then
    Result := Str.Copy(FileName, I + Ord(not IncludeDot)) else
    Result := '';
end;

function GetFilePath(const FileName: String): String;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('\:', FileName);
  Result := Str.Copy(FileName, 1, I);
end;

function MoveFile(const OldName, NewName: String): Boolean;
begin
  Result := Windows.MoveFileEx(PChar(OldName), PChar(NewName), MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH);
end;

function CopyFile(const Source, Dest: String): Boolean;
begin
  Result := Windows.CopyFile(PChar(Source), PChar(Dest), false);
end;

function RemoveDirectory(const PathName: String): Boolean;
begin
  Result := Windows.RemoveDirectory(PChar(PathName));
end;

function TranslatePath(const Path: String; Old, New: Char): String;
var
  i: Integer;
begin
  Result := Path;
  if Str.IsEmpty(Result) then Exit;

  if (Result[1] = Old) and (Length(Result) > 1) and ((Length(Result) = 2) or (Result[3] = Old)) then
    Result := Result[2] + ':';

  for i := 1 to Length(Result) do
    if Result[i] = Old then
      Result[i] := New;
end;

function ExpandPath(const FileName: String): String;
var
  Module: IModule2;
begin
  Result := FileName;

  if Str.NotEmpty(Result) and (Result[1] = '.') then
  begin
    Str.Delete(Result, 1, 1);

    if Str.NotEmpty(Result) and (Result[1] in ['\', '/']) then
      Str.Delete(Result, 1, 1);

    if Ref.GetInterface(OS.Module.Current, IModule2, Module) then
      Result := Module.Info.Path + Result;
  end;
end;

function DirectoryExists(const Path: String): Boolean;
var
  Attr: TFileAttributes;
begin
  Result := GetAttributes(Path, Attr) and (SilOiFile.faDirectory in Attr);
end;

end.
