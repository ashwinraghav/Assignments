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
  libc,
  SysUtils,
  UnixUtil,

  SilOiFile;

function FileTimeToDateTime(Stamp: time_t): TDateTime;
function LinuxAttrToAttributes(Attr: Integer): TFileAttributes;
function AttributesToLinuxAttr(const Attr: TFileAttributes): Integer;
function GetFileInfo(Handle: THandle; const Info: IFileInfoDef): Boolean;

function SetAttributes(const FileName: String; const Attributes: TFileAttributes): Boolean;
function GetAttributes(const FileName: String; out Attributes: TFileAttributes): Boolean;
function SetFileTime(Handle: THandle; const Value: TDateTime): Boolean;
function GetFileTime(Handle: THandle; out Value: TDateTime): Boolean;
function GetFileSize(Handle: THandle; out Value: LongWord): Boolean;

function Exists(const FileName: String): Boolean;
function MoveFile(const OldName, NewName: String): Boolean;
function CopyFile(const Source, Dest: String): Boolean;
function CreateDirectory(const PathName: String): Boolean;
function RemoveDirectory(const PathName: String): Boolean;
function DeleteFile(const FileName: String): Boolean;
function TranslatePath(const Path: String): String;
function GetFilePath(const FileName: String): String;
function GetFileName(const FileName: String; IncludeExt: Boolean = true): String;
function GetFileExt(const FileName: String; IncludeDot: Boolean = true): String;
function ChangeFileExt(const FileName, Extension: string): string;
function ExpandPath(const FileName: String): String;
function DirectoryExists(const Path: String): Boolean;

implementation

uses
  SilBtStr,
  SilBtDate,
  SilBtTime,
  SilOtTool;

const
  { read/write search permission for everyone }
  MODE_MKDIR = S_IWUSR OR S_IRUSR OR
               S_IWGRP OR S_IRGRP OR
               S_IWOTH OR S_IROTH OR
               S_IXUSR OR S_IXGRP OR S_IXOTH;

function GetFileInfo(Handle: THandle; const Info: IFileInfoDef): Boolean;
var
  Stat: TStatBuf;
begin
  Result := libc.fstat(Handle, @Stat) >= 0;

  if Result then
  begin
    Info.Time := FileTimeToDateTime(Stat.st_mtime);
    Info.Size := Stat.st_size;
    Info.Attributes := LinuxAttrToAttributes(Stat.st_mode);
  end;
end;

function LinuxAttrToAttributes(Attr: Integer): TFileAttributes;
begin
  Result := [];

  if S_ISDIR(Attr) then
    Include(Result, faDirectory)
  else
    Include(Result, faArchive);

  if (Attr and S_IWUSR) = 0 then Include(Result, faReadOnly);
  if S_ISSOCK(Attr) or S_ISBLK(Attr) or S_ISCHR(Attr) or S_ISFIFO(Attr) then Include(Result, faSysFile);
end;

function AttributesToLinuxAttr(const Attr: TFileAttributes): Integer;
begin
  Result := 0;

  if faDirectory in Attr then Result := Result or S_IFDIR;
  if faReadOnly in Attr then Result := Result or S_IWUSR;
end;

function SetFileTime(Handle: THandle; const Value: TDateTime): Boolean;
(*)var
  dtTime: TFileTime;(*)
begin
  Result := false;
(*)
  dtTime := DateTimeToFileTime(Time);

  if (dtTime.dwLowDateTime <> 0) and (dtTime.dwHighDateTime <> 0) then
    Result := libc.SetFileTime(Handle, nil, nil, @dtTime) else
    Result := false;
(*)end;

function FileTimeToDateTime(Stamp: time_t): TDateTime;
var
  year, month, day, hour, minute, second: Word;
begin
  EpochToLocal(Stamp, year, month, day, hour, minute, second);
  Result := Date.EncodeParts(year, month, day) + Time.EncodeParts(hour, minute, second, 0);
end;

function GetFileTime(Handle: THandle; out Value: TDateTime): Boolean;
var
  Stat: TStatBuf;
begin
  Result := libc.fstat(Handle, @Stat) >= 0;

  if Result then
    Value := FileTimeToDateTime(Stat.st_mtime);
end;

function SetAttributes(const FileName: String; const Attributes: TFileAttributes): Boolean;
begin
  Result := false;
//  Result := libc.SetFileAttributes(PChar(FileName), AttributesToLinuxAttr(Attributes));
end;

function GetAttributes(const FileName: String; out Attributes: TFileAttributes): Boolean;
var
  Stat: TStatBuf;
begin
  Result := libc.stat(PChar(FileName), @Stat) >= 0;
  if Result then Attributes := LinuxAttrToAttributes(Stat.st_mode);
end;

function GetFileSize(Handle: THandle; out Value: LongWord): Boolean;
var
  Stat: TStatBuf;
begin
  Result := libc.fstat(Handle, @Stat) >= 0;
  Value := Stat.st_size;
end;

function CreateDirectory(const PathName: String): Boolean;
begin
  Result := libc.__mkdir(PChar(PathName), MODE_MKDIR) >= 0;
end;

function DeleteFile(const FileName: String): Boolean;
begin
  Result := libc.unlink(PChar(FileName)) >= 0;
end;

function Exists(const FileName: String): Boolean;
var
  Stat: TStatBuf;
begin
  Result := libc.stat(PChar(FileName), @Stat) >= 0;
end;

function DirectoryExists(const Path: String): Boolean;
var
  Stat: TStatBuf;
begin
  Result := (libc.stat(PChar(Path), @Stat) = 0) and S_ISDIR(Stat.st_mode);
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('./:', Filename);
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
  I := SysUtils.LastDelimiter('/:', FileName);
  Result := Str.Copy(FileName, I + 1);

  if not IncludeExt then
  begin
    I := Str.LastDelimiter('./:', Result);
    if (I > 0) and (Result[I] = '.') then
      Result := Str.Copy(Result, 1, I - 1);
  end;
end;

function GetFileExt(const FileName: String; IncludeDot: Boolean): String;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('./:', FileName);

  if (I > 0) and (FileName[I] = '.') then
    Result := Str.Copy(FileName, I + Ord(not IncludeDot)) else
    Result := '';
end;

function GetFilePath(const FileName: String): String;
var
  I: Integer;
begin
  I := SysUtils.LastDelimiter('/:', FileName);
  Result := Str.Copy(FileName, 1, I);
end;

function MoveFile(const OldName, NewName: String): Boolean;
begin
  Result := libc.__rename(PChar(OldName), PChar(NewName)) >= 0;
end;

function CopyFile(const Source, Dest: String): Boolean;
begin
  raise Exception.CreateFmt('%s: not implemented', ['CopyFile']);
  (*)
  Result := libc.CopyFile(PChar(Source), PChar(Dest), false);
(*)end;

function RemoveDirectory(const PathName: String): Boolean;
begin
  Result := libc.__rmdir(PChar(PathName)) >= 0;
end;

function TranslatePath(const Path: String): String;
(*)var
  i: Integer;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TranslatePath']);
(*)
  Result := Path;
  if Str.IsEmpty(Path) then Exit;

  if (Result[1] = '/') and (Length(Result) > 1) then
  begin
    Result[1] := Result[2];
    Result[2] := ':';
  end;

  for i := 1 to Length(Result) do if Result[i] = '/' then Result[i] := '\';
(*)end;

function ExpandPath(const FileName: String): String;
begin
  Result := FileName;

  if Str.NotEmpty(Result) and (Result[1] = '.') then
  begin
    Str.Delete(Result, 1, 1);
    if Str.NotEmpty(Result) and (Result[1] in ['\', '/']) then Str.Delete(Result, 1, 1);
    Result := OS.FileSystem.GetFilePath(OS.Module.Current.FullName) + Result;
  end;
end;

end.
