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

unit SilOtFile;

{$I Defines.inc}

interface

uses
  SysUtils,

  SilBkTool,
  SilLiList,
  SilOjFile,
  SilOsTypes,
  SilOiHandle,
  SilOiFile;

type
  OsFile = class (SilFile)
    class function CreateDirectory(const PathName: String): Boolean; override;
    class function RemoveDirectory(const PathName: String): Boolean; override;
    class function Exists(const FileName: String): Boolean; override;
    class function DirectoryExists(const Path: String): Boolean; override;
    //class function GetInfo(const FileName: String): IFileInfo; override;
    class function FillInfo(const FileName: String; var Info: IFileInfoDef): Boolean; override;
    class function MoveFile(const OldName, NewName: String): Boolean; override;
    class function CopyFile(const Source, Dest: String): Boolean; override;
    class function DeleteFile(const FileName: String): Boolean; override;
    class function TranslatePath(const Path: String; Old: Char = '/'; New: Char = CPathSeparator): String; override;
    class function GetFilePath(const FileName: String): String; override;
    class function GetFileName(const FileName: String; IncludeExt: Boolean = true): String; override;
    class function GetFileExt(const FileName: String; IncludeDot: Boolean = true): String; override;
    class function ChangeFileExt(const FileName, Extension: string): string; override;
    class function ExpandPath(const FileName: String): String; override;
    class function GetList(const Path: String; const Attributes: TFileAttributes; Recursive: Boolean = false; const Listener: IListEvents = nil): IFileInfoList; overload; override;
  end;

implementation

uses
  libc,

  SilBtStr,
  SilLiEnumerator,
  //SilLtConnection,
  SilOfFile{,
  //SilOtError,
  //SilOmFileInfo,
  //SilOmTextFile,
  SilOkFileInfo};

class function OsFile.Exists(const FileName: String): Boolean;
begin
  Result := SilOfFile.Exists(FileName);
end;

class function OsFile.FillInfo(const FileName: String; var Info: IFileInfoDef): Boolean;
var
  Stat: TStatBuf;
begin
  Info.Name := FileName;

  if libc.stat(PChar(FileName), @Stat) >= 0 then
  begin
    Info.Time := SilOfFile.FileTimeToDateTime(Stat.st_mtime);
    Info.Attributes := SilOfFile.LinuxAttrToAttributes(Stat.st_mode);
    Info.Size := Stat.st_size;
  end;
end;

(*)class function OsFile.GetInfo(const FileName: String): IFileInfo;
var
  Find: Integer;
  Search: TSearchRec;
  Info: IFileInfoDef;
begin
  raise Exception.CreateFmt('%s: not implemented', ['OsFile.GetInfo']);
  Find := FindFirst(FileName, faAnyFile, Search);

  if Find = 0 then
  begin
    Info := TSilFileInfo.Create;
    Info.Name := FileName;
    Info.Time := FileDateToDateTime(Search.Time);
    Info.Attributes := SilOfFile.LinuxAttrToAttributes(Search.Attr);
    Info.Size := Search.Size;
    FindClose(Search);
  end;

  Result := Info;
end;(*)

class function OsFile.MoveFile(const OldName, NewName: String): Boolean;
begin
  Result := SilOfFile.MoveFile(OldName, NewName);
end;

class function OsFile.CopyFile(const Source, Dest: String): Boolean;
begin
  Result := SilOfFile.CopyFile(Source, Dest);
end;

class function OsFile.CreateDirectory(const PathName: String): Boolean;
begin
  Result := SilOfFile.CreateDirectory(PathName);
end;

class function OsFile.RemoveDirectory(const PathName: String): Boolean;
begin
  Result := SilOfFile.RemoveDirectory(PathName);
end;

class function OsFile.TranslatePath(const Path: String; Old: Char = '/'; New: Char = CPathSeparator): String;
begin
  Result := SilOfFile.TranslatePath(Path);
end;

class function OsFile.DeleteFile(const FileName: String): Boolean;
begin
  Result := SilOfFile.DeleteFile(FileName);
end;

class function OsFile.ChangeFileExt(const FileName, Extension: string): string;
begin
  Result := SilOfFile.ChangeFileExt(FileName, Extension);
end;

class function OsFile.GetFileName(const FileName: String; IncludeExt: Boolean): String;
begin
  Result := SilOfFile.GetFileName(FileName, IncludeExt);
end;

class function OsFile.GetFileExt(const FileName: String; IncludeDot: Boolean): String;
var
  i: Integer;
begin
  i := Str.LastPos('.', FileName);
  if i > 0 then
    Result := Str.Copy(FileName, i)
  else
    Result := '';
end;

class function OsFile.GetFilePath(const FileName: String): String;
begin
  Result := SilOfFile.GetFilePath(FileName);
end;

class function OsFile.ExpandPath(const FileName: String): String;
begin
  Result := SilOfFile.ExpandPath(FileName);
end;

class function OsFile.DirectoryExists(const Path: String): Boolean;
begin
  Result := SilOfFile.DirectoryExists(Path);
end;

class function OsFile.GetList(const Path: String; const Attributes: TFileAttributes; Recursive: Boolean; const Listener: IListEvents): IFileInfoList;

  procedure DoRead(const Dir: IDirectoryReader);
  var
    Enum: IEnumerator;
    Item: IFileInfo;
  begin
    while Dir.Read do;
    Result.AddList(Dir.List);

    if Recursive then
      while Result.Enumerate(Enum, Item) do
        if faDirectory in Item.Attributes then
          DoRead(ReadDirectory(Item.FullName));
  end;

begin
  Result := FileInfoList;
  DoRead(ReadDirectory(Path));
end;

end.
