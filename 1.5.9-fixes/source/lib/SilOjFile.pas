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

unit SilOjFile;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiList,
  SilLiCompare,
  SilLiEnumerator,
  SilOeFilesystemNotifier,
  SilOiFile,
  SilOiTextFile,
  SilOiHandle,
  SilOiFilesystemNotifier,
  SilOsTypes;

type
  SilFile = class (Tool)
    class function OpenFile(const FileName: string; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustExist: Boolean = False): IFile;
    class function CreateFile(const FileName: string; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustCreate: Boolean = True): IFile;

    class function StampFileName(const FileName: string; const Format: String = '-yyyymmdd.hhnnss'): String;
    class function StampLimitCount(const FileName: string; MaxCount: Integer = 10): Boolean;
    class function StampLast(const FileName: string; out LastName: String): Boolean;

    class function LogCreate(const FileName: string; MaxCount: Integer): String;
    class function LogLimitCount(const FileName: string; MaxCount: Integer): Boolean;

    class function CreateDirectory(const PathName: String): Boolean; virtual; abstract;
    class function RemoveDirectory(const PathName: String): Boolean; virtual; abstract;
    class function OpenTextFile(const FileName: string; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustExist: Boolean = False): ITextFile;
    class function CreateTextFile(const FileName: string; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustCreate: Boolean = True): ITextFile;
    class function Exists(const FileName: String): Boolean; virtual; abstract;
    class function DirectoryExists(const Path: String): Boolean; virtual; abstract;
    class function GetList(const Path: String; Recursive: Boolean = false; const Listener: IListEvents = nil): IFileInfoList; overload;
    class function GetList(const Path: String; const Attributes: TFileAttributes; Recursive: Boolean = false; const Listener: IListEvents = nil): IFileInfoList; overload; virtual; abstract;
    class function ReadDirectory(const PathName: String; const Include: TFileAttributes = []; const Exclude: TFileAttributes = []): IDirectoryReader; overload;
    class function ReadDirectory(const PathName: String; Recursive: Boolean; const Include: TFileAttributes = []; const Exclude: TFileAttributes = []): IFileInfoList; overload;
    class function GetInfo(const FileName: String): IFileInfo; virtual;
    class function FillInfo(const FileName: String; var Info: IFileInfoDef): Boolean; virtual; abstract;
    class function MoveFile(const OldName, NewName: String): Boolean; virtual; abstract;
    class function CopyFile(const Source, Dest: String): Boolean; virtual; abstract;
    class function DeleteFile(const FileName: String): Boolean; virtual; abstract;
    class function Delete(const FilePath: String; Recursive: Boolean = true): Boolean; virtual; abstract;
    class function Notification(const Path: String; Subtree: Boolean = false; Filter: TFilesystemChangeFilters = [cfLastWrite]): IFilesystemChangeNotification;
    class function TranslatePath(const Path: String; Old: Char = '/'; New: Char = CPathSeparator): String; virtual; abstract;
    class function GetFilePath(const FileName: String): String; virtual; abstract;
    class function GetFileName(const FileName: String; IncludeExt: Boolean = true): String; virtual; abstract;
    class function GetFileExt(const FileName: String; IncludeDot: Boolean = true): String; virtual; abstract;
    class function ChangeFileExt(const FileName, Extension: string): string; virtual; abstract;
    class function ExpandPath(const FileName: String): String; virtual; abstract;
    class function FileInfo(const FileName: String): IFileInfoDef; virtual;
    class function FileInfoList(Locked: Boolean = false): IFileInfoList; virtual;
    class function FileComparable(Kind: TFileInfoKind = ikName; const Value: String = ''): IComparable;
    class function FileComparator(Kind: TFileInfoKind = ikName): IComparator;
    class function ForceDirectories(Path: String): Boolean;
    class function AddSlash(const FileName: String; Separator: Char = CPathSeparator): String;
    class function DeleteSlash(const FileName: String; Separator: Char = CPathSeparator): String;
    class function IsRelative(const FileName: string; Separator: Char = CPathSeparator): Boolean;
    class function IsAbsolute(const FileName: string; Separator: Char = CPathSeparator): Boolean;
    class function MakeRelative(const Path: string; const Root: string; Separator: Char = CPathSeparator): string;
  end;

implementation

uses
  SysUtils,

  SilBtStr,
  SilBtInt,
  SilOsClasses,
  SilBtDateTime,
  SilLtSort,

  SilOkFileInfo,
  SilOmTextFile;

class function SilFile.OpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExist: Boolean): IFile;
begin
  Result := TSilOsFile.OpenFile(FileName, Access, Share, MustExist);
end;

class function SilFile.CreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): IFile;
begin
  Result := TSilOsFile.CreateFile(FileName, Access, Share, MustCreate);
end;

class function SilFile.CreateTextFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): ITextFile;
begin
  Result := TTextFile.CreateFile(FileName, Access, Share, MustCreate);
end;

class function SilFile.OpenTextFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExist: Boolean): ITextFile;
begin
  Result := TTextFile.OpenFile(FileName, Access, Share, MustExist);
end;

class function SilFile.Notification(const Path: String; Subtree: Boolean; Filter: TFilesystemChangeFilters): IFilesystemChangeNotification;
begin
  Result := TSilOsFilesystemNotifier.Create(Path, Subtree, Filter);
end;

class function SilFile.FileComparable(Kind: TFileInfoKind; const Value: String): IComparable;
begin
  Result := TSilFileInfoCompare.Create(Kind, Value);
end;

class function SilFile.FileComparator(Kind: TFileInfoKind): IComparator;
begin
  Result := TSilFileInfoCompare.Create(Kind);
end;

class function SilFile.FileInfo(const FileName: String): IFileInfoDef;
begin
  Result := TSilFileInfo.Create(FileName);
end;

class function SilFile.FileInfoList(Locked: Boolean): IFileInfoList;
begin
  Result := TSilFileInfoList.Create(Locked);
end;

class function SilFile.ReadDirectory(const PathName: String; const Include, Exclude: TFileAttributes): IDirectoryReader;
begin
  Result := TSilOsDirectoryReader.Create(PathName, Include, Exclude);
end;

class function SilFile.ReadDirectory(const PathName: String; Recursive: Boolean; const Include, Exclude: TFileAttributes): IFileInfoList;
var
  path: string;
  mask: string;

  procedure DoRead(const current: string);
  var
    reader: IDirectoryReader;
    info: IFileInfo;
    e: IEnumerator;
  begin
    reader := TSilOsDirectoryReader.Create(current, Include, Exclude);

    while reader.Read do
      while reader.Recent.Enumerate(e, info) do
      begin
        if (length(mask) = 0) or Str.WildCard(info.name, mask) then
          result.add(info);

        if recursive and (SilOiFile.faDirectory in info.Attributes) then
          DoRead(reader.path + info.name);
      end;
  end;

var
  i: integer;
begin
  i := Str.LastPos(CPathSeparator, PathName);
  
  if i > 0 then
  begin
    path := Str.Copy(PathName, 1, i - 1);
    mask := Str.Copy(PathName, i + 1);
  end else
  begin
    path := PathName;
    mask := '';
  end;

  result := TSilFileInfoList.Create;
  DoRead(path);
end;

class function SilFile.ForceDirectories(Path: String): Boolean;
begin
  Path := Str.TrimRight(Path, CPathSeparator);

  if (Length(Path) > 0) and not DirectoryExists(Path) then
    Result := ForceDirectories(GetFilePath(Path)) and CreateDirectory(Path) else
    Result := true;
end;

class function SilFile.AddSlash(const FileName: String; Separator: Char): String;
begin
  if Str.LastChar(FileName) = Separator then
    Result := FileName else
    Result := FileName + Separator;
end;

class function SilFile.DeleteSlash(const FileName: String; Separator: Char): String;
begin
  if Str.LastChar(FileName) <> Separator then
    Result := FileName else
    Result := Str.TrimRight(FileName, Separator);
end;

class function SilFile.IsRelative(const FileName: string; Separator: Char): Boolean;
var
  I: Integer;
  S: string;
begin
  I := Str.Pos(Separator, FileName);
  if (I <> 0) then
  begin
    S := Str.Copy(FileName, 1, I);

    if not Str.IsEmpty(S) then
      Result := (Str.Len(S) > 1) and (S[2] <> ':') else
      Result := False;

  end else
    Result := True;
end;

class function SilFile.IsAbsolute(const FileName: string; Separator: Char): Boolean;
begin
  Result := not IsRelative(FileName, Separator);
end;

class function SilFile.MakeRelative(const Path, Root: string; Separator: Char): string;
begin
  if IsAbsolute(Path, Separator) then
  begin
    Result := AddSlash(Root, Separator) + Str.Copy(Path, 2);
  end else
    Result := AddSlash(Root, Separator) + Path;
end;

class function SilFile.GetInfo(const FileName: String): IFileInfo;
var
  Info: IFileInfoDef;
begin
  Info := TSilFileInfo.Create;
  FillInfo(FileName, Info);
  Result := Info;
end;

class function SilFile.GetList(const Path: String; Recursive: Boolean; const Listener: IListEvents): IFileInfoList;
begin
  Result := GetList(Path, [], Recursive, Listener);
end;

class function SilFile.StampFileName(const FileName: string; const Format: String): String;
begin
  Result := ChangeFileExt(FileName, '') + DateTime.ToStr(DateTime.Now, Format) + GetFileExt(FileName);
end;

class function SilFile.StampLimitCount(const FileName: string; MaxCount: Integer): Boolean;
var
  Files: IFileInfoList;
  name: string;
begin
  Result := false;

  if MaxCount > 0 then
  begin
    name := ChangeFileExt(FileName, '') + '.*' + GetFileExt(FileName);
    Files := GetList(name);
    SortTool.Default(Files);

    while Files.Count > MaxCount do
    begin
      DeleteFile(Files[0].FullName);
      Files.Delete(0);
      Result := true;
    end;
  end;
end;

class function SilFile.StampLast(const FileName: string; out LastName: String): Boolean;
var
  Files: IFileInfoList;
begin
  if Str.DelimiterPos('*?', FileName) > 0 then
    Files := GetList(FileName)
  else
    Files := GetList(ChangeFileExt(FileName, '') + '.*' + GetFileExt(FileName));

  SortTool.Default(Files);

  Result := Files.Count > 0;

  if Result then
    LastName := Files.Last.FullName;
end;

class function SilFile.LogCreate(const FileName: string; MaxCount: Integer): String;
var
  Files: IFileInfoList;
  FileInfo: IFileInfo;
  i, Count: Integer;
  Digits: Byte;
  FullName, Ext: String;
  Error: Boolean;
begin
  Digits := Length(Int.ToStr(MaxCount));
  Ext := GetFileExt(FileName);
  Error := false;
  Count := 0;

  if MaxCount > 0 then
  begin
    if Str.DelimiterPos('*?', FileName) > 0 then
      Files := GetList(FileName)
    else
      Files := GetList(ChangeFileExt(FileName, '') + '.*' + Ext);

    SortTool.Default(Files);
    Count := Files.Count;

    while Count > MaxCount do
    begin
      DeleteFile(Files[Count - 1].FullName);
      Files.Delete(Count - 1);
      Dec(Count);
    end;

      while Count > 0 do
      begin
        FileInfo := Files[Count - 1];
        FullName := FileInfo.FullName;
        i := Str.LastPos('\', FullName);
        i := Str.Pos('.', FullName, i + 1);

        if not MoveFile(FullName, Str.Format('%s.%s%s', [Str.Copy(FullName, 1, i - 1), Str.Zero(Count, Digits), Ext])) then
        begin
          Error := true;
          break;
        end;

        Dec(Count);
      end;
  end;

  if Error then
    Result := Str.Format('%s.%s%s', [ChangeFileExt(FileName, ''), Str.Zero(Count, Digits), Ext])
  else
    Result := Str.Format('%s.%s%s', [ChangeFileExt(FileName, ''), Str.Zero(0, Digits), Ext]);
end;

class function SilFile.LogLimitCount(const FileName: string; MaxCount: Integer): Boolean;
var
  Files: IFileInfoList;
  Pos: Integer;
begin
  Result := false;

  if MaxCount > 0 then
  begin
    Pos := Str.Pos('.', FileName);

    if Str.DelimiterPos('*?', FileName) > 0 then
      Files := GetList(FileName)
    else
      Files := GetList(Str.Copy(FileName, 1, Pos) + '.*' + GetFileExt(FileName));
      
    SortTool.Default(Files);

    while Files.Count > MaxCount do
      try
        DeleteFile(Files.Last.FullName);
        Files.Delete(Files.Count - 1);
        Result := true;
      except
        Result := false;
      end;
  end;
end;

end.
