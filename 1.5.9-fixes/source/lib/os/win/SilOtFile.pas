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
  SilLiEnumerator,
  SilLiList,
  SilOjFile,
  SilOsTypes,
  SilOiHandle,
  SilOiFile;

type
  WindowsFile = class (SilFile)
    class function CreateDirectory(const PathName: String): Boolean; override;
    class function RemoveDirectory(const PathName: String): Boolean; override;
    class function Exists(const FileName: String): Boolean; override;
    class function DirectoryExists(const Path: String): Boolean; override;
    class function GetList(const Path: String; const Attributes: TFileAttributes; Recursive: Boolean = false; const Listener: IListEvents = nil): IFileInfoList; override;
    class function FillInfo(const FileName: String; var Info: IFileInfoDef): Boolean; override;
    class function MoveFile(const OldName, NewName: String): Boolean; override;
    class function CopyFile(const Source, Dest: String): Boolean; override;
    class function DeleteFile(const FileName: String): Boolean; override;
    class function Delete(const FilePath: String; Recursive: Boolean = true): Boolean; override;
    class function TranslatePath(const Path: String; Old: Char = '/'; New: Char = CPathSeparator): String; override;
    class function GetFilePath(const FileName: String): String; override;
    class function GetFileName(const FileName: String; IncludeExt: Boolean = true): String; override;
    class function GetFileExt(const FileName: String; IncludeDot: Boolean = true): String; override;
    class function ChangeFileExt(const FileName, Extension: string): string; override;
    class function ExpandPath(const FileName: String): String; override;
  end;

implementation

uses
  SilBtStr,
  SilLtConnection,
  SilOfFile,
  SilOtError,
  SilOmFileInfo,
  SilOmTextFile,
  SilOsClasses,
  SilOkFileInfo;

class function WindowsFile.Exists(const FileName: String): Boolean;
begin
  Result := SilOfFile.Exists(FileName);
end;

class function WindowsFile.GetList(const Path: String; const Attributes: TFileAttributes; Recursive: Boolean; const Listener: IListEvents): IFileInfoList;

  procedure DoFind(const Path: String; Attr: Integer);
  var
    iPos, Find: Integer;
    Search: TSearchRec;
    sPath: String;
    Info: IFileInfoDef;
    WAttr: TFileAttributes;
  begin
    Find := FindFirst(Path, Attr, Search);
    iPos := Str.LastPos(CPathSeparator, Path);

    while Find = 0 do
    begin
      WAttr := SilOfFile.WindowsAttrToAttributes(Search.Attr);

      if (Search.Name <> '.') and (Search.Name <> '..') and ((Attr = faAnyFile) or (WAttr * Attributes <> [])) then
      begin
        sPath := Str.Copy(Path, 1, iPos);

        Info := TSilWindowsFileInfo.Create(sPath + Search.Name, FileDateToDateTime(Search.Time), SilOfFile.WindowsAttrToAttributes(Search.Attr), Search.Size);
        Result.Add(Info);

        if (faDirectory in Info.Attributes) and Recursive then
          DoFind(sPath + Search.Name + Str.Copy(Path, iPos), Attr);
      end;

      Find := FindNext(Search);
    end;
    FindClose(Search);
  end;

var
  WinAttr: Integer;
begin
  Result := TSilFileInfoList.Create(true);

  if Attributes <> [] then
    WinAttr := SilOfFile.AttributesToWindowsAttr(Attributes) else
    WinAttr := faAnyFile;

  if Listener <> nil then Sink.Connect(Result, Listener);
  DoFind(Path + Str.IIf(Str.LastPos(CPathSeparator, Path) = Length(Path), '*.*', ''), WinAttr);
end;

class function WindowsFile.FillInfo(const FileName: String; var Info: IFileInfoDef): Boolean;
var
  Find: Integer;
  Search: TSearchRec;
begin
  Find := FindFirst(FileName, faAnyFile, Search);
  Info.Name := FileName;
  Result := Find = 0;

  if Result then
  begin
    Info.Time := FileDateToDateTime(Search.Time);
    Info.Attributes := SilOfFile.WindowsAttrToAttributes(Search.Attr);
    Info.Size := Search.Size;
    FindClose(Search);
  end;
end;

class function WindowsFile.MoveFile(const OldName, NewName: String): Boolean;
begin
  Result := SilOfFile.MoveFile(OldName, NewName);
end;

class function WindowsFile.CopyFile(const Source, Dest: String): Boolean;
begin
  Result := SilOfFile.CopyFile(Source, Dest);
end;

class function WindowsFile.CreateDirectory(const PathName: String): Boolean;
begin
  Result := SilOfFile.CreateDirectory(PathName);
end;

class function WindowsFile.RemoveDirectory(const PathName: String): Boolean;
begin
  Result := SilOfFile.RemoveDirectory(PathName);
end;

class function WindowsFile.TranslatePath(const Path: String; Old, New: Char): String;
begin
  Result := SilOfFile.TranslatePath(Path, Old, New);
end;

class function WindowsFile.DeleteFile(const FileName: String): Boolean;
begin
  Result := SilOfFile.DeleteFile(FileName);
end;

class function WindowsFile.ChangeFileExt(const FileName, Extension: string): string;
begin
  Result := SilOfFile.ChangeFileExt(FileName, Extension);
end;

class function WindowsFile.GetFileName(const FileName: String; IncludeExt: Boolean): String;
begin
  Result := SilOfFile.GetFileName(FileName, IncludeExt);
end;

class function WindowsFile.GetFileExt(const FileName: String; IncludeDot: Boolean): String;
begin
  Result := SilOfFile.GetFileExt(FileName, IncludeDot);
end;

class function WindowsFile.GetFilePath(const FileName: String): String;
begin
  Result := SilOfFile.GetFilePath(FileName);
end;

class function WindowsFile.ExpandPath(const FileName: String): String;
begin
  Result := SilOfFile.ExpandPath(FileName);
end;

class function WindowsFile.DirectoryExists(const Path: String): Boolean;
begin
  Result := SilOfFile.DirectoryExists(Path);
end;

class function WindowsFile.Delete(const FilePath: String; Recursive: Boolean): Boolean;
var
  List: IFileInfoList;
  Enum: IEnumerator;
  Item: IFileInfoDef;
begin
  List := GetList(FilePath);
  Result := false;

  while List.Enumerate(Enum, Item) do
  begin
    if [faHidden, faReadOnly, faSysFile] * Item.Attributes <> [] then
      Item.Attributes := Item.Attributes - [faHidden, faReadOnly, faSysFile];

    if faDirectory in Item.Attributes then
    begin
      if Recursive then WindowsFile.Delete(Item.FullName + '\');
      Result := RemoveDirectory(Item.FullName);
    end else
      Result := DeleteFile(Item.FullName);
  end;
end;

end.
