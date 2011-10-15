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

unit SilOkDirectoryReader;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilOiFile,
  SilLkInterfaced;

type
  TSilBaseDirectoryReader = class (
    // extends
    TSilInterfacedObject,
    // implements
    IDirectoryReader)
  protected
    FPath: String;
    FMask: String;
    FRecent: IFileInfoList;
    FList: IFileInfoList;
    FIsComplete: Boolean;
    FBufferSize: LongWord;
    FInclude: TFileAttributes;
    FExclude: TFileAttributes;
  protected // IDirectoryReader
    function GetPath: String;
    function GetMask: String;
    function GetPathName: String;
    function GetRecent: IFileInfoList;
    function GetList: IFileInfoList;
    function GetIsComplete: Boolean;
    function GetBufferSize: LongWord;
    procedure SetBufferSize(Value: LongWord);
    function Read: Boolean; virtual; abstract;
    function ReadAll: Boolean;
  public
    constructor Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char = CPathSeparator); virtual;
    destructor Destroy; override;
  end;

  TSilDirectoryReader = class (TSilBaseDirectoryReader)
  protected
    function DoRead: IFileInfoDef; virtual; abstract;
  protected // IDirectoryReader
    function Read: Boolean; override;
  end;

implementation

uses
  SilBtStr,
  SilOsTool,
  SilOjFile,
  SilOfFile;

{ TSilBaseDirectoryReader }

constructor TSilBaseDirectoryReader.Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char);
var
  i: Integer;
begin
  inherited Create;

  FInclude := Include;
  FExclude := Exclude;
  FList := OsFile.FileInfoList(true);
  FBufferSize := 0;
  FRecent := OsFile.FileInfoList(true);
  i := Str.LastPos(Separator, PathName);
  FMask := Str.Copy(PathName, i + 1);

  if (Str.Pos('?', FMask) = 0) and (Str.Pos('*', FMask) = 0) then
  begin
    FPath := OsFile.AddSlash(PathName, Separator);
    FMask := '';
  end else
    FPath := OsFile.AddSlash(Str.Copy(PathName, 1, i));
end;

destructor TSilBaseDirectoryReader.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilBaseDirectoryReader.GetBufferSize: LongWord;
begin
  Result := FBufferSize;
end;

function TSilBaseDirectoryReader.GetIsComplete: Boolean;
begin
  Result := FIsComplete;
end;

function TSilBaseDirectoryReader.GetList: IFileInfoList;
begin
  Result := FList;
end;

function TSilBaseDirectoryReader.GetMask: String;
begin
  Result := Str.Iif(Str.IsEmpty(FMask), '*.*', FMask);
end;

function TSilBaseDirectoryReader.GetPath: String;
begin
  Result := FPath;
end;

function TSilBaseDirectoryReader.GetPathName: String;
begin
  Result := GetPath + GetMask;
end;

procedure TSilBaseDirectoryReader.SetBufferSize(Value: LongWord);
begin
  FBufferSize := Value;
end;

function TSilBaseDirectoryReader.GetRecent: IFileInfoList;
begin
  if FBufferSize > 0 then
    Result := FRecent else
    Result := FList;
end;

function TSilBaseDirectoryReader.ReadAll: Boolean;
begin
  while Read do;
  Result := FList.Count > 0;
end;

{ TSilDirectoryReader }

function TSilDirectoryReader.Read: Boolean;
var
  FileInfo: IFileInfo;
  Attr: TFileAttributes;
  i: LongWord;
begin
  i := 0;
  FRecent.Clear;

  while (FBufferSize = 0) or (i < FBufferSize) do
  begin
    FileInfo := DoRead;
    FIsComplete := not Assigned(FileInfo);

    if not FIsComplete then
    begin
      Attr := FileInfo.Attributes;

      if ((FInclude = []) or (Attr * FInclude <> [])) and ((FExclude = []) or (Attr * FExclude = []))
        and ((Length(FMask) = 0) or Str.WildCard(FileInfo.Name, FMask)) then
      begin
        FList.Add(FileInfo);
        if FBufferSize > 0 then FRecent.Add(FileInfo);
      end;
    end else
      Break;

    Inc(i);
  end;

  Result := i > 0;
end;

end.

