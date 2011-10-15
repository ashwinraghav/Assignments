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

unit SilOmDirectoryReader;

{$I Defines.inc}

interface

uses
  libc,
  SysUtils,

  SilOsTypes,
  SilOiFile,
  SilOkDirectoryReader;

type
  TSilLinuxDirectoryReader = class (TSilDirectoryReader)
  private
    FSearch: PDir;
    FEntry: PDirent;
    FFirst: Boolean;
    function DoCreateInfo: IFileInfoDef;
  protected
    function DoRead: IFileInfoDef; override;
  public
    constructor Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char = CPathSeparator); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilOsTool,
  SilOjFile,
  SilOfFile;

{ TSilLinuxDirectoryReader }

constructor TSilLinuxDirectoryReader.Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char = CPathSeparator);
begin
  inherited;
  FFirst := true;
end;

destructor TSilLinuxDirectoryReader.Destroy;
begin
  libc.closedir(FSearch);
  inherited;
end;

function TSilLinuxDirectoryReader.DoCreateInfo: IFileInfoDef;
var
  Name: String;
  Stat: TStatBuf;
begin
  Name := FPath + FEntry.d_name;
  Result := OsFile.FileInfo(Name);

  if libc.stat(PChar(Name), @Stat) >= 0 then
  begin
    Result.Time := SilOfFile.FileTimeToDateTime(Stat.st_mtime);
    Result.Attributes := SilOfFile.LinuxAttrToAttributes(Stat.st_mode);
    Result.Size := Stat.st_size;
  end;
end;

function TSilLinuxDirectoryReader.DoRead: IFileInfoDef;
var
  Find: Boolean;
  Name: String;
begin
  if FFirst then
  begin
    FFirst := false;
    FSearch := libc.opendir(PChar(GetPath));
  end;

  if Assigned(FSearch) then
  begin
    FEntry := libc.readdir(FSearch);
    Find := Assigned(FEntry);
  end else
    Find := false;

  if Find then
  begin
    Name := FEntry.d_name;

    if (Name <> '.') and (Name <> '..') then
      Result := DoCreateInfo else
      Result := DoRead;
  end else
    Result := nil;
end;

end.
