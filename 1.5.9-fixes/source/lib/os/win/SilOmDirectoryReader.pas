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
  SysUtils,

  SilOsTypes,
  SilOiFile,
  SilOkDirectoryReader;

type
  TSilWindowsDirectoryReader = class (TSilDirectoryReader)
  private
    FSearch: TSearchRec;
    FFirst: Boolean;
    function DoCreateInfo: IFileInfoDef;
  protected
    function DoRead: IFileInfoDef; override;
  public
    constructor Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char = CPathSeparator); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilOsTool,
  SilOjFile,
  SilOfFile;

{ TSilWindowsDirectoryReader }

constructor TSilWindowsDirectoryReader.Create(const PathName: String; const Include, Exclude: TFileAttributes; Separator: Char);
begin
  inherited;
  FFirst := true;
end;

destructor TSilWindowsDirectoryReader.Destroy;
begin
  FindClose(FSearch);
  inherited;
end;

function TSilWindowsDirectoryReader.DoCreateInfo: IFileInfoDef;
begin
  Result := OsFile.FileInfo(FPath + FSearch.Name);
  Result.Time := FileDateToDateTime(FSearch.Time);
  Result.Attributes := SilOfFile.WindowsAttrToAttributes(FSearch.Attr);
  Result.Size := FSearch.Size;
end;

function TSilWindowsDirectoryReader.DoRead: IFileInfoDef;
var
  Find: Boolean;
begin
  Result := nil;

  while not Assigned(Result) do
  begin
    if FFirst then
    begin
      FFirst := false;
      Find := FindFirst(GetPathName, SysUtils.faAnyFile, FSearch) = 0;
    end else
      Find := FindNext(FSearch) = 0;

    if Find then
    begin
      if (FSearch.Name <> '.') and (FSearch.Name <> '..') then
        Result := DoCreateInfo;
    end else
      Break;
  end;
end;

end.
