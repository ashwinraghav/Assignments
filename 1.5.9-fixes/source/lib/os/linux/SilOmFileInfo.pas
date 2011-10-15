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
  TSilLinuxFileInfo = class (TSilBaseFileInfo)
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
  end;

implementation

uses
  SysUtils,
  SilOfFile;

{ TSilLinuxFileInfo }

constructor TSilLinuxFileInfo.Create(const Handle: IHandle; const FileName: String);
begin
  inherited Create(Handle, FileName);
  FHandle := Handle.Value;
end;

function TSilLinuxFileInfo.GetAttributes: TFileAttributes;
begin
  SilOfFile.GetAttributes(GetFullName, Result);
end;

function TSilLinuxFileInfo.GetSize: LongWord;
begin
  SilOfFile.GetFileSize(FHandle, Result);
end;

function TSilLinuxFileInfo.GetTime: TDateTime;
begin
  SilOfFile.GetFileTime(FHandle, Result);
end;

procedure TSilLinuxFileInfo.SetAttributes(Value: TFileAttributes);
begin
  SilOfFile.SetAttributes(GetFullName, Value);
end;

procedure TSilLinuxFileInfo.SetSize(Value: LongWord);
begin
end;

procedure TSilLinuxFileInfo.SetTime(Time: TDateTime);
begin
  SilOfFile.SetFileTime(FHandle, Time);
end;

end.
