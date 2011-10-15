{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit UfConfig;

interface

uses
  Sil,
  SilXml;

const
  FBAGENT_GUI_TITLE   = 'Firebird SQL Agent';
  FBAGENT_GUI_VERSION = '1.0.14';
  FBAGENT_SIGNATURE   = 'firebirdsql.agent';

const
  CIsolation: array [0..3] of String = (
    // snapshot
    'Concurrency'#13'NoWait',
    // read committed
    'CommittedVersion'#13'NoWait',
    // read only table stability
    'Read'#13'Consistency',
    // read write table stability
    'Write'#13'Consistency' );

const
  CIBXIsolation: array [0..3] of String = (
    // snapshot
    'concurrency'#13'nowait',
    // read committed
    'rec_version'#13'NoWait',
    // read only table stability
    'read'#13'consistency',
    // read write table stability
    'write'#13'consistency' );

function Config: IXmlTree;
procedure ReloadConfig;
procedure SaveConfig;

implementation

uses SilOjFile, SilOeProcess;

var
  MConfigFile: String;
  MTree: IXmlTree;

{
  fbagent.exe -config=D:\marianop\dev\delphi\lib\sil\1.5.2\programs\projects\fbagent\2\fbagent.xml
}

procedure ReloadConfig;
var
  AlternateConfig: String;
begin
  AlternateConfig := Sil.OS.Process.Params.Options['config'];

  if Str.IsEmpty(AlternateConfig) then
    MConfigFile := Sil.OS.Process.Current.Info.Path else
    MConfigFile := AlternateConfig;

  if Str.IsEmpty(Sil.OS.FileSystem.GetFileName(MConfigFile)) then
    MConfigFile := Sil.OS.FileSystem.AddSlash(MConfigFile) + 'fbagent.xml';

  MTree := SilXml.Tool.ReadFile(MConfigFile, nil, fmAccessReadWrite, fmShareRead, false);
end;

procedure SaveConfig;
begin
  if Assigned(MTree) and MTree.Modified then
    SilXml.Tool.WriteFile(MTree, MConfigFile);
end;

function Config: IXmlTree;
begin
  if not Assigned(MTree) then ReloadConfig;
  Result := MTree;
end;

{
Transaction Model
  Unespecified,
  Concurrency,          concurrency
  CommittedNoVersion,   no_rec_version
  CommittedVersion,     rec_version
  Consistency           consistency

Transaction Access
  Unespecified,
  ReadOnly,             read
  ReadWrite             write

Transaction Resolution
  Unespecified,
  Wait,                 wait
  NoWait                nowait
}

initialization

finalization
  SaveConfig;

end.
