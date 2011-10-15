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

unit SilStUrl;

interface

uses
  Sil,
  SilSiUrl;

type
  UrlTool = class(Tool)
    class function Create(const Text: string = ''): IUrl; overload; virtual;
    class function LocalFile(const PathName: string; const Protocol: string = ''): IUrl; overload;  
    class function Create(
                     const AProtocol: string;
                     const AServer: string;
                     const AUser: string = '';
                     const APassword: string = ''): IUrl; overload;
  end;

  UrlDbTool = class(Tool)
    class function Create(const Text: string = ''): IUrlDb; reintroduce; overload;
    class function Create(
                     const AProtocol: string;
                     const AServer: string;
                     const ADatabase: string = '';
                     const ATable: string = '';
                     const AUser: string = '';
                     const APassword: string = ''): IUrlDb; overload;
  end;

implementation

uses
  SilSmUrl;

{ UrlTool }

class function UrlTool.Create(const Text: string): IUrl;
begin
  result := TSilUrl.Create( Text );
end;

class function UrlTool.Create(const AProtocol, AServer, AUser, APassword: string): IUrl;
begin
  Result := TSilUrl.Create();
  Result.Protocol.Text := AProtocol;
  Result.Address.Host.Server := AServer;
  Result.Address.Login.User := AUser;
  Result.Address.Login.Password := APassword;
end;

class function UrlTool.LocalFile(const PathName, Protocol: string): IUrl;
begin
  Result := Create();
  Result.Protocol.Text := Protocol;
  Result.Path.Text := PathName;    
end;

{ UrlDbTool }

class function UrlDbTool.Create(const Text: string): IUrlDb;
begin
  Result := TUrlDb.Create(Text);
end;

class function UrlDbTool.Create(const AProtocol, AServer, ADatabase, ATable, AUser, APassword: string): IUrlDb;
begin
  Result := TUrlDb.Create();
  Result.Protocol.Text := AProtocol;
  Result.Address.Host.Server := AServer;
  Result.Address.Login.User := AUser;
  Result.Address.Login.Password := APassword;
  Result.Database := ADatabase;
  Result.Table := ATable;
end;

end.
