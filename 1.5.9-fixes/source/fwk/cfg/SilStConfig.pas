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

unit SilStConfig;

{$I Defines.inc}

interface

uses
  Sil,
  SilUrl,
  SilXml,
  SilSiConfig;

type
  SilConfigTool = class (SilSiConfig.SilConfigTool)
    class function Open(const Url: string; CanCreate: Boolean = False): IConfiguration; override;
    class function Open(const Url: IUrl; CanCreate: Boolean = False): IConfiguration; override;
    class function Open(const Xml: IXmlTree; CanCreate: Boolean = False): IConfiguration; override;
    class function Open(const Xml: IXmlTag; CanCreate: Boolean = False): IConfiguration; override;
    class function Open(const Ini: IValueKeys; CanCreate: Boolean = False): IConfiguration; override;
    class function Open(const Key: INamedKey; CanCreate: Boolean = False): IConfiguration; override;
    class procedure Save(const Config: IConfiguration); override;
    class procedure Save(const Config: IConfiguration; const Target: IUrl); override;
    class procedure Save(const Config: IConfiguration; const Target: string); override;
  end;

implementation

uses
  SilSiUrl,
  SilSmXmlConfig,
  SilSmRegistryConfig,
  SilSmIniFileConfig;

{ SilConfigTool }

class function SilConfigTool.Open(const Url: string; CanCreate: Boolean): IConfiguration;
begin
  Result := Open(SilUrl.Url.Create(Url), CanCreate);
end;

class function SilConfigTool.Open(const Url: IUrl; CanCreate: Boolean): IConfiguration;
var
  Protocol: String;
begin
  if not Assigned(Url) then
    raise Error.Create('falta la url'); // localizar

  Protocol := Url.Protocol.Text;

  if Str.IsEmpty(Protocol) then
    raise Error.Create('falta el protocolo'); // localizar

  if Sil.Text.Compare(Protocol, 'xml') = 0 then
    Result := TSilXmlConfig.Create(Url, CanCreate)
  else
  if Sil.Text.Compare(Protocol, 'reg') = 0 then
    Result := TSilRegistryConfig.Create(Url, CanCreate) else
  if Sil.Text.Compare(Protocol, 'ini') = 0 then
    Result := TSilIniFileConfig.Create(Url, CanCreate);
end;

class function SilConfigTool.Open(const Xml: IXmlTree; CanCreate: Boolean): IConfiguration;
begin
end;

class function SilConfigTool.Open(const Xml: IXmlTag; CanCreate: Boolean): IConfiguration;
begin
end;

class function SilConfigTool.Open(const Ini: IValueKeys; CanCreate: Boolean): IConfiguration;
begin
end;

class function SilConfigTool.Open(const Key: INamedKey; CanCreate: Boolean): IConfiguration;
begin
end;

class procedure SilConfigTool.Save(const Config: IConfiguration; const Target: string);
begin

end;

class procedure SilConfigTool.Save(const Config: IConfiguration; const Target: IUrl);
begin

end;

class procedure SilConfigTool.Save(const Config: IConfiguration);
begin

end;

end.
 