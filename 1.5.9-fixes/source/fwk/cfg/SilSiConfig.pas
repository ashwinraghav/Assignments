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

unit SilSiConfig;

{$include Defines.inc}

interface                            

uses
  Sil,
  SilLiValue,
  SilUrl,
  SilXml;

type
  IConfiguration = interface;
  IConfigNode = interface;
  IConfigData = interface;

  IConfiguration = interface
    ['{B19B6C83-4CD6-4C0B-852A-8314027CB29A}']
    function GetSource: IUrl;
    function GetContent: IConfigNode;    
    property Source: IUrl read GetSource;
    property Content: IConfigNode read GetContent;    
  end;

  IConfigNode = interface
    ['{8D351D7C-101C-45D4-9A93-F899A25F1EF4}']
    function GetName: string;
    function GetData: IConfigData;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IConfigNode): Boolean; overload;
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Item: IConfigNode): Boolean; overload; 
    function Get(const Name: string; CanCreate: Boolean = False): IConfigNode;
    function Remove(const Name: string; Recursive: Boolean = True): Boolean;
    procedure Clear(Recursive: Boolean = True);
    property Name: string read GetName;
    property Data: IConfigData read GetData;
  end;

  RConfigData = record
    Name: string;
    Item: IVariable;
  end;

  IConfigData = interface
    ['{3516DD1F-6704-4F35-AB54-B54738C42A2C}']
    function Find(const Name: string): Boolean; overload;
    function Find(const Name: string; out Value: IVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: RConfigData): Boolean; overload;
    function Get(const Name: string; CanCreate: Boolean = False): IVariable;
    function Remove(const Name: string): Boolean;
    procedure Clear;
  end;

//  N.Get('prueba', True).Get('dato', True).AsInteger.Get(4637648);
//  N.Get('prueba', True).Get('dato').AsInteger.Value := 4637648;
//  K.Keys.Get('prueba', true).Values.ReadInteger('dato', true, 4637648);
//  K.Keys.Get('prueba', true).Values.WriteInteger('dato', 4637648);


(*)
  URLs:
    xml://d:/dir/dir/dir/file.ext
    reg://[machine/]system/key/key/key
    ini://d:/dir/dir/file.ext
(*)

type
  SilConfigType = class of SilConfigTool;
  SilConfigTool = class (Tool)
    class function Open(const Url: string; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class function Open(const Url: IUrl; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class function Open(const Xml: IXmlTree; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class function Open(const Xml: IXmlTag; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class function Open(const Ini: IValueKeys; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class function Open(const Key: INamedKey; CanCreate: Boolean = False): IConfiguration; overload; virtual; abstract;
    class procedure Save(const Config: IConfiguration); overload; virtual; abstract;
    class procedure Save(const Config: IConfiguration; const Target: IUrl); overload; virtual; abstract;
    class procedure Save(const Config: IConfiguration; const Target: string); overload; virtual; abstract;
  end;

implementation
end.
