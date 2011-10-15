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

unit SilSiHttpData;

interface

uses
  Sil,
  SilUrl,
  SilSiHttpTypes,
  SilSiHttpTags;

type
  IHttpMessage = interface;
  IHttpFiler = interface;
  IHttpRequest = interface;
  IHttpRequestParams = interface;
  IHttpRequestUri = interface;
  IHttpResponse = interface;
  IHttpResponseStatus = interface;
  IHttpEntity = interface;

  IHttpMessage = interface (IHttpObject)
    ['{7E7B88DC-C476-4F5B-8261-05ACCA0FD334}']
    function GetHeader: IHttpGeneralTags;
    function GetEntity: IHttpEntity;
    property Header: IHttpGeneralTags read GetHeader;
    property Entity: IHttpEntity read GetEntity;
  end;

  IHttpFiler = interface
    ['{679576D0-F330-4A61-ACF1-D359810AAACC}']
    function Read(const Stream: IStream; out Request: IHttpRequest): Boolean; overload;
    function Read(const Stream: IStream; out Response: IHttpResponse): Boolean; overload; 
    procedure Write(const Stream: IStream; const Request: IHttpObject); overload;
    procedure Write(const Stream: IStream; const Request: IHttpRequest); overload;
    procedure Write(const Stream: IStream; const Response: IHttpResponse); overload;
    function ToStr(const Request: IHttpObject): string; overload;
    function ToStr(const Request: IHttpRequest): string; overload;
    function ToStr(const Response: IHttpResponse): string; overload;
  end;

  IHttpRequest = interface (IHttpMessage)
    ['{9B39DFB6-5569-4ECB-AF58-2EE039FC111A}']
    function GetRequestTags: IHttpRequestTags;
    function GetRequest: IHttpRequestParams;
    property Tags: IHttpRequestTags read GetRequestTags;
    property Request: IHttpRequestParams read GetRequest;
  end;

  IHttpRequestUri = interface (IHttpObject)
    ['{D0EB43F1-F3A4-4BFE-AC3A-2A113B8F8638}']
    function GetUrl: IUrl;
    property Url: IUrl read GetUrl;
  end;

  IHttpRequestParams = interface (IHttpObject)
    ['{EAE0D40D-B773-43BC-9E45-D31C95C2801B}']
    function GetMethod: THttpMethod;
    procedure SetMethod(Value: THttpMethod);
    function GetResource: IHttpRequestUri;
    function GetVersion: IHttpVersion;
    property Method: THttpMethod read GetMethod write SetMethod;
    property Resource: IHttpRequestUri read GetResource;
    property Version: IHttpVersion read GetVersion;
  end;

  IHttpResponse = interface (IHttpMessage)
    ['{B034455F-8820-4478-8DA7-14075497C7B3}']
    function GetVersion: IHttpVersion;
    function GetStatus: IHttpResponseStatus;
    function GetResponseTags: IHttpResponseTags;
    property Version: IHttpVersion read GetVersion;
    property Status: IHttpResponseStatus read GetStatus;
    property Tags: IHttpResponseTags read GetResponseTags;
  end;

  IHttpResponseStatus = interface
    ['{B455121D-A574-4FEE-8D7F-A034C514F0F4}']
    function GetValue: THttpStatus;
    function GetCode: Word;
    function GetReason: string;
    procedure SetValue(Value: THttpStatus);
    procedure SetCode(Value: Word);
    procedure SetReason(const Value: string);
    property Value: THttpStatus read GetValue write SetValue;
    property Code: Word read GetCode write SetCode;
    property Reason: string read GetReason write SetReason;
  end;

  IHttpEntity = interface
    ['{B35E40D6-DAD8-47AD-A5E9-A8803061FC3B}']
    function GetEntityTags: IHttpEntityTags;
    function GetStream: IMemoryStream;
    property Tags: IHttpEntityTags read GetEntityTags;
    property Stream: IMemoryStream read GetStream;
  end;

implementation
end.
