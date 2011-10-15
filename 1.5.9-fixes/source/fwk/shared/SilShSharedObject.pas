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

unit SilShSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  Sil;

const
  GsSharedFactoryList: TGUID    = '{5F40260A-13D5-44CB-8125-B2AE00D1672F}';
  GsSharedProviderList: TGUID   = '{01DCCBB9-DF41-4DCB-9AE2-1D18EF82D033}';
  GsSharedManager: TGUID         = '{008F92ED-41FA-4048-BCFF-79786F488B49}';

const
  CSilClassQuery      = 'SilClassQuery';
  CSilGetTool         = 'SilGetTool';
  CSilCreateObject    = 'SilCreateObject';
  CSilGetClassObject  = 'SilGetClassObject';
  CSilRegister        = 'SilRegister';
  CSilUnregister      = 'SilUnregister';
  CSilLocalServices   = 'SilLocalServices';
  CDllGetClassObject  = 'DllGetClassObject';
  CDllRegister        = 'DllRegisterServer';
  CDllUnregister      = 'DllUnregisterServer';

type
  TSilSignature = function: PChar; stdcall;
  TSilCreateObject = function(const ClassID: TGuid; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; stdcall;
  TSilGetTool = function: TClass; stdcall;
  TSilClassQuery = function(const ClassID: TGuid): Boolean; stdcall;
  TSilGetClassObject = function(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall;
  TSilRegister = function: Integer; stdcall;
  TSilUnregister = function: Integer; stdcall;
  TSilLocalServices = function: IUnknown; stdcall;

type
  ESilSharedObjectError = class(Exception);

type
  ISharedObjectFunctions = interface;
  ISharedObjectReferences = interface;

  TSharedObjectEntryPoint = (
      epSignature,
      epCreateObject,
      epGetTool,
      epGetClassQuery,
      epGetClassObject,
      epRegister,
      epUnregister,
      epLocalServices
    );

  TSharedObjectEntryPoints = set of TSharedObjectEntryPoint;
  
  ISharedObjectFunctions = interface
    ['{F1746E50-5E59-4F64-A42A-B6597C25EEA5}']
    function GetDll: ISharedLibrary;
    function GetModule: IModule2;
    function GetLibrary: IVersionNumber;
    function GetRuntime: IVersionNumber;
    function GetSupport: TSharedObjectEntryPoints;
    function DoSignature: TSilSignature;
    function DoCreateObject: TSilCreateObject;
    function DoGetTool: TSilGetTool;
    function DoGetClassQuery: TSilClassQuery;
    function DoGetClassObject: TSilGetClassObject;
    function DoRegister: TSilRegister;
    function DoUnregister: TSilUnregister;
    function DoLocalServices: TSilLocalServices;
    property Dll: ISharedLibrary read GetDll;
    property Module: IModule2 read GetModule;
    property Lib: IVersionNumber read GetLibrary;
    property Rtl: IVersionNumber read GetRuntime;
    property Support: TSharedObjectEntryPoints read GetSupport;
    property Signature: TSilSignature read DoSignature;
    property CreateObject: TSilCreateObject read DoCreateObject;
    property GetTool: TSilGetTool read DoGetTool;
    property ClassQuery: TSilClassQuery read DoGetClassQuery;
    property GetClassObject: TSilGetClassObject read DoGetClassObject;
    property Register: TSilRegister read DoRegister;
    property Unregister: TSilUnregister read DoUnregister;
    property LocalServices: TSilLocalServices read DoLocalServices;
  end;

  ISharedObjectReferences = interface
    ['{D6D870C9-CD9A-4485-BA8F-912D8E482ECF}']
    function GetCount: Integer;
    procedure AddRef(Ref: PUnknown);
    function DropRef(Ref: PUnknown): Boolean;
    function Contains(Ref: PUnknown): Boolean;
    property Count: Integer read GetCount;
  end;

implementation

end.
 