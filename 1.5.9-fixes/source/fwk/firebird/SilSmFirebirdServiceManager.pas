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

unit SilSmFirebirdServiceManager;

{$INCLUDE Defines.inc}

interface

uses
  Sil, 
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdServiceManager = class(
    TSilFirebirdHandled,
    IFbServiceManager,
    IFbServicesInternal )
  private
    FApplication: Pointer;
    FProperties: IFbParametersInternal;
  private 
    procedure DoAttach(const Address: string; const Values: IParameterList);
    function DoBuildParams(const Values: IParameterList): String;
  protected
    procedure DoCloseHandle(const Sender: IFbHandle); override;
  protected // IFbServiceManager
    procedure Query(const Parameters: IParameterList; Timeout: LongWord); overload; 
    function Query(const Items: array of string; const Parameters: IParameterList; Timeout: LongWord = INFINITE): IParameterList; overload;
    function Query(const Items: array of string; Timeout: LongWord = INFINITE): IParameterList; overload; 
    procedure Backup(const DatabaseName: string; const Parameters: IParameterList = nil);
    procedure Restore(const DatabaseName: string; const Parameters: IParameterList = nil);
  protected // IFbServicesInternal
    function DoGetApplication: IFbApplicationInternal;
    function DoGetHandle: PISC_SVC_HANDLE; 
  protected 
    property Application: IFbApplicationInternal read DoGetApplication;
    property Handle: PISC_SVC_HANDLE read DoGetHandle;
  public
    constructor Create(const Application: IFbApplicationInternal; const Address: string; const Parameters: IParameterList = nil);
    destructor Destroy; override;
  end;

implementation

uses
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSfFirebird,
  SilSgFirebirdParameters, SilLtList, SilBtVart;

{ TSilFirebirdServiceManager }

constructor TSilFirebirdServiceManager.Create(const Application: IFbApplicationInternal; const Address: string; const Parameters: IParameterList);
begin
  inherited Create;
  FApplication := Pointer(Application as IFbApplicationInternal);
  FProperties := Application.GetParameters(GFbServiceParameterBlock, fb.block(TSilFirebirdParameterBlock));
  DoAttach(Address, Parameters);
end;

destructor TSilFirebirdServiceManager.Destroy;
begin
  Close;
  FProperties := nil;
  FApplication := nil;
  inherited;
end;

procedure TSilFirebirdServiceManager.Query(const Parameters: IParameterList; Timeout: LongWord);
var
  Block: IFbParametersInternal;
  Enum: IEnumerator;
  Item: RParameter;
  Info: PFbParameterInfo;
  SPB, Request, Reply: string;
begin
  Block := Application.GetParameters(GFbInfoBlock, fb.block(TSilFirebirdServiceRequestBuffer));

  if (Timeout <> INFINITE) and (Timeout <> 0) then
  begin
    Sil.Str.Append(SPB, Char(isc_spb_version));
    Sil.Str.Append(SPB, Char(isc_spb_current_version));
    Timeout := (Timeout + 999) div 1000;
    Block.Add(SPB, CInfoTimeout, Timeout);
  end;

  with Parameters do
    while Enumerate(Enum, Item) do
      if Block.Find(Item.Name, Info) then
        Sil.Str.Append(Request, System.Char(Info.Code));

  SetLength(Reply, 65535);
  Sil.Mem.Clear(Reply[1], Length(Reply));

  Check(fb.api.service.query(fb.status, Handle, nil, Length(SPB), PChar(SPB), Length(Request), PChar(Request), Length(Reply), PChar(Reply)));

  Block.Parse(@Reply[1], Parameters);
end;

function TSilFirebirdServiceManager.Query(const Items: array of string; const Parameters: IParameterList; Timeout: LongWord): IParameterList;
var
  Index: Integer;
begin
  if not Assigned(Parameters) then
    Result := Sil.List.Parameters() else
    Result := Parameters;
  for Index := Low(Items) to High(Items) do
    Result[Items[Index]] := Sil.Vart.Unassigned;
  Query(Result, Timeout);
end;

function TSilFirebirdServiceManager.Query(const Items: array of string; Timeout: LongWord): IParameterList;
begin
  Result := Query(Items, nil, Timeout);
end;

procedure TSilFirebirdServiceManager.Backup(const DatabaseName: string; const Parameters: IParameterList);
var
  Block: IFbParametersInternal;
  Buffer: string;
begin
  Block := Application.GetParameters(GFbRequestBufferBlock, fb.block(TSilFirebirdServiceRequestBuffer));

  Sil.Str.Append(Buffer, Char(isc_action_svc_backup));
  Block.Add(Buffer, CSpbDatabaseName, DatabaseName);
  Block.Add(Buffer, Parameters);
  Check(fb.api.service.start(fb.status, Handle, nil, Length(Buffer), PChar(Buffer)));
end;

procedure TSilFirebirdServiceManager.Restore(const DatabaseName: string; const Parameters: IParameterList);
var
  Block: IFbParametersInternal;
  Buffer: string;
begin
  Block := Application.GetParameters(GFbRequestBufferBlock, fb.block(TSilFirebirdServiceRequestBuffer));

  Sil.Str.Append(Buffer, Char(isc_action_svc_restore));
  FProperties.Add(Buffer, CSpbDatabaseName, DatabaseName);
  FProperties.Add(Buffer, Parameters);
  Check(fb.api.service.start(fb.status, Handle, nil, Length(Buffer), PChar(Buffer)));
end;

procedure TSilFirebirdServiceManager.DoAttach(const Address: string; const Values: IParameterList);
var
  DPB: String;
begin
  DPB := DoBuildParams(Values);
  Check(fb.api.service.attach(fb.status, 0, PChar(Address), Self.Handle, Length(DPB), PChar(DPB)));
end;

procedure TSilFirebirdServiceManager.DoCloseHandle(const Sender: IFbHandle);
begin
  if Sender.IsAssigned then fb.api.service.detach(fb.status, Sender.Value);
end;

function TSilFirebirdServiceManager.DoBuildParams(const Values: IParameterList): String;
begin
  if Assigned(Values) then
  begin
    Sil.Str.Append(Result, Char(isc_spb_version));
    Sil.Str.Append(Result, Char(isc_spb_current_version));
    FProperties.Add(Result, Values);
  end;
end;

function TSilFirebirdServiceManager.DoGetApplication: IFbApplicationInternal;
begin
  Result := IFbApplicationInternal(FApplication);
end;

function TSilFirebirdServiceManager.DoGetHandle: PISC_SVC_HANDLE;
begin
  Result := inherited Handle.Value;
end;

end.
