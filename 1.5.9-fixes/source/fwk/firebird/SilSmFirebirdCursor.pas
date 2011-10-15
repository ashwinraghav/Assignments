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

unit SilSmFirebirdCursor;

{$INCLUDE Defines.inc}

interface

uses
  Sil, 
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

const
  fbcsActive = [fbcsOpened, fbcsFetching];

type
  TSilFirebirdCursor = class(
    TSilObject,
    IEnumerable,
    IFbCursorInternal )
  private
    FCommand: IFbCommandInternal;
    FBindings: IFbBindingsInternal;
    FBuffer: IFbBufferInternal;
    FPosition: Integer;
    FState: TSilFirebirdCursorState;
  private
    procedure DoFetch(const Values: IFbValuesInternal);
    procedure DoCheckOpen;
    procedure DoOpen(const Name: string = '');
    procedure DoClose;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean = False): Boolean; virtual;
  protected // IFbCursor
    function GetCommand: IFbCommand;
    function GetBindings: IFbBindings;
    function GetFields: IFbBuffer;
    function GetHasMore: Boolean;
    function GetPosition: Integer;
    function GetState: TSilFirebirdCursorState;
    function Fetch: Boolean; overload; 
    function Fetch(const Buffer: IFbBuffer): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbBuffer): Boolean; overload;
    procedure Close;
  protected // IFbCursorInternal
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetBuffer: IFbBufferInternal;
  protected
    property Command: IFbCommandInternal read FCommand;
    property Buffer: IFbBufferInternal read DoGetBuffer;
  public
    constructor Create(const Command: IFbCommandInternal; const Fields: IFbBindingsInternal = nil); overload;
    constructor Create(const Command: IFbCommandInternal; const Buffer: IFbBufferInternal); overload;
    destructor Destroy; override;
  end;

  TSilFirebirdCursorEnumerator = class(
    TSilObject,
    IEnumerator )
  private
    FCursor: IFbCursorInternal;
    FIteration: Integer;
    FIID: TGUID;
  protected // IEnumerator
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function GetIteration: Integer;
    function Next: Boolean;
    procedure Detach;
    procedure Reset;
  public
    constructor Create(const Cursor: IFbCursorInternal; const IID: TGUID);
    destructor Destroy; override;
  end;

implementation

uses
  SilSfFirebirdClient,
  SilScFirebirdClient,
  SilSfFirebird,
  SilSmFirebirdValues,
  SilStFirebird;

const
  CDefaultName: PChar = 'SQL$DYNAMIC$';

var
  MCursorNumber: Integer = 0;

{ TSilFirebirdCursor }

constructor TSilFirebirdCursor.Create(const Command: IFbCommandInternal; const Fields: IFbBindingsInternal);
begin
  inherited Create;
  FState := fbcsClosed;
  FCommand := Command;
  FBindings := Fields;
  if not Assigned(FBindings) then
    FBindings := IFbBindingsInternal(Firebird.Bindings(Command.Statement.Fields));
  FPosition := -1;
  FState := fbcsUndefined;
end;

constructor TSilFirebirdCursor.Create(const Command: IFbCommandInternal; const Buffer: IFbBufferInternal);
begin
  Create(Command, Buffer.Bindings);
  FBuffer := Buffer;
end;

destructor TSilFirebirdCursor.Destroy;
begin
  DoClose;
  FBuffer := nil;
  FBindings := nil;
  FCommand := nil;
  inherited;
end;

function TSilFirebirdCursor.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Enum := TSilFirebirdCursorEnumerator.Create(Self, IFbCursor);
  Result := True;
end;

function TSilFirebirdCursor.GetCommand: IFbCommand;
begin
  Result := FCommand;
end;

function TSilFirebirdCursor.GetBindings: IFbBindings;
begin
  Result := FBindings;
end;

function TSilFirebirdCursor.GetFields: IFbBuffer;
begin
  Result := Buffer;
end;

function TSilFirebirdCursor.GetHasMore: Boolean;
begin
  DoCheckOpen;
  Result := FState in fbcsActive;
end;

function TSilFirebirdCursor.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TSilFirebirdCursor.GetState: TSilFirebirdCursorState;
begin
  Result := FState;
end;

function TSilFirebirdCursor.Fetch: Boolean;
begin
  Result := Fetch(Self.Buffer);
end;

function TSilFirebirdCursor.Fetch(const Buffer: IFbBuffer): Boolean;
begin
  DoCheckOpen;
  DoFetch(IFbBufferInternal(Buffer).Values);
  Result := FState = fbcsFetching;
  if Result then Inc(FPosition);
end;

function TSilFirebirdCursor.Enumerate(var Enum: IEnumerator; out Fields: IFbBuffer): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
    Fields := Self.Buffer else
    Enum := nil;
end;

procedure TSilFirebirdCursor.Close;
begin
  DoClose;
end;

procedure TSilFirebirdCursor.DoCheckOpen;
begin
  if FState = fbcsUndefined then
    DoOpen();
end;

procedure TSilFirebirdCursor.DoFetch(const Values: IFbValuesInternal);
var
  Result: ISC_STATUS;
begin
  Result := fb.api.sql.fetch(fb.status, Command.Handle, SQL_DIALECT_CURRENT, Values.Handle);
  case Result of
      0:  FState := fbcsFetching;
    100:  DoClose;
    else  Check(Result);
  end;
end;

procedure TSilFirebirdCursor.DoOpen(const Name: string);
var
  NameStr: string;                 
begin
  if FState in fbcsActive then DoClose;
  if Sil.Str.IsAssigned(Name) then
    NameStr := Name else
    NameStr := CDefaultName + Sil.Int.ToStr(Sil.Int.Inc(MCursorNumber));
  Check(fb.api.sql.set_cursor_name(fb.status, Command.Handle, PChar(NameStr), 0));
  FState := fbcsOpened;
end;

procedure TSilFirebirdCursor.DoClose;
begin
  if FState in fbcsActive then
  begin
    fb.api.sql.free_statement(fb.status, Command.Handle, DSQL_close);
    FState := fbcsClosed;
  end;
end;

function TSilFirebirdCursor.DoGetCommand: IFbCommandInternal;
begin
  Result := FCommand;
end;

function TSilFirebirdCursor.DoGetBindings: IFbBindingsInternal;
begin
  Result := FBindings;
end;

function TSilFirebirdCursor.DoGetBuffer: IFbBufferInternal;
begin
  if FBuffer = nil then FBuffer := IFbBufferInternal(Firebird.Buffer(Command, FBindings));
  Result := FBuffer;
end;

{ TSilFirebirdCursorEnumerator }

constructor TSilFirebirdCursorEnumerator.Create(const Cursor: IFbCursorInternal; const IID: TGUID);
begin
  inherited Create;
  FCursor := Cursor;
  FIID := IID;
end;

destructor TSilFirebirdCursorEnumerator.Destroy;
begin
  Detach;
  inherited;
end;

procedure TSilFirebirdCursorEnumerator.Detach;
begin
  FCursor := nil;
end;

function TSilFirebirdCursorEnumerator.Get(out Item): Boolean;
begin
  Result := FCursor.QueryInterface(FIID, Item) = 0;  
end;

function TSilFirebirdCursorEnumerator.GetCurrent: Pointer;
begin
  Result := Pointer(FCursor);  
end;

function TSilFirebirdCursorEnumerator.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TSilFirebirdCursorEnumerator.HasMore: Boolean;
begin
  Result := FCursor.HasMore;
  if Result and (FCursor.State = fbcsOpened) then
    Result := FCursor.Fetch();
end;

function TSilFirebirdCursorEnumerator.Next: Boolean;
begin
  Result := FCursor.Fetch;
end;

procedure TSilFirebirdCursorEnumerator.Reset;
begin
  FIteration := 0;
end;

end.
