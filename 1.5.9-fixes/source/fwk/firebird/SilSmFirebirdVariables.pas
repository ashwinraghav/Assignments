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

unit SilSmFirebirdVariables;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TFbVariableField = (
      fbidVariable,
      fbidName
    );

type
  TSilFirebirdVariablesClass = class of TSilFirebirdVariables;
  TSilFirebirdVariables = class(
    TSilObject,
    IFbVariablesInternal )
  private
    FSession: Pointer;
    FStatement: Pointer;
    FItemClass: TSilFirebirdVariableClass;
    FList: IStringsDynamic;
  private
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean = False): Boolean; virtual;
  protected // IFbVariables
    function GetSession: IFbSession;
    function GetCount: Integer;
    function GetItem(Index: Integer): IFbVariable;
    function Get(const Name: string): IFbVariable;
    function Find(const Name: string; out Item: IFbVariable): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: IFbVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; overload;
  protected // IFbVariablesInternal
    function DoGetStatement: IFbStatementInternal;
    function DoGetSession: IFbSessionInternal;
    function Add(const Item: IFbVariableInternal): Integer;
    function Remove(const Item: IFbVariableInternal): Integer;
  protected
    procedure DoClear;
    procedure DoInitialize;
    procedure DoCreate(var Buffer: PXSQLDA_V1; Version: Integer); virtual;
    procedure DoFill(Info: PXSQLDA_V1; Version: Integer); virtual;
    function DoAlloc(Count: Integer; Version: Integer): PXSQLDA_V1; virtual;
    procedure DoFree(var Buffer: PXSQLDA_V1); virtual;
    procedure DoLoad(Source: PXSQLDA_V1); virtual;
    function DoAdd(Source: PXSQLVAR_V1): IFbVariableInternal; virtual;
  protected
    procedure DoDescribe(Buffer: PXSQLDA_V1); virtual; abstract;
  protected
    property Statement: IFbStatementInternal read DoGetStatement;
    property Session: IFbSessionInternal read DoGetSession;
  public
    constructor Create(const Statement: IFbStatementInternal; ItemClass: TSilFirebirdVariableClass); overload;  
    constructor Create(const Statement: IFbStatementInternal); overload; virtual; abstract;     
    destructor Destroy; override;
  end;

  TSilFirebirdFields = class(TSilFirebirdVariables)
  protected
    procedure DoDescribe(Buffer: PXSQLDA_V1); override;      
  public
    constructor Create(const Statement: IFbStatementInternal); override;      
  end;

  TSilFirebirdParameters = class(TSilFirebirdVariables)
  protected
    procedure DoDescribe(Buffer: PXSQLDA_V1); override;
  public
    constructor Create(const Statement: IFbStatementInternal); override;
    destructor Destroy; override;      
  end;

implementation

uses
  SilLiContainerStrings,
  SilLmContainerEnumerator,
  SilSfFirebird,
  SilSfFirebirdClient,
  SilSmFirebirdVariable;

const
  FindByVariable  = Pointer(Ord(fbidVariable));
  FindByName      = Pointer(Ord(fbidName));

{ TSilFirebirdVariables }

constructor TSilFirebirdVariables.Create(const Statement: IFbStatementInternal; ItemClass: TSilFirebirdVariableClass);
begin
  inherited Create;
  FItemClass := ItemClass;
  FStatement := Pointer(Statement);
  FSession := Pointer(Statement.Session);
  FList := Vector.Strings(Handler.Create(TypeInfo(IFbVariableInternal), Compare.Create(DoCompare)));
  DoInitialize();
end;

destructor TSilFirebirdVariables.Destroy;
begin
  DoClear();
  FList := nil;
  FSession := nil;
  FStatement := nil;
  inherited;
end;

function TSilFirebirdVariables.Enumerate(var Enum: IEnumerator; out Item: IFbVariable): Boolean;
var
  Ptr: PStringData;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum) then
    Result := Enum.HasMore
  else
    Result := False;

  if not Result then
    Enum := nil
  else if Enum.Get(Ptr) then
    Item := IFbVariableInternal(PUnknown(@Ptr.Data)^);
end;

function TSilFirebirdVariables.Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean;
var
  Variable: IFbVariable;
begin
  repeat
    Result := Enumerate(Enum, Variable);
  until not Result or (Variable.QueryInterface(IID, Item) = 0);
end;

function TSilFirebirdVariables.Find(const Name: string; out Item: IFbVariable): Boolean;
var
  Index: HItem;
begin
  Result := FList.Find(Name, Index);
  if Result then Item := IFbVariableInternal(FList.Ptrs[Index]^);
end;

function TSilFirebirdVariables.Get(const Name: string): IFbVariable;
begin
  if not Find(Name, Result) then
    Result := nil;
end;

function TSilFirebirdVariables.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList.Base, Sequence.Create(FList.Base));
end;

function TSilFirebirdVariables.GetSession: IFbSession;
begin
  Result := Session;
end;

function TSilFirebirdVariables.GetCount: Integer;
begin
  Result := FList.Items.Count;
end;

function TSilFirebirdVariables.GetItem(Index: Integer): IFbVariable;
begin
  Result := IFbVariableInternal(FList.Ptrs[Index]^);;  
end;

function TSilFirebirdVariables.Add(const Item: IFbVariableInternal): Integer;
begin
  Result := FList.Add(Item.Name, @Item);
end;

function TSilFirebirdVariables.Remove(const Item: IFbVariableInternal): Integer;
begin
  if FList.Find(@Item, HItem(Result), nil, FindByVariable) then
    FList.Items.Delete(Result);
end;

procedure TSilFirebirdVariables.DoClear;
var
  Enum: IEnumerator;
  Item: IFbVariableInternal;
begin
  while Enumerate(Enum, IFbVariableInternal, Item) do
  try
    Item.Detach;
  finally
    Item := nil;
  end;
end;

procedure TSilFirebirdVariables.DoInitialize;
var
  Buffer: PXSQLDA_V1;
begin
  DoCreate(Buffer, SQLDA_VERSION1);
  try
    if Assigned(Buffer) then
    begin
      DoDescribe(Buffer);
      DoLoad(Buffer);
    end;
  finally
    DoFree(Buffer);
  end;
end;

procedure TSilFirebirdVariables.DoCreate(var Buffer: PXSQLDA_V1; Version: Integer);
var
  Info: TXSQLDA_V1;
begin
  DoFill(@Info, Version);
  DoDescribe(@Info);
  Buffer := DoAlloc(Info.sqld, Version);
end;

procedure TSilFirebirdVariables.DoFill(Info: PXSQLDA_V1; Version: Integer);
begin
  Sil.Mem.Clear(Info^, SizeOf(Info^));
  Info.version := Version; 
  Info.sqln := 1;
  Info.sqld := 1;
end;

function TSilFirebirdVariables.DoAlloc(Count: Integer; Version: Integer): PXSQLDA_V1;
begin
  if Count > 0 then
  begin
    Result := Sil.Mem.Alloc(XSQLDA_LENGTH(Count, Version));
    Result.version := Version;
    Result.sqln := Count;
  end else
    Result := nil;
end;

procedure TSilFirebirdVariables.DoFree(var Buffer: PXSQLDA_V1);
begin
  if Assigned(Buffer) then Sil.Mem.Free(Buffer);
end;

procedure TSilFirebirdVariables.DoLoad(Source: PXSQLDA_V1);
var
  Index: Integer;
  Item: PXSQLVAR_V1;
begin
  Item := @Source.sqlvar;
  for Index := 0 to Source.sqld - 1 do
  begin
    DoAdd(Item);
    Inc(Item);
  end;
end;

function TSilFirebirdVariables.DoAdd(Source: PXSQLVAR_V1): IFbVariableInternal;
begin
  FItemClass.Create(Self, Statement, Source).GetInterface(IFbVariableInternal, Result);
end;

function TSilFirebirdVariables.DoGetStatement: IFbStatementInternal;
begin
  Result := IFbStatementInternal(FStatement);
end;

function TSilFirebirdVariables.DoGetSession: IFbSessionInternal;
begin
  Result := IFbSessionInternal(FSession);
end;

function TSilFirebirdVariables.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TFbVariableField(Param) of
    fbidVariable:     Result := Sil.Ref.Compare(IFbVariable(Data1^), IFbVariable(Data2^));
    fbidName:         Result := Sil.Str.TextCompare(string(Data1^), IFbVariable(Data2^).Name);
    else              Result := -1;
  end;
end;

{ TSilFirebirdFields }

constructor TSilFirebirdFields.Create(const Statement: IFbStatementInternal);
begin
  inherited Create(Statement, TSilFirebirdField);
end;

procedure TSilFirebirdFields.DoDescribe(Buffer: PXSQLDA_V1);
begin
  Check(fb.api.sql.describe(fb.status, Statement.Handle, SQL_DIALECT_CURRENT, Pointer(Buffer)));
end;

{ TSilFirebirdParameters }

constructor TSilFirebirdParameters.Create(const Statement: IFbStatementInternal);
begin
  inherited Create(Statement, TSilFirebirdParam);
end;

destructor TSilFirebirdParameters.Destroy;
begin
  inherited;
end;

procedure TSilFirebirdParameters.DoDescribe(Buffer: PXSQLDA_V1);
begin
  Check(fb.api.sql.describe_bind(fb.status, Statement.Handle, SQL_DIALECT_CURRENT, Buffer));
end;

end.
