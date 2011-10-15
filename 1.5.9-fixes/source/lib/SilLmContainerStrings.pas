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

unit SilLmContainerStrings;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilBeTypes,
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLiContainer,
  SilLiContainerStrings,
  SilLkContainer;

type
  TSilStringContainer = class(
    TSilContainer,
    IStringsStatic,
    IStringsDynamic,
    IStringCursorsStatic,
    IStringCursorsDynamic )
  private
    FData: ITypeHandler;
  protected // IStringCursorsStatic
    function IStringCursorsStatic.Create = CreateCursor;
    function GetFirstCursorStatic: IStringPointerStatic;
    function GetLastCursorStatic: IStringPointerStatic;
    function IStringCursorsStatic.Cursor = CursorStatic;
    function CursorStatic(const Text: string; Param: Pointer = nil; Delta: Integer = 1): IStringPointerStatic; overload;
  protected // IStringCursorsDynamic
    function IStringCursorsDynamic.Create = CreateCursor;
    function GetFirstCursorDynamic: IStringPointerDynamic;
    function GetLastCursorDynamic: IStringPointerDynamic;
    function IStringCursorsDynamic.Cursor = CursorDynamic;
    function CursorDynamic(const Text: string; Param: Pointer = nil; Delta: Integer = 1): IStringPointerDynamic; overload;
  protected // IStringsStatic
    function GetBaseStatic: IContainerStatic;
    function GetCursorsStatic: IStringCursorsStatic;
    function Cursor(Data: HData; Param: Pointer = nil; Delta: Integer = 1): IStringPointerStatic; overload;
    function HandleGet(Item: HItem): PStringData; reintroduce;
    function CursorGet(const Cursor: IContainerCursor): PStringData; reintroduce;
    function GetCursorItem(const Cursor: IContainerCursor): string;
    function GetCursorData(const Cursor: IContainerCursor): HData;
    function GetHandleItem(Item: HItem): string;
    function GetHandleData(Item: HItem): HData;
    function Handle(const Text: string; Param: Pointer = nil): HItem; overload;
    function Handle(Data: HData; Param: Pointer = nil): HItem; overload;
    function Find(const Text: string; out Item: HItem; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Item: HItem; const Compare: TTypeCompare; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; const Compare: TTypeCompare; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; const Comparator: ITypeComparator; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Item: HItem; Param: Pointer = nil): Boolean; overload;
    function Find(Data: HData; out Item: HItem; const Compare: TTypeCompare; Param: Pointer = nil): Boolean; overload;
    function Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer = nil): Boolean; overload; 
    function Find(Data: HData; out Value: IStringPointerStatic; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Value: IContainerCursorStatic; const Compare: TTypeCompare; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Value: IContainerCursorStatic; const Comparator: ITypeComparator; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
  protected // IStringsDynamic
    function GetBaseDynamic: IContainerDynamic;
    function GetCursorsDynamic: IStringCursorsDynamic;
    function Add(const Text: string; Source: HData = nil): HItem; overload;
    function Insert(const Text: string; Source: HData = nil; Item: HItem = HNull): HItem; overload;
    procedure Put(Item: HItem; const Text: string; Source: HData);
    procedure HandlePut(Item: HItem; Value: PStringData); reintroduce; 
    procedure PutCursorItem(const Cursor: IContainerCursor; const Text: string);
    procedure PutCursorData(const Cursor: IContainerCursor; Source: HData);
    procedure PutHandleItem(Item: HItem; const Text: string);
    procedure PutHandleData(Item: HItem; Source: HData);
    function Remove(const Text: string; Param: Pointer = nil): HItem; overload;
  public
    constructor Create(
            AClass: TSilBaseContainerClass;
      const Handler: ITypeHandler;
      const Options: IParameters = nil;
      const Owner: IUnknown = nil;
      const Controller: IUnknown = nil;
            Param: Pointer = nil); override;
    destructor Destroy; override;
  end;

  TSilStringCursor = class(
    TSilCursor,
    IStringPointerStatic,
    IStringPointerDynamic )
  private
    function DoGetOwner: TSilStringContainer;
  protected // IStringPointerStatic
    function GetOwnerStatic: IStringsStatic;
    function GetThisStaticString: IStringPointerStatic;
    function GetText: string;
    function GetData: HData;
    function Get(out Data: PStringData): Boolean;
  protected // IStringPointerDynamic
    function GetOwnerDynamic: IStringsDynamic;
    function GetThisDynamicString: IStringPointerDynamic;
    procedure SetText(const Text: string);
    procedure SetData(Data: HData);
    function Put(const Data: PStringData): Boolean; overload;
    function Put(const Text: string; Source: HData): Boolean; overload;
    function Insert(const Text: string; Source: HData): Boolean; overload;
    function Insert(const Data: PStringData): Boolean; overload;
  protected
    property Owner: TSilStringContainer read DoGetOwner;
  end;

implementation

uses
  SilLmContainerTypes,
  SilLtContainer;

type
  TSilStringsHandler = class(TSilTypeCompositeHandler)
  private
    FStringComparator: ITypeComparator;
  protected
    function Compare(Data1, Data2: HData; Param: Pointer): Integer; override; 
  public
    constructor Create(const DataHandler: ITypeHandler); reintroduce;
    destructor Destroy; override;
  end;

  TSilStringsTextCompare = class(
    TSilObject,
    IAdoptionTrait,
    ITypeComparator )
  protected // ITypeComparator
    function Compare(Data1, Data2: HData; Param: Pointer): Integer;
  public
    class function Create: ITypeComparator;
    destructor Destroy; override;
  end;

  TSilStringsDataCompare = class(
    TSilObject,
    IAdoptionTrait,
    ITypeComparator )
  private
    FData: ITypeComparator;
  protected // ITypeComparator
    function Compare(Data1, Data2: HData; Param: Pointer): Integer;
  public
    constructor CreateNew(const Data: ITypeComparator); reintroduce;
    destructor Destroy; override;
  public
    class function Create(const Data: ITypeComparator): ITypeComparator;
  end;

{ TSilStringContainer }

constructor TSilStringContainer.Create(AClass: TSilBaseContainerClass; const Handler: ITypeHandler; const Options: IParameters; const Owner: IUnknown; const Controller: IUnknown; Param: Pointer);
var
  DataHandler: ITypeHandler;
begin
  DataHandler := SilLtContainer.Handler.Check(Handler);
  inherited Create(AClass, TSilStringsHandler.Create(DataHandler), Options, Owner, Controller, Param);
  CursorClass := TSilStringCursor;
  FData := DataHandler;
end;

destructor TSilStringContainer.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilStringContainer.GetBaseStatic: IContainerStatic;
begin
  Result := Self;
end;

function TSilStringContainer.GetCursorsStatic: IStringCursorsStatic;
begin
  Result := Self;
end;

function TSilStringContainer.GetFirstCursorStatic: IStringPointerStatic;
begin
  CreateCursor(GetFirstItem(), IStringPointerStatic, Result, +1);
end;

function TSilStringContainer.GetLastCursorStatic: IStringPointerStatic;
begin
  CreateCursor(GetLastItem(), IStringPointerStatic, Result, -1); 
end;

function TSilStringContainer.HandleGet(Item: HItem): PStringData;
begin
  Result := inherited HandleGet(Item);
end;

function TSilStringContainer.CursorGet(const Cursor: IContainerCursor): PStringData;
begin
  Result := inherited CursorGet(Cursor);
end;

function TSilStringContainer.GetCursorItem(const Cursor: IContainerCursor): string;
begin
  Result := CursorGet(Cursor).Text;
end;

function TSilStringContainer.GetCursorData(const Cursor: IContainerCursor): HData;
begin
  Result := @CursorGet(Cursor).Data;
end;

function TSilStringContainer.GetHandleItem(Item: HItem): string;
begin
  Result := HandleGet(Item).Text;
end;

function TSilStringContainer.GetHandleData(Item: HItem): HData;
begin
  Result := @HandleGet(Item).Data;
end;

function TSilStringContainer.CursorStatic(const Text: string; Param: Pointer; Delta: Integer): IStringPointerStatic;
begin
  CreateCursor(Handle(Text, Param), IStringPointerStatic, Result, Delta);
end;

function TSilStringContainer.Cursor(Data: HData; Param: Pointer; Delta: Integer): IStringPointerStatic;
begin
  CreateCursor(Handle(Data, Param), IStringPointerStatic, Result, Delta);
end;

function TSilStringContainer.Handle(const Text: string; Param: Pointer): HItem;
begin
  if not Find(Text, Result, Param) then
    Result := HNull;
end;

function TSilStringContainer.Handle(Data: HData; Param: Pointer): HItem;
begin
  if not Find(Data, Result, Param) then
    Result := HNull;
end;

function TSilStringContainer.Find(const Text: string; out Item: HItem; Param: Pointer): Boolean;
begin
  Result := Find(Text, Item, Handler, Param);
end;

function TSilStringContainer.Find(const Text: string; out Item: HItem; const Compare: TTypeCompare; Param: Pointer): Boolean;
var
  Comparator: ITypeComparator;
begin
  Comparator := TSilTypeMethodCompare.Create(Compare);
  Result := Find(Text, Item, Comparator, Param);
end;

function TSilStringContainer.Find(const Text: string; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer): Boolean;
var
  Rec: PStringData;
begin
  Rec := Base.Scratch;
  Rec.Text := Text;
  FData.Initialize(@Rec.Data);
  Result := inherited Find(Rec, Item, Comparator, Param);
end;

function TSilStringContainer.Find(const Text: string; out Value: IStringPointerStatic; Param: Pointer; Delta: Integer): Boolean;
var
  Item: HItem;
begin
  Result := Find(Text, Item, Param) and CreateCursor(Item, IStringPointerStatic, Value, Delta);
end;

function TSilStringContainer.Find(const Text: string; out Value: IStringPointerStatic; const Compare: TTypeCompare; Param: Pointer; Delta: Integer): Boolean;
var
  Comparator: ITypeComparator;
begin
  Comparator := TSilTypeMethodCompare.Create(Compare);
  Result := Find(Text, Value, Comparator, Param);
end;

function TSilStringContainer.Find(const Text: string; out Value: IStringPointerStatic; const Comparator: ITypeComparator; Param: Pointer; Delta: Integer): Boolean;
var
  Item: HItem;
begin
  Result := Find(Text, Item, Comparator, Param) and CreateCursor(Item, IStringPointerStatic, Value, Delta);
end;

function TSilStringContainer.Find(Data: HData; out Item: HItem; Param: Pointer): Boolean;
var
  Comparator: ITypeComparator;
begin
  Comparator := TSilStringsDataCompare.Create(FData);
  Result := Find(Data, Item, Comparator, Param);
end;

function TSilStringContainer.Find(Data: HData; out Item: HItem; const Compare: TTypeCompare; Param: Pointer): Boolean;
var
  Comparator: ITypeComparator;
begin
  Comparator := TSilTypeMethodCompare.Create(Compare);
  Result := Find(Data, Item, Comparator, Param);
end;

function TSilStringContainer.Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer): Boolean;
var
  Rec: PStringData;
begin
  Rec := FBase.Scratch;
  Rec.Text := '';
  FData.Copy(@Rec.Data, Data);
  Result := inherited Find(Rec, Item, Comparator, Param);
end;

function TSilStringContainer.Find(Data: HData; out Value: IStringPointerStatic; Param: Pointer; Delta: Integer): Boolean;
var
  Item: HItem;
begin
  Result := Find(Data, Item, Param) and CreateCursor(Item, IStringPointerStatic, Value, Delta);
end;

function TSilStringContainer.Find(Data: HData; out Value: IContainerCursorStatic; const Compare: TTypeCompare; Param: Pointer; Delta: Integer): Boolean;
var
  Item: HItem;
begin
  Result := Find(Data, Item, Compare, Param) and CreateCursor(Item, IStringPointerStatic, Value, Delta);
end;

function TSilStringContainer.Find(Data: HData; out Value: IContainerCursorStatic; const Comparator: ITypeComparator; Param: Pointer; Delta: Integer): Boolean;
var
  Item: HItem;
begin
  Result := Find(Data, Item, Comparator, Param) and CreateCursor(Item, IStringPointerStatic, Value, Delta);
end;

function TSilStringContainer.GetBaseDynamic: IContainerDynamic;
begin
  Result := Self;
end;

function TSilStringContainer.GetCursorsDynamic: IStringCursorsDynamic;
begin
  Result := Self;
end;

function TSilStringContainer.GetFirstCursorDynamic: IStringPointerDynamic;
begin
  CreateCursor(GetFirstItem(), IStringPointerDynamic, Result, +1);
end;

function TSilStringContainer.GetLastCursorDynamic: IStringPointerDynamic;
begin
  CreateCursor(GetLastItem(), IStringPointerDynamic, Result, -1); 
end;

function TSilStringContainer.Add(const Text: string; Source: HData): HItem;
var
  Rec: PStringData;
begin
  Rec := Base.Scratch;
  Rec.Text := Text;
  if Assigned(Source) then FData.Copy(@Rec.Data, Source);
  Result := inherited Add(1, Rec);
end;

function TSilStringContainer.Insert(const Text: string; Source: HData; Item: HItem): HItem;
var
  Rec: PStringData;
begin
  Result := Insert(HData(Rec), Item, 1);
  Rec.Text := Text;
  if Assigned(Source) then FData.Copy(@Rec.Data, Source);
end;

procedure TSilStringContainer.Put(Item: HItem; const Text: string; Source: HData);
var
  Rec: PStringData;
begin
  Rec := HandleGet(Item);
  Rec.Text := Text;
  if Assigned(Source) then FData.Copy(@Rec.Data, Source);
end;

procedure TSilStringContainer.HandlePut(Item: HItem; Value: PStringData);
begin
  inherited HandlePut(Item, Value);
end;

procedure TSilStringContainer.PutCursorItem(const Cursor: IContainerCursor; const Text: string);
begin
  PStringData(CursorGet(Cursor)).Text := Text;
end;

procedure TSilStringContainer.PutCursorData(const Cursor: IContainerCursor; Source: HData);
begin
  FData.Copy(@PStringData(CursorGet(Cursor)).Data, Source);
end;

procedure TSilStringContainer.PutHandleItem(Item: HItem; const Text: string);
begin
  PStringData(HandleGet(Item)).Text := Text;
end;

procedure TSilStringContainer.PutHandleData(Item: HItem; Source: HData);
begin
  FData.Copy(@PStringData(HandleGet(Item)).Data, Source);
end;

function TSilStringContainer.Remove(const Text: string; Param: Pointer): HItem;
begin
  if Find(Text, Result, Param) then
    Delete(Result);
end;

function TSilStringContainer.CursorDynamic(const Text: string; Param: Pointer; Delta: Integer): IStringPointerDynamic;
begin
  CreateCursor(Handle(Text, Param), IStringPointerDynamic, Result, Delta);
end;

{ TSilStringsHandler }

constructor TSilStringsHandler.Create(const DataHandler: ITypeHandler);
var
  StringHandler: ITypeHandler;
begin
  FStringComparator := TSilStringsTextCompare.Create();
  StringHandler := Handler.Create(System.TypeInfo(RStringData), FStringComparator);
  inherited Create([StringHandler, DataHandler]);
end;

destructor TSilStringsHandler.Destroy;
begin
  FStringComparator := nil;
  inherited;
end;

function TSilStringsHandler.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  if Assigned(FStringComparator) then
    Result := FStringComparator.Compare(Data1, Data2, Param) else
    Result := inherited Compare(Data1, Data2, Param);
end;

{ TSilStringsTextCompare }

class function TSilStringsTextCompare.Create: ITypeComparator;
begin
  Result := inherited Create;
end;

destructor TSilStringsTextCompare.Destroy;
begin
  inherited;
end;

function TSilStringsTextCompare.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Text.Compare(PStringData(Data1).Text, PStringData(Data2).Text);
end;

{ TSilStringsDataCompare }

class function TSilStringsDataCompare.Create(const Data: ITypeComparator): ITypeComparator;
begin
  Result := CreateNew(Data);
end;

constructor TSilStringsDataCompare.CreateNew(const Data: ITypeComparator);
begin
  inherited Create;
  FData := Data;
end;

destructor TSilStringsDataCompare.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilStringsDataCompare.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := FData.Compare(@PStringData(Data1).Data, @PStringData(Data2).Data, Param);
end;

{ TSilStringCursor }

function TSilStringCursor.GetOwnerStatic: IStringsStatic;
begin
  Result := Owner;
end;

function TSilStringCursor.GetThisStaticString: IStringPointerStatic;
begin
  Result := Self;
end;

function TSilStringCursor.GetText: string;
begin
  Result := Owner.GetHandleItem(FItem);
end;

function TSilStringCursor.GetData: HData;
begin
  Result := Owner.GetHandleData(FItem);
end;

function TSilStringCursor.Get(out Data: PStringData): Boolean;
begin
  Result := inherited Get(HData(Data));
end;

function TSilStringCursor.GetOwnerDynamic: IStringsDynamic;
begin
  Result := Owner;
end;

function TSilStringCursor.GetThisDynamicString: IStringPointerDynamic;
begin
  Result := Self;
end;

procedure TSilStringCursor.SetText(const Text: string);
begin
  Owner.PutHandleItem(FItem, Text);
end;

procedure TSilStringCursor.SetData(Data: HData);
begin
  Owner.PutHandleData(FItem, Data);
end;

function TSilStringCursor.Put(const Data: PStringData): Boolean;
begin
  Result := inherited Put(HData(Data));
end;

function TSilStringCursor.Put(const Text: string; Source: HData): Boolean;
begin
  Result := IsValid;
  if Result then Owner.Put(FItem, Text, Source);
end;

function TSilStringCursor.Insert(const Text: string; Source: HData): Boolean;
begin
  Result := IsValid;
  if Result then Owner.Insert(Text, Source, FItem);
end;

function TSilStringCursor.Insert(const Data: PStringData): Boolean;
begin
  Result := inherited Insert(HData(Data));
end;

function TSilStringCursor.DoGetOwner: TSilStringContainer;
begin
  Result := TSilStringContainer(inherited Owner);
end;

end.
