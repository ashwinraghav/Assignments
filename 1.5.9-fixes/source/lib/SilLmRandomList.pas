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

unit SilLmRandomList;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLkList,
  SilLiPointerList,
  SilLiBits;

type
  //
  // Lista de elementos intercalados
  //
  TCustomRandomList = class(TSilList)
  protected
    FLastIndex: integer;
    FBitmap: IBits;
  private
    function DoSearchPos(Index: integer; var Position: integer): boolean;
    function DoSearchPtr(APointer: Pointer; var Position: integer): boolean;
    procedure DoDelete(Index: integer);
    procedure DoCalcLastIndex;
    function DoAddItem(AIndex: integer; APointer: Pointer): Integer;
  protected // IList
    function GetCount: integer; override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Delete(Index: Integer); override;
  protected // IPointerList
    function First: Pointer;
    function Last: Pointer;
    function IndexOf(APointer: Pointer): Integer;
    function Add(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Remove(Item: Pointer): Integer;
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; APointer: Pointer);
    procedure SetCount(NewCount: Integer); override;
  protected // IRandomPointerList
    function GetFirstIndex: integer;
    function GetUsedCount: integer;
    function GetUsedItem(Index: integer): Pointer;
    procedure SetUsedItem(Index: integer; const Value: Pointer);
    function GetUsedIndex(Index: integer): integer;
    function FirstFree: Integer;
  protected // misc
    procedure Move(CurIndex, NewIndex: Integer);
    function Extract(Item: Pointer): Pointer;
    function GetUsed(Index: integer; var Position: integer): Pointer;
    procedure Pack;
  protected // properties
    property UsedItems[Index: integer]: Pointer read GetUsedItem write SetUsedItem;
    property FirstIndex: integer read GetFirstIndex;
    property LastIndex: integer read FLastIndex;
    property UsedCount: integer read GetUsedCount;
    property UsedIndex[Index: integer]: integer read GetUsedIndex;
    property Items[Index: integer]: Pointer read GetItem write SetItem;
  public
    constructor Create(Locked: Boolean = false; CastPtr: HandlerType = nil; CastData: Pointer = nil); override; 
    destructor Destroy; override;
  end;

  TRandomList = class(TCustomRandomList, IPointerList)
  protected // IPointerList
    procedure AddList(const Source: IPointerList);
  public
    property Items;
    property UsedItems;
  end;

implementation

uses
  SilBtError,
  SilLdBaseList,
  SilLmBits,
  SilBtVoidPtr;

type
  //
  // Elemento de la lista de punteros
  //
  TRandomPointerItem = class(TObject)
    Index: integer;
    Pointer: Pointer;
  public
    constructor Create(AIndex: integer; APointer: Pointer);
    destructor Destroy; override; 
  end;

{ TCustomRandomList }

constructor TCustomRandomList.Create(Locked: Boolean; CastPtr: HandlerType; CastData: Pointer);
begin
  inherited Create(Locked, ObjectHandler, CastData);
  FBitmap := TSilBits.Create();
end;

destructor TCustomRandomList.Destroy;
begin
  FBitmap := nil;
  inherited;
end;

procedure TCustomRandomList.Delete(Index: integer);
var
  i1: integer;
begin
  if DoSearchPos( Index, i1 ) then
    DoDelete( i1 );

  if ( Index = FLastIndex ) then
    DoCalcLastIndex();
end;

function TCustomRandomList.Add(Item: Pointer): Integer;
begin
  if ( Count > 0 ) then
    Result := FLastIndex + 1 else
    Result := 0;
  SetItem( Result, Item );
end;

procedure TCustomRandomList.Insert(Index: Integer; Item: Pointer);
var
  i1, i2: integer;
begin
  DoSearchPos( Index, i1 );

  // Incrementa las posiciones de los elementos posteriores
  for i2 := i1 to Count - 1 do
    with TRandomPointerItem( List^[ i2 ] ) do
    begin
      FBitmap[Index] := False;
      Inc(Index);
      FBitmap[Index] := True;
    end;

  SetItem( Index, Item );
end;

procedure TCustomRandomList.SetItem(Index: integer; APointer: Pointer);
var
  i1: integer;
begin

  // Elimina un elemento
  if ( APointer = nil ) then
  begin
    Delete( Index );
    exit;
  end;

  if ( Index > FLastIndex ) then
  begin
    // Agrega al final
    FLastIndex := Index;
    DoAddItem(Index, APointer);
  end
  else if DoSearchPos( Index, i1 ) then
    // Actualiza un elemento existente
    TRandomPointerItem( List^[ i1 ] ).Pointer := APointer
  else
    // Agrega un nuevo elemento
    DoAddItem(Index, APointer);
    
end;

function TCustomRandomList.First: Pointer;
var
  Obj: TRandomPointerItem;
begin
  ItemGet(0, Obj);
  Result := Obj.Pointer;
end;

function TCustomRandomList.Last: Pointer;
var
  Obj: TRandomPointerItem;
begin
  ItemGet(Count - 1, Obj);
  Result := Obj.Pointer;
end;

function TCustomRandomList.GetItem(Index: integer): Pointer;
var
  i1: integer;
begin
  if DoSearchPos( Index, i1 ) then
    Result := TRandomPointerItem( List^[ i1 ] ).Pointer else
    Result := nil;
end;

function TCustomRandomList.GetCount: integer;
begin
  if ( Count > 0 ) then
    Result := LastIndex + 1 else
    Result := 0;
end;

procedure TCustomRandomList.Exchange(Index1, Index2: Integer);
var
  ptr1: Pointer;
begin
  CheckIndex(Index1);
  CheckIndex(Index2);

  // Realiza el intercambio - No está optimizada
  ptr1 := GetItem( Index1 );
  SetItem( Index1, GetItem( Index2 ) );
  SetItem( Index2, ptr1 );
end;

function TCustomRandomList.Extract(Item: Pointer): Pointer;
var
  i1, i2: integer;
begin
  Result := nil;
  if DoSearchPtr( Item, i1 ) then
  begin
    Result := TRandomPointerItem( List^[ i1 ] ).Pointer;
    i2 := TRandomPointerItem( List^[ i1 ] ).Index;
    TRandomPointerItem( List^[ i1 ] ).Pointer := nil;
    DoDelete( i1 );
    if ( i2 = FLastIndex ) then
      DoCalcLastIndex();
  end;
end;

procedure TCustomRandomList.Move(CurIndex, NewIndex: Integer);
var
  i1: integer;
begin
  if ( CurIndex <> NewIndex ) then
  begin
    if DoSearchPos( CurIndex, i1 ) then
      SetItem( NewIndex, TRandomPointerItem( List^[ i1 ] ).Pointer ) else
      Delete( NewIndex );
    Delete( CurIndex );
  end;
end;

procedure TCustomRandomList.Pack;
begin
  // No hace nada
end;

function TCustomRandomList.Remove(Item: Pointer): Integer;
var
  i1: integer;
begin
  if DoSearchPtr( Item, i1 ) then
  begin
    Result := TRandomPointerItem( List^[ i1 ] ).Index;
    DoDelete( i1 );
    if ( Result = FLastIndex ) then
      DoCalcLastIndex();
  end else
    Result := -1;
end;

procedure TCustomRandomList.SetCount(NewCount: Integer);
begin
  if ( Count > 0 ) and ( FLastIndex + 1 > NewCount ) then
  begin
    while ( Count > 0 ) and ( FLastIndex + 1 > NewCount ) do
      DoDelete( Count - 1 );
    DoCalcLastIndex;
  end;
end;

function TCustomRandomList.IndexOf(APointer: Pointer): integer;
var
  i1: integer;
begin
  if DoSearchPtr( APointer, i1 ) then
    Result := TRandomPointerItem( List^[ i1 ] ).Index else
    Result := -1;
end;

function TCustomRandomList.GetFirstIndex: integer;
var
  Obj: TRandomPointerItem;
begin
  if ( Count > 0 ) then
  begin
    ItemGet(0, Obj);
    Result := Obj.Index;
  end else
    Result := -1;
end;

function TCustomRandomList.GetUsedCount: integer;
begin
  Result := Count;
end;

function TCustomRandomList.GetUsedItem(Index: integer): Pointer;
var
  Obj: TRandomPointerItem;
begin
  ItemGet(Index, Obj);
  Result := Obj.Pointer;
end;

procedure TCustomRandomList.SetUsedItem(Index: integer; const Value: Pointer);
var
  Obj: TRandomPointerItem;
begin
  ItemGet(Index, Obj);
  Obj.Pointer := Value;
end;

function TCustomRandomList.GetUsed(Index: integer; var Position: integer): Pointer;
var
  Obj: TRandomPointerItem;
begin
  ItemGet(Index, Obj);
  Position := Obj.Index;
  Result := Obj.Pointer;
end;

function TCustomRandomList.GetUsedIndex(Index: integer): integer;
var
  Obj: TRandomPointerItem;
begin
  ItemGet(Index, Obj);
  Result := Obj.Index;
end;

function TCustomRandomList.FirstFree: Integer;
begin
  Result := FBitmap.GetFirst;
end;

function TCustomRandomList.DoAddItem(AIndex: integer; APointer: Pointer): Integer;
var
  Item: Pointer;
begin
  Item := TRandomPointerItem.Create(AIndex, APointer);
  Result := inherited ItemAdd(Item^);
  FBitmap[AIndex] := True;
end;

procedure TCustomRandomList.DoDelete( Index: integer );
var
  item: TRandomPointerItem;
begin
  CheckIndex(Index);
  item := List^[Index];
  FBitmap[item.Index] := False;
  inherited Delete(Index);
end;

//
// Realiza una búsqueda secuencial de un puntero
//
function TCustomRandomList.DoSearchPtr( APointer: Pointer; var Position: integer ): boolean;
var
  i1: integer;
begin
  Result := true;
  for i1 := 0 to Count - 1 do
    if ( TRandomPointerItem( List^[ i1 ] ).Pointer = APointer ) then
    begin
      Position := i1;
      exit;
    end;
  Result := false;
end;

//
// Realiza una búsqueda binaria del elemento
//
function TCustomRandomList.DoSearchPos( Index: integer; var Position: integer ): boolean;
var
  binary: boolean;
  i1, i2, p1, d1, p2, d2: integer;
begin
  i1 := 0;
  i2 := Count - 1;

  // Si hay mucha dispersión busca solo en modo binario
  binary := ( LastIndex > Count * 2 );

  while ( i1 <= i2 ) do
  begin
    p1 := ( i1 + i2 ) div 2;
    if not binary and ( i2 > i1 + 16 ) then
    begin
      p2 := p1;
      p1 := i1 + ( Index - FirstIndex ) * Round( ( i2 - i1 ) / ( LastIndex - FirstIndex ) );
      if ( p1 < i1 ) then p1 := i1;
      if ( p1 > i2 ) then p1 := i2;
    end
    else
      p2 := p1;

    d1 := TRandomPointerItem( List^[ p1 ] ).Index - Index;
    if ( d1 = 0 ) then
    begin
      Result := true;
      Position := p1;
      exit;
    end;
    if ( p1 <> p2 ) then
    begin
      d2 := TRandomPointerItem( List^[ p2 ] ).Index - Index;
      if ( d2 = 0 ) then
      begin
        Result := true;
        Position := p2;
        exit;
      end;

      if ( d1 < 0 ) then
        i1 := p1 + 1 else
        i2 := p1 - 1;

      if ( d2 < 0 ) then
      begin
        if ( p2 >= i1 ) then
        begin
          i1 := p2 + 1;
          binary := true;
        end
      end
      else
        if ( p2 <= i2 ) then
        begin
          i2 := p2 - 1;
          binary := true;
        end;
    end
    else if ( d1 < 0 ) then
      i1 := p1 + 1 else
      i2 := p1 - 1;
  end;
  Position := i1;
  Result := false;
end;

procedure TCustomRandomList.DoCalcLastIndex;
var
  Obj: TRandomPointerItem;
begin
  if ( Count > 0 ) then
  begin
    ItemGet(Count - 1, Obj);
    FLastIndex := Obj.Index;
  end else
    FLastIndex := 0;
end;

{ TRandomPointerItem }

constructor TRandomPointerItem.Create(AIndex: integer; APointer: Pointer);
begin
  Index := AIndex;
  Pointer := APointer;
end;

destructor TRandomPointerItem.Destroy;
begin
  inherited;
end;

{ TRandomList }

procedure TRandomList.AddList(const Source: IPointerList);
var
  i: Integer;
begin
  for i := 0 to Source.Count - 1 do
    Add(Source.Items[i]);
end;

end.

