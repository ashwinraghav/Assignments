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

unit SilLmListEnumerator;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLiList,
  SilLkAbstractEnumerator;

type
  TListEnumerator = class(TAbstractEnumerator)
  private
    FCurrent: Pointer;                //apunta al elemento actual, el que sería devuelto por el Get
    function GetList: IList;          //devuelve FList casteada como IList
    function GetItem: Pointer;        //devuleve el item en FList apuntado por FIteration
  protected
    FList: Pointer;                   //mantiene la lista que estamos enumerando
    property List: IList read GetList;
  protected
    function DoHasMore: Boolean; override;
    function Next: Boolean; override; //pone en FCurrent el siguiente elemento en la sequencia
    function GetCurrent: Pointer; override; // simplemente devuelve FCurrent
    procedure DoReset; override;      //asigna a FCurrent el primer elemento de la lista
    function Check(out Item: Pointer): Boolean; virtual; //determina si el elemento actual, y el que apunta la iteracion son el mismo
  public
    constructor Create(const Container: IUnknown; const Locked: Boolean = false; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil); override;
  end;

implementation

{ TListEnumerator }

constructor TListEnumerator.Create(const Container: IUnknown; const Locked: Boolean; const TypeHandler: HandlerType; const TypeData: Pointer);
begin
  inherited;
  FList := Pointer(Container as IList);
end;

function TListEnumerator.DoHasMore: Boolean;
begin
  Result := FIteration < List.Count;
//  if not Result then Detach;
end;

function TListEnumerator.Next: Boolean;
  var Item: Pointer;
begin
  if Check(Item) then
    begin //- si todo está segun la secuencia, entonces simplemente incrementa en uno y memoriza el siguiente item
      Inc(FIteration);
      Result := HasMore;
      if Result
        then FCurrent := GetItem
        else Detach; // en el caso de que ya no haya más, esto desatacha el enumerador de la lista 
    end
  else
    begin //si se borró algun elemento, entonces se memoriza el siguiente en la secuencia. 
      FCurrent := Item;
      Result := True;
    end;
end;

function TListEnumerator.Check(out Item: Pointer): Boolean;
begin
//- Checkeo que el item al que apunta FCurrent sea el mismo que me indica el FIteration,
//- si no lo fuera eso sería porque se han borrado items durante la iteración anterior 
  if HasMore then
    begin
      Item := GetItem;
      if Item = FCurrent then
        Item := nil;
    end
  else
    Item := nil;

  Result := (Item = nil);
end;

function TListEnumerator.GetCurrent: Pointer;
begin
  Result := FCurrent; //devuelvo el que dejó seteado el último llamado a Next   
end;

procedure TListEnumerator.DoReset;
begin
  inherited; // Acá se resetea FIteration a cero.
  FCurrent := GetItem; // y esto asigna a FCurrent el primero de la lista
end;

function TListEnumerator.GetList: IList;
begin
  Result := IList(FList);
end;

function TListEnumerator.GetItem: Pointer;
begin
  //Result := List.Get(FIteration);
  Result := List.ItemPtr(FIteration);
end;

end.
