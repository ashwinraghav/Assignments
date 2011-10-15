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

unit SilLiLinkedList;

{$I Defines.inc}

interface

uses
  SilLiList,
  SilLiEnumerator;

type
  IItems = interface
    ['{0E412CCC-79AD-46F5-822D-120B1B6FC4DA}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;
  
  IUntypedItems = interface (IItems)
    ['{B04DFBAC-B808-45E4-B66F-7D3882060020}']
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
    function Contains(const Item): Boolean;
    function GetFirst(var Item): Boolean;
    function GetLast(var Item): Boolean;
  end;

  IUntypedList = interface (IItems)
    ['{2B87013D-C8FD-4C83-A576-AC03F5E2F77D}']
  end;

  ILinkedItems = interface (IUntypedItems)
    ['{59B7E223-BF83-443A-B473-A1445E85DD98}']
  end;

  ILinkedList = interface (ILinkedItems)
    ['{0068D9ED-AB7B-4F36-9973-09CFC910C0FF}']
    procedure Clear;
    procedure Add(const Item);
    procedure Insert(const Before; const Item);
    procedure Remove(const Item);
  end;

  ILinkedPointerItems = interface (IItems)
    ['{D6DF4841-2F21-4EC0-BBBA-57A36B47B120}']
    function Contains(const Item: Pointer): Boolean;
  end;

  ILinkedPointerList = interface (ILinkedPointerItems)
    ['{C547501F-7BF8-4A16-918C-223614B3728F}']
  end;
  
  ILinkedInterfaceItems = interface (IItems)
    ['{17002E41-DFF3-43EB-9971-B3EAFC46686C}']
  end;

  ILinkedInterfaceList = interface (ILinkedInterfaceItems)
    ['{89FBA9E8-9303-4C07-BFF5-BEB1D7A3570D}']
  end;

  ILinkedVariantItems = interface (IItems)
    ['{2348C3D1-4C00-4D58-BFD7-58B74CDD1C4F}']
  end;
  
  ILinkedVariantList = interface (ILinkedVariantItems)
    ['{1C8A56EA-2862-42B1-BB53-CEB046564A25}']
  end;

  ILinkedStringItems = interface (IItems)
    ['{97B4AFE5-DBC6-43B8-889D-4F72F71EAABD}']
  end;
 
  ILinkedStringList = interface (ILinkedStringItems)
    ['{7A3DA586-CDAE-4F19-8865-9395B5EBF983}']
  end;

implementation
end.
