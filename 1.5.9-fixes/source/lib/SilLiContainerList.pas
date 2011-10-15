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

unit SilLiContainerList;

{$I Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiContainerBase;

type
  PPListItem = ^PListItem;
  PListItem = ^RListItem;

  TListLink = (llNext, llPrev);

  RListItem = record
    Magic: LongWord;
    Owner: Pointer;
    Link: array[TListLink] of PListItem;
    Data: record end;
  end;

const
  CListMagic = LongWord(Ord('L') shl 24 + Ord('i') shl 16 + Ord('n') shl 8 + Ord('k'));

type
  IBaseList = interface (IBaseContainer)
    ['{8EBA6331-476C-4B63-9F4F-BE83EBB0B71D}']
    function GetFirst: PListItem;
    function GetLast: PListItem;
    procedure Remove(Rec: PListItem; FreeItem: Boolean = True);
    function Insert(Before, Rec: PListItem): HItem; overload;
    property First: PListItem read GetFirst;
    property Last: PListItem read GetLast;
  end;
  
implementation
end.
