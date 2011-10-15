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

unit SilSiResourcePool;

{$I Defines.inc}

interface

uses
  SilLiInterfaceList;

type
  IResourcePool = interface;
  IPoolItem = interface;
  IPoolItemManager = interface;

  IPoolItem = interface
    ['{0B10C9FF-C6D9-11D4-989F-00104B0FA1EF}']
    function Activate: Boolean;
    function Deactivate: Boolean;
    function InUse: Boolean;
  end;

  IPoolItemManager = interface
    ['{0B10CA00-C6D9-11D4-989F-00104B0FA1EF}']
    function Get(const Pool: IResourcePool): IPoolItem;
    function Select(const Pool: IResourcePool; Param: Pointer): IPoolItem;
    function Release(const Pool: IResourcePool; const Item: IPoolItem): Boolean;
  end;

  IResourcePool = interface
    ['{0B10C9FE-C6D9-11D4-989F-00104B0FA1EF}']
    function GetMaxSize: LongWord;
    procedure SetMaxSize(Value: LongWord);
    function GetSlackSize: LongWord;
    procedure SetSlackSize(Value: LongWord);
    function GetCacheSize: LongWord;
    procedure SetCacheSize(Value: LongWord);
    function GetActualSize: LongWord;
    function GetList: IInterfaceList;
    function Get(out Item: IPoolItem; Timeout: LongWord = 0): Boolean;
    function Select(Param: Pointer; out Item: IPoolItem): Boolean;
    function Release(const Item: IPoolItem): Boolean;
    property MaxSize: LongWord read GetMaxSize write SetMaxSize;
    property SlackSize: LongWord read GetSlackSize write SetSlackSize;
    property CacheSize: LongWord read GetCacheSize write SetCacheSize;
    property ActualSize: LongWord read GetActualSize;
    property List: IInterfaceList read GetList;
  end;

implementation

end.
