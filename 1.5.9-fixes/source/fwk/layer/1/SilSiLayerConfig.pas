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

unit SilSiLayerConfig;

{$include Defines.inc}

interface

uses
  Sil,
  SilSiLayer;

type
  ILayerConfig = interface;
  ILayerConfigScope = interface;
  ILayerConfigChains = interface;
  ILayerConfigChain = interface;

  ILayerConfig = interface
    ['{845A19BE-79D5-42F0-9E35-437E21C29CB7}']
    function GetScope: ILayerConfigScope;
    function GetChains: ILayerConfigChains;
    property Scope: ILayerConfigScope read GetScope; 
    property Chains: ILayerConfigChains read GetChains;
  end;

  ILayerConfigScope = interface (IArgumentList)
    ['{D5B34035-0185-4DDC-9338-E2A22BF16B44}']
    function GetName: string;
    function GetParent: ILayerConfigScope;
    function Add(const Name: String; const IID: TGUID; const Getter: IUnknown; const Setter: PUnknown = nil): ILayerConfigScope; overload;
    function Add(const Name: String; const IID: TGUID; const Setter: PUnknown): ILayerConfigScope; overload;
    property Name: string read GetName;
    property Parent: ILayerConfigScope read GetParent;
  end;

  ILayerConfigChains = interface
    ['{8ADF3AD9-DA55-463D-AA9C-27C9C72F577C}']
   
  end;

  ILayerConfigChain = interface
    ['{A1030D44-5AD0-478C-AEE4-AFEBC294A037}']

  end;

implementation
end.
 