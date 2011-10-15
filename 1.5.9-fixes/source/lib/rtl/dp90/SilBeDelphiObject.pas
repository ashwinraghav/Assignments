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

unit SilBeDelphiObject;

{$I Defines.inc}

interface

uses
  SilBcDelphi;

const
  vmtSelfPtr           = System.vmtSelfPtr;
  vmtIntfTable         = System.vmtIntfTable;
  vmtAutoTable         = System.vmtAutoTable;
  vmtInitTable         = System.vmtInitTable;
  vmtTypeInfo          = System.vmtTypeInfo;
  vmtFieldTable        = System.vmtFieldTable;
  vmtMethodTable       = System.vmtMethodTable;
  vmtDynamicTable      = System.vmtDynamicTable;
  vmtClassName         = System.vmtClassName;
  vmtInstanceSize      = System.vmtInstanceSize;
  vmtParent            = System.vmtParent;
                         
type
  PObjectClass = ^RObjectClass;
  
  RObjectClass = record
    SelfPtr: Pointer;
    IntfTable: Pointer;
    AutoTable: Pointer;
    InitTable: Pointer;
    TypeInfo: Pointer;
    FieldTable: Pointer;
    MethodTable: Pointer;
    DynamicTable: Pointer;
    ClassName: PShortString;
    InstanceSize: Integer;
    Parent: Pointer;             
    SafeCallException: Pointer;  
    AfterConstruction: Pointer;  
    BeforeDestruction: Pointer;  
    Dispatch: Pointer;           
    DefaultHandler: Pointer;     
    NewInstance: Pointer;        
    FreeInstance: Pointer;       
    Destroy: Pointer;
  end;

implementation

end.
 