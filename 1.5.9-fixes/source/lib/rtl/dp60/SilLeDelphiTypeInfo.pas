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

unit SilLeDelphiTypeInfo;

{$I Defines.inc}

interface

uses
  TypInfo,
  SilBeTypeInfo;

type // copiados de System.pas (dependen de la version de Delphi!!!!)
  PPropData = ^TPropData;
  PIntfMethods = ^TIntfMethods;
  PParamData = ^TParamData;
  PParamInfo = ^TParamInfo;
  PParamRec = ^TParamRec;
  PMethodRec = ^TMethodRec;
  PMethodData = ^TMethodData;
  PMethodParams = ^TMethodParams;

  TIntfMethods = packed record
    Count: Word;
    Data: record end;
  end;

  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

  TParamData = packed record
    DataType: string;
    Info: PPTypeInfo;
  end;

  TParamInfo = packed record
    Name: string;
    Flags: TParamAttributes;
  end;

  TParamRec = packed record
    Data: TParamData;
    Info: TParamInfo;
  end;

  TMethodData = record
    Name: string;
    Kind: TMethodKind;
    Call: TCallingKind;
  end;

  TMethodParams = record
    List: array of TParamRec;
    Result: TParamData;
  end;

  TMethodRec = record
    Data: TMethodData;
    Params: TMethodParams;
  end;

implementation

end.
 