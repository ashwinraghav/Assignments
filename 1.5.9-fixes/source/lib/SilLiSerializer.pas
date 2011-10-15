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

unit SilLiSerializer;

{$I Defines.inc}

interface

uses
  SilLiStream;

type
  ISerializable = interface
    ['{750707B2-25BA-11D4-9880-00104B0FA1EF}']
    procedure SaveToStream(const Stream: IStream);
    procedure LoadFromStream(const Stream: IStream);
  end;


  IEvOnTransferInit = interface;
  IEvOnTransferProgress = interface;
  IEvOnTransferFinished = interface;
  
  RTransferEvents = record
    OnInit: IEvOnTransferInit;
    OnProgress: IEvOnTransferProgress;
    OnFinished: IEvOnTransferFinished;
  end;

  REvOnTransfer = record
    Source, Dest: IStream;
    BytesCount: LongWord;
    BytesTotal: LongWord;
    BytesCopied: LongWord;
    BytesRemain: LongWord;
    ChunkSize: Integer;
    Iterations: Integer;
    Context: IUnknown;
    ReadTime: Double;
    WriteTime: Double;
    ExtraTime: Double;
    TotalTime: Double;
    AccumTime: Double;
    Events: RTransferEvents;
  end;

  IEvOnTransferInit = interface
    ['{4C398244-BE84-4CD4-B52C-23A82034B6FB}']
    procedure OnTransferInit(var Event: REvOnTransfer);
  end;

  IEvOnTransferProgress = interface
    ['{D1006857-EA10-41A3-BAE9-961D0B185484}']
    procedure OnTransferProgress(var Event: REvOnTransfer);
  end;

  IEvOnTransferFinished = interface
    ['{F9958D7B-B9DE-4B17-8787-3C5803A653A3}']
    procedure OnTransferFinished(var Event: REvOnTransfer);
  end;

implementation
end.
 