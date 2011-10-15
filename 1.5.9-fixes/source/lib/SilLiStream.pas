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

unit SilLiStream;

{$I Defines.inc}

interface

uses
  SilBeTypes;

type
  LongWord = SilBeTypes.LongWord;

{$MINENUMSIZE 4}

type
  TSeekOrigin = (soFromBeginning, soFromCurrent, soFromEnd);

  IStream = interface
    ['{DDA63E32-1180-11D4-987D-00104B0FA1EF}']
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    property Size: LongWord read GetSize;
  end;

  IRandomStream = interface (IStream)
    ['{A88D09B2-21D2-11D4-987F-00104B0FA1EF}']
    function GetPosition: LongWord;
    procedure SetPosition(Pos: LongWord);
    procedure SetSize(NewSize: LongWord);
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
    procedure Truncate;
    property Position: LongWord read GetPosition write SetPosition;
    property Size: LongWord read GetSize write SetSize;
  end;

  ITextStream = interface (IRandomStream)
    ['{56D651D1-5BF7-11D4-988A-00104B0FA1EF}']
    function ReadStr(var Buffer: String): LongWord;
    function WriteStr(const Buffer: String): LongWord;
    function ReadLn(var Buffer: String): Boolean;
    function WriteLn(const Buffer: String = ''): Boolean;
  end;

  IMemoryStream = interface (IRandomStream)
    ['{DDA63E33-1180-11D4-987D-00104B0FA1EF}']
    function GetMemory: PChar;
    function GetRemaining: LongWord;
    function GetCurrent: PChar;
    property Memory: PChar read GetMemory;
    property Remaining: LongWord read GetRemaining;
    property Current: PChar read GetCurrent;
  end;

  IBufferStream = interface (IMemoryStream)
    ['{BC9D0623-623E-4079-9DB4-B4424052D8FD}']
    function GetSource: IStream;
    procedure SetSource(const Value: IStream);
    property Source: IStream read GetSource write SetSource;
  end;

  IWriteOnlyStream = interface (IStream)
    ['{3B78387F-3A36-4E8E-8DA2-CC4D07D1E4C7}']
    function Buffer: String;
  end;

  TStreamBufferEvent = record
    Sender: IStream;
    Buffer: PChar;
    Size: LongWord;
  end;

  IStreamEvents = interface
    ['{79691EF2-840F-11D4-9898-00104B0FA1EF}']
    procedure OnWrite(var Event: TStreamBufferEvent);
    procedure OnRead(var Event: TStreamBufferEvent);
  end;

  TStreamPositionEvent = record
    Sender: IStream;
    OldPosition: LongWord;
    NewPosition: LongWord;
  end;

  IRandomStreamEvents = interface (IStreamEvents)
    ['{79691EF3-840F-11D4-9898-00104B0FA1EF}']
    procedure OnPosition(var Event: TStreamPositionEvent);
  end;

implementation

end.
