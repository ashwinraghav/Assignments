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

unit SilLtSerializer;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiStream,
  SilBkTool;

type
  SerializerClass = class of Serializer;

  Serializer = class(Tool)
    class procedure LoadFromFile(const This: IUnknown; const FileName: String);
    class procedure SaveToFile(const This: IUnknown; const FileName: String);
    class procedure SaveToStream(const This: IUnknown; const Stream: IStream);
    class procedure LoadFromStream(const This: IUnknown; const Stream: IStream);
    class procedure WriteStream(const Source, Dest: IStream; Size: Integer = -1; Chunk: Integer = Word(-1); const Listener: IUnknown = nil; const Context: IUnknown = nil);
    class function LoadFromBuffer(const This: IUnknown; const Buffer; Size: Integer): Integer;
    class function SaveToBuffer(const This: IUnknown; const Buffer; Size: Integer): Integer;
    class function LoadFromString(const This: IUnknown; const Buffer: String): Integer;
    class function SaveToString(const This: IUnknown; var Buffer: String): Integer;

    class function ReadFromFile(const FileName: String; out Buffer: String): Boolean; overload;
    class function ReadFromFile(const FileName: String; out Buffer: WideString): Boolean; overload;
    class function WriteToFile(const FileName, Buffer: String): Boolean; overload;
    class function WriteToFile(const FileName: String; const Buffer: WideString): Boolean; overload;
  end;

implementation

uses
  SilLtStream,
  SilLtReference,
  SilLiSerializer,
  SilOiPerformance,
  SilOiFile,
  SilOtTool;

class function Serializer.LoadFromBuffer(const This: IInterface; const Buffer; Size: Integer): Integer;
var
  Mem: IMemoryStream;
begin
  Mem := Stream.Memory(Size, @Buffer);
  LoadFromStream(This, Mem);
  Result := Mem.Position;
end;

class function Serializer.SaveToBuffer(const This: IInterface; const Buffer; Size: Integer): Integer;
var
  Mem: IMemoryStream;
begin
  Mem := Stream.Memory(Size, @Buffer);
  SaveToStream(This, Mem);
  Result := Mem.Position;
end;

class function Serializer.LoadFromString(const This: IInterface; const Buffer: String): Integer;
begin
  Result := LoadFromBuffer(This, Buffer[1], Length(Buffer));
end;

class function Serializer.SaveToString(const This: IInterface; var Buffer: String): Integer;
var
  Mem: IMemoryStream;
begin
  Mem := Stream.Memory;
  SaveToStream(This, Mem);
  Result := Mem.Size;

  SetLength(Buffer, Result);
  Mem.Position := 0;
  Mem.Read(Buffer[1], Result);
end;

class procedure Serializer.LoadFromFile(const This: IUnknown; const FileName: String);
var
  Storage: IFile;
begin
  Storage := OS.FileSystem.OpenFile(FileName, fmAccessRead, fmShareReadWrite, true);
  LoadFromStream(This, Storage.Stream);
end;

class procedure Serializer.LoadFromStream(const This: IUnknown; const Stream: IStream);
var
  Obj: ISerializable;
begin
  if Reference.GetInterface(This, ISerializable, Obj) then Obj.LoadFromStream(Stream);
end;

class procedure Serializer.SaveToFile(const This: IUnknown; const FileName: String);
var
  Storage: IFile;
begin
  Storage := OS.FileSystem.OpenFile(FileName);
  SaveToStream(This, Storage.Stream);
end;

class procedure Serializer.SaveToStream(const This: IUnknown; const Stream: IStream);
var
  Obj: ISerializable;
  Src: IStream;
  Truncable: IRandomStream;
begin
  if Reference.GetInterface(This, ISerializable, Obj) then Obj.SaveToStream(Stream) else
  if Reference.GetInterface(This, IStream, Src) then WriteStream(Src, Stream);
  if Reference.GetInterface(Stream, IRandomStream, Truncable) then Truncable.Truncate;
end;

class procedure Serializer.WriteStream(const Source, Dest: IStream; Size: Integer; Chunk: Integer; const Listener, Context: IUnknown);
var
  sBuf: String;
  lwSize: LongWord;
  Data: REvOnTransfer;
  Perf: IPerformanceCounter;
begin
  
  Ref.GetInterface(Listener, IEvOnTransferInit, Data.Events.OnInit);
  Ref.GetInterface(Listener, IEvOnTransferProgress, Data.Events.OnProgress);
  Ref.GetInterface(Listener, IEvOnTransferFinished, Data.Events.OnFinished);

  Perf := Os.Performance.Create();
   
  Data.Source := Source;
  Data.Dest := Dest;
  Data.BytesTotal := Source.Size;
  Data.BytesCopied := 0;
  Data.BytesRemain := Data.BytesTotal;
  Data.ChunkSize := Chunk;
  Data.Iterations := 0;
  Data.Context := Context;

  SetLength(sBuf, 0);

  if Assigned(Data.Events.OnInit) then Data.Events.OnInit.OnTransferInit(Data);
  
  repeat
    if Size > 0 then
    begin
      if Size - Chunk < 0 then Chunk := Size;
      Dec(Size, Chunk);
    end;

    if Length(sBuf) <> Chunk then SetLength(sBuf, Chunk);

    if Assigned(Data.Events.OnProgress) then Data.ExtraTime := Os.Performance.ToMSeconds(Perf, True); 
    lwSize := Source.Read(sBuf[1], Chunk);
    if Assigned(Data.Events.OnProgress) then Data.ReadTime := Os.Performance.ToMSeconds(Perf, True);

    if lwSize <> 0 then
    begin
      Data.BytesCount := Dest.Write(sBuf[1], lwSize);

      if Assigned(Data.Events.OnProgress) then Data.WriteTime := Os.Performance.ToMSeconds(Perf, True);

      if Assigned(Data.Events.OnProgress) then
      begin

        Data.BytesTotal := Source.Size;
        Inc(Data.BytesCopied, Data.BytesCount);

        if Data.BytesTotal > Data.BytesCopied then
          Data.BytesRemain := Data.BytesTotal - Data.BytesCopied else
          Data.BytesRemain := 0;

        Inc(Data.Iterations);

        Data.TotalTime := Data.ReadTime + Data.WriteTime + Data.ExtraTime;
        Data.AccumTime := Data.AccumTime + Data.TotalTime;
       
        Data.Events.OnProgress.OnTransferProgress(Data);
      end;
      
    end;

  until (Size = 0) or (lwSize = 0);

  if Assigned(Data.Events.OnFinished) then Data.Events.OnFinished.OnTransferFinished(Data);
  
end;

class function Serializer.ReadFromFile(const FileName: String; out Buffer: WideString): Boolean;
var
  Source: IFile;
begin
  try            
    Source := OS.FileSystem.OpenFile(FileName, fmAccessReadWrite, fmShareRead, true);
    SetLength(Buffer, Source.Stream.Size div 2);
    Source.Stream.Read(Buffer[1], Source.Stream.Size);
    Result := true;
  except
    Result := false;
  end;
end;

class function Serializer.ReadFromFile(const FileName: String; out Buffer: String): Boolean;
var
  Source: IFile;
begin
  try
    Source := OS.FileSystem.OpenFile(FileName, fmAccessReadWrite, fmShareRead, true);
    SetLength(Buffer, Source.Stream.Size);
    Source.Stream.Read(Buffer[1], Length(Buffer));
    Result := true;
  except
    Result := false;
  end;
end;

class function Serializer.WriteToFile(const FileName: String; const Buffer: WideString): Boolean;
var
  Source: IFile;
begin
  try
    Source := OS.FileSystem.CreateFile(FileName);
    Source.Stream.Write(Buffer[1], Length(Buffer) * 2);
    Result := true;
  except
    Result := false;
  end;
end;

class function Serializer.WriteToFile(const FileName, Buffer: String): Boolean;
var
  Source: IFile;
begin
  try
    Source := OS.FileSystem.CreateFile(FileName);
    Source.Stream.Write(Buffer[1], Length(Buffer));
    Result := true;
  except
    Result := false;
  end;
end;

end.
