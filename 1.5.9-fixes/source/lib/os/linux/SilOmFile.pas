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

unit SilOmFile;

{$I Defines.inc}

interface

uses
  SysUtils,
  libc,

  SilOsTypes,

  SilLiStream,
  SilOiHandle,
  SilOiFile,
  SilOkFile,

  SilOmHandle;

type
  TSilOsFile = class (TSilFile)
  protected // TSilHandledObject
    function DoCreateHandle(const Value: THandle; const MustFree: Boolean = True): IHandle; override;
  protected // TSilFile
    function DoGetInfo(const FileName: string): IFileInfoDef; override;
    function DoOpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): THandle; override;
    function DoCreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): THandle; override;
  protected // IFile
    function FlushBuffer: Boolean; override;
  protected // IRandomStream
    function GetPosition: LongWord; override;
    procedure SetPosition(Pos: LongWord); override;
    function GetSize: LongWord; override;
    procedure SetSize(NewSize: LongWord); override;
    function Read(var Buffer; Count: LongWord): LongWord; override;
    function Write(const Buffer; Count: LongWord): LongWord; override;
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord; override;
    procedure Truncate; override;
  protected // ICloneable
    function Clone: IUnknown; override;
  end;

type
  TSilFileHandle = class (TSilHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  end;

function __open(__file: Pchar; __oflag: longint; args: longint): longint; cdecl; external 'c' name 'open';

implementation

uses
  SilOfFile,
  SilOmObjectHandle,
  SilOmFileInfo,
  SilOsError;

const
  FileAccessMode: array[TFileAccessMode] of Integer = (
      O_RDONLY,
      O_WRONLY,
      O_RDWR
    );

  FileShareMode: array[TFileShareMode] of Integer = (
      0 {O_EXLOCK}, {share none}
      0 {O_SHLOCK},
      0 {O_SHLOCK},
      0
    );

  OpenDisposition: array[Boolean] of Integer = (
      O_CREAT {OPEN_ALWAYS},
      0       {OPEN_EXISTING}
    );

  CreateDisposition: array[Boolean] of Integer = (
      O_CREAT + O_EXCL {CREATE_NEW},
      O_CREAT + O_TRUNC {CREATE_ALWAYS}
    );

{ TSilOsFile }

function TSilOsFile.DoCreateHandle(const Value: THandle; const MustFree: Boolean): IHandle;
begin
  Result := TSilFileHandle.Create(Value, MustFree);
end;

function TSilOsFile.DoGetInfo(const FileName: string): IFileInfoDef;
begin
  Result := TSilLinuxFileInfo.Create(Handle, FileName);
end;

function TSilOsFile.DoOpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): THandle;
begin
  Result := __open(PChar(FileName), FileAccessMode[Access] or FileShareMode[Share] or OpenDisposition[MustExists], 422);
  if Result < 0 then raise OsError.Create(OsCode(libc.errno), 'TSilOsFile.DoOpenFile [libc.open] "%s"', [FileName]);
end;

function TSilOsFile.DoCreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): THandle;
begin
  Result := __open(PChar(FileName), FileAccessMode[Access] or FileShareMode[Share] or CreateDisposition[MustCreate], 422);
  if Result < 0 then raise OsError.Create(OsCode(libc.errno), 'TSilOsFile.DoCreateFile [libc.open] "%s"', [FileName]);
end;

function TSilOsFile.FlushBuffer: Boolean;
begin
  Result := false;//libc.flush(Self.Handle.Value);
end;

function TSilOsFile.GetPosition: LongWord;
begin
  Result := Seek(0, soFromCurrent);
end;

procedure TSilOsFile.SetPosition(Pos: LongWord);
begin
  Seek(Pos, soFromBeginning);
end;

function TSilOsFile.GetSize: LongWord;
begin
  SilOfFile.GetFileSize(Self.Handle.Value, Result);
end;

procedure TSilOsFile.SetSize(NewSize: LongWord);
begin
  OsError.Check(libc.ftruncate(Self.Handle.Value, Seek(NewSize, soFromBeginning)));
end;

function TSilOsFile.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := libc.__read(Self.Handle.Value, Buffer, Count);

  if not (libc.errno in [2 {no file}]) then
    OsError.Check(libc.errno, 'TSilOsFile.Read [Linux.ReadFile]');
end;

function TSilOsFile.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := libc.__write(Self.Handle.Value, Buffer, Count);

  if not (libc.errno in [2 {no file}, 4 {interrupted}, 11 {try again}]) then
    OsError.Check(libc.errno, 'TSilOsFile.Write [Linux.WriteFile]');
end;

function TSilOsFile.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
const
  Map: array[TSeekOrigin] of Integer = (SEEK_SET, SEEK_CUR, SEEK_END);
var
  State: Integer;
begin
  State := libc.lseek(Handle.Value, Offset, Map[Origin]);
  //if Result < 0 then raise OsError.Create(libc.errno, 'TSilOsFile.DoCreateFile [libc.open] "%s"', [FileName]);
  Result := State;
end;

procedure TSilOsFile.Truncate;
begin
  OsError.Check(libc.ftruncate(Self.Handle.Value, GetPosition), 'TSilOsFile.Truncate');
end;

function TSilOsFile.Clone: IUnknown;
begin(*)
  Result := TSilOsFile.OpenFile(FFileInfo.FullName, fmAccessReadWrite, fmShareReadWrite, true);
(*)end;

{ TSilFileHandle }

procedure TSilFileHandle.HandleClose;
begin
  OsError.Check(libc.__close(Handle), 'TSilFileHandle.HandleClose');
  inherited;
end;

procedure TSilFileHandle.HandleIsValid(var Result: Boolean);
begin
  Result := (Handle > INVALID_HANDLE_VALUE);
end;

end.
