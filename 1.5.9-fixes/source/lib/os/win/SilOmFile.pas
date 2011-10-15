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
  Windows,

  SilLiStream,
  SilOiHandle,
  SilOiFile,
  SilOkFile;

type
	TSilWindowsFile = class (TSilFile)
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

implementation

uses
  SysUtils,

  SilOfFile,
  SilOmObjectHandle,
  SilOmFileInfo,
  SilOsError;

const
	FileAccessMode: array[TFileAccessMode] of LongWord = (
		GENERIC_READ,
		GENERIC_WRITE,
		GENERIC_READ or GENERIC_WRITE);

	FileShareMode: array[TFileShareMode] of LongWord = (
		0, {share none}
		FILE_SHARE_READ,
		FILE_SHARE_WRITE,
		FILE_SHARE_READ or FILE_SHARE_WRITE);

  OpenDisposition: array[Boolean] of LongWord = (
    OPEN_ALWAYS,
    OPEN_EXISTING );

  CreateDisposition: array[Boolean] of LongWord = (
    CREATE_NEW,
    CREATE_ALWAYS);

{ TSilWindowsFile }

function TSilWindowsFile.GetPosition: LongWord;
begin
  Result := Seek(0, soFromCurrent);
end;

procedure TSilWindowsFile.SetPosition(Pos: LongWord);
begin
  Seek(Pos, soFromBeginning);
end;

function TSilWindowsFile.GetSize: LongWord;
begin
  SilOfFile.GetFileSize(Self.Handle.Value, Result);
end;

procedure TSilWindowsFile.SetSize(NewSize: LongWord);
begin
  Seek(NewSize, soFromBeginning);
  OsError.Check(SetEndOfFile(Self.Handle.Value));
end;

function TSilWindowsFile.Read(var Buffer; Count: LongWord): LongWord; var BytesReaden: Windows.DWORD;
begin
  OsError.Check(Windows.ReadFile(Self.Handle.Value, Buffer, Count, BytesReaden, nil), 'TSilWindowsFile.Read [Windows.ReadFile]');
  Result := BytesReaden;
end;

function TSilWindowsFile.Write(const Buffer; Count: LongWord): LongWord; var BytesWritten: Windows.DWORD;
begin
  OsError.Check(Windows.WriteFile(Self.Handle.Value, Buffer, Count, BytesWritten, nil), 'TSilWindowsFile.Write [Windows.WriteFile]');
  Result := BytesWritten;
end;

function TSilWindowsFile.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
begin
  Result := SetFilePointer(Self.Handle.Value, Offset, nil, Ord(Origin));
end;

procedure TSilWindowsFile.Truncate;
begin
  OsError.Check(SetEndOfFile(Self.Handle.Value));
end;

function TSilWindowsFile.DoCreateHandle(const Value: THandle; const MustFree: Boolean): IHandle;
begin
  Result := TSilObjectHandle.Create(Value, MustFree);
end;

function TSilWindowsFile.DoGetInfo(const FileName: string): IFileInfoDef;
begin
  Result := TSilWindowsFileInfo.Create(Handle, FileName);
end;

function TSilWindowsFile.DoOpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): THandle;
begin
  Result := Windows.CreateFile(PChar(FileName), FileAccessMode[Access], FileShareMode[Share], nil, OpenDisposition[MustExists], FILE_ATTRIBUTE_NORMAL, 0);
  OsError.Check(Result <> INVALID_HANDLE_VALUE, 'FileSystem.Open [Windows.CreateFile] "%s"', [FileName]);
end;

function TSilWindowsFile.DoCreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): THandle;
begin
  Result := Windows.CreateFile(PChar(FileName), FileAccessMode[Access], FileShareMode[Share], nil, CreateDisposition[MustCreate], FILE_ATTRIBUTE_NORMAL, 0);
  OsError.Check(Result <> INVALID_HANDLE_VALUE, 'FileSystem.Create [Windows.CreateFile] "%s"', [FileName]);
end;

function TSilWindowsFile.FlushBuffer: Boolean;
begin
  Result := FlushFileBuffers(Self.Handle.Value);
end;

function TSilWindowsFile.Clone: IUnknown;
begin
  Result := TSilWindowsFile.OpenFile(FFileInfo.FullName, fmAccessReadWrite, fmShareReadWrite, true);
end;

end.
