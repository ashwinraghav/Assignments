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

unit SilSiLayerProtocolFile;

{$include Defines.inc}

interface

uses
  Sil;

type
  IClientSideFileProtocol = interface;
  IServerSideFileProtocol = interface;
  IServerSideFileProtocolHook = interface;

  IClientSideFileProtocol = interface
    ['{3979CD77-82F9-4B00-9FCF-B6DF408009F0}']
    function OpenFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustExists: Boolean = False): IFile;
    function CreateFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustCreate: Boolean = True): IFile;
    function CreateDirectory(const PathName: String; Force: Boolean = false): Boolean;
    function ReadDirectory(const PathName: String; const Include: TFileAttributes = []; const Exclude: TFileAttributes = []): IDirectoryReader;
    function GetInfo(const FileName: String): IFileInfo;
    function Move(const OldName, NewName: String): Boolean;
    function Delete(const FileName: String): Boolean;
  end;

  IServerSideFileProtocol = interface
    ['{4BF1B87F-665B-4E73-85D7-932D7ADB8B63}']
  end;

  RFPOpenFileEvent = record
    Sender: IServerSideFileProtocol;
    FileName: string;
    Access: TFileAccessMode;
    Share: TFileShareMode;
    MustExists: Boolean;
    Result: IFile;
  end;

  RFPCreateFileEvent = record
    Sender: IServerSideFileProtocol;
    FileName: string;
    Access: TFileAccessMode;
    Share: TFileShareMode;
    MustCreate: Boolean;
    Result: IFile;
  end;

  RFPCreateDirectoryEvent = record
    Sender: IServerSideFileProtocol;
    PathName: String;
    Result: Boolean;
  end;

  RFPInfoEvent = record
    Sender: IServerSideFileProtocol;
    FileName: String;
    Info: IFileInfo;
  end;

  RFPMoveEvent = record
    Sender: IServerSideFileProtocol;
    OldName: String;
    NewName: String;
    Result: Boolean;
  end;

  RFPDeleteEvent = record
    Sender: IServerSideFileProtocol;
    FileName: String;
    Result: Boolean;
  end;

  RFFileDataEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Buffer: Pointer;
    Count: LongWord;
    Result: LongWord;
  end;

  RFSeekFileEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Offset: Integer;
    Origin: TSeekOrigin;
  end;

  RFFlushFileEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Result: Boolean;
  end;

  RFCloseFileEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
  end;

  RFCreateDirectoryReaderEvent = record
    Sender: IServerSideFileProtocol;
    Path: String;
    Include, Exclude: TFileAttributes;
    Result: IDirectoryReader;
  end;

  RFDirectoryReaderEvent = record
    Sender: IServerSideFileProtocol;
    Reader: IDirectoryReader;
    BufferSize: LongWord;
    Result: Boolean;
  end;

  RFPFileSizeEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Size: LongWord;
  end;

  RFPFileAttributeEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Attributes: TFileAttributes;
  end;

  RFPFileTimeEvent = record
    Sender: IServerSideFileProtocol;
    Source: IFile;
    Time: TDateTime;
  end;

  IServerSideFileProtocolHook = interface
    ['{1A1A00A3-37A3-410E-8D54-84C85B0F4ABF}']
    procedure OnOpenFile(var Event: RFPOpenFileEvent);
    procedure OnCreateFile(var Event: RFPCreateFileEvent);
    procedure OnReadFile(var Event: RFFileDataEvent);
    procedure OnWriteFile(var Event: RFFileDataEvent);
    procedure OnSeekFile(var Event: RFSeekFileEvent);
    procedure OnFlushFile(var Event: RFFlushFileEvent);
    procedure OnCloseFile(var Event: RFCloseFileEvent);
    procedure OnGetFileSize(var Event: RFPFileSizeEvent);
    procedure OnCreateDirectory(var Event: RFPCreateDirectoryEvent);
    procedure OnCreateDirectoryReader(var Event: RFCreateDirectoryReaderEvent);
    procedure OnDirectoryRead(var Event: RFDirectoryReaderEvent);
    procedure OnDestroyDirectoryReader(var Event: RFDirectoryReaderEvent);
    procedure OnMove(var Event: RFPMoveEvent);
    procedure OnDelete(var Event: RFPDeleteEvent);
    procedure OnGetInfo(var Event: RFPInfoEvent);
    procedure OnSetFileSize(var Event: RFPFileSizeEvent);
    procedure OnSetFileAttributes(var Event: RFPFileAttributeEvent);
    procedure OnSetFileTime(var Event: RFPFileTimeEvent);
  end;

implementation

end.
 