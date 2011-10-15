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

unit SilSmLayerProtocolFile;

interface

{$include Defines.inc}

uses
  Sil,
  SilOkFile,
  SilOkFileInfo,
  SilOkDirectoryReader,

  SilOsTypes,
  SilSiLayer,
  SilSeLayerProtocol,
  SilSmLayerProtocolCustomImate,
  SilSiLayerProtocolFile,
  SilSeLayerProtocolFile,
  SilSfLayerProtocolFile;

type
  TSilClientSideFileProtocol = class (
    // extends
    TSilCustomImateProtocol,
    // implemets
    IClientSideFileProtocol)
  protected // IClientSideFileProtocol
    function OpenFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustExists: Boolean = False): IFile;
    function CreateFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone; MustCreate: Boolean = True): IFile;
    function CreateDirectory(const PathName: String; Force: Boolean = false): Boolean;
    function ReadDirectory(const PathName: String; const Include: TFileAttributes; const Exclude: TFileAttributes): IDirectoryReader;
    function GetInfo(const FileName: String): IFileInfo;
    function Move(const OldName, NewName: String): Boolean;
    function Delete(const FileName: String): Boolean;
  end;

  TSilServerSideFileProtocol = class (
    // extends
    TSilCustomImateProtocol,
    // implemets
    IServerSideFileProtocol)
  private
    FHook: IServerSideFileProtocolHook;
    FHandles: IInterfaceList;
    FRoot: String;
  private
    function DoAddToList(const List: IInterfaceList; const Item: IUnknown): Integer;
    procedure DoDelFromList(const List: IInterfaceList; Index: Integer);
    function DoCheckPath(const FilePath: String): String;
  private
    procedure FireCreateDirectory(var Msg: RImateProtocolMessage); message CM_CREATEDIR;
    procedure FireCreateFile(var Msg: RImateProtocolMessage); message CM_CREATEFILE;
    procedure FireOpenFile(var Msg: RImateProtocolMessage); message CM_OPENFILE;
    procedure FireDelete(var Msg: RImateProtocolMessage); message CM_DELETE;
    procedure FireGetInfo(var Msg: RImateProtocolMessage); message CM_GETINFO;
    procedure FireMove(var Msg: RImateProtocolMessage); message CM_MOVE;
    procedure FireReadDirectory(var Msg: RImateProtocolMessage); message CM_CREATEDIRREADER;
    procedure FireReadFile(var Msg: RImateProtocolMessage); message CM_READFILE;
    procedure FireWriteFile(var Msg: RImateProtocolMessage); message CM_WRITEFILE;
    procedure FireSeekFile(var Msg: RImateProtocolMessage); message CM_SEEKFILE;
    procedure FireFlushFile(var Msg: RImateProtocolMessage); message CM_FLUSHFILE;
    procedure FireCloseFile(var Msg: RImateProtocolMessage); message CM_CLOSEFILE;
    procedure FireDirectoryRead(var Msg: RImateProtocolMessage); message CM_DIRREAD;
    procedure FireDestroyDirReader(var Msg: RImateProtocolMessage); message CM_DESTROYDIRREADER;
    procedure FireGetFileSize(var Msg: RImateProtocolMessage); message CM_GETFILESIZE;
    procedure FireSetFileSize(var Msg: RImateProtocolMessage); message CM_SETFILESIZE;
    procedure FireSetFileAttr(var Msg: RImateProtocolMessage); message CM_SETFILEATTR;
    procedure FireSetFileTime(var Msg: RImateProtocolMessage); message CM_SETFILETIME;
  protected
    procedure Initialize(const Hook: IUnknown); override;
    procedure Finalize; override; 
  protected // IServerSideFileProtocol
  end;

  TSilRemoteFile = class (TSilFile)
  private
    FProt: ILayerProtocol;
    procedure DoClose; reintroduce;
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
  public
    constructor Create(const Prot: ILayerProtocol; AHandle: THandle; const FileName: String = '');
    destructor Destroy; override;
  end;

  TSilRemoteFileInfo = class (TSilFileInfo)
  private
    FProt: ILayerProtocol;
    FHasInfo: Boolean;
    FHandle: THandle;
    procedure DoCheckInfo;
  protected // IFileInfo
    function GetTime: TDateTime; override;
    function GetAttributes: TFileAttributes; override;
    function GetSize: LongWord; override;
    function GetVersion: IVersionInfo; override;
  protected // IFileInfoDef
    procedure SetTime(Time: TDateTime); override;
    procedure SetAttributes(Value: TFileAttributes); override;
    procedure SetSize(Value: LongWord); override;
  public
    constructor Create(const FileName: String = ''; const Time: TDateTime = 0; Attributes: TFileAttributes = []; Size: LongWord = 0); overload; override;
    constructor Create(const Prot: ILayerProtocol; const Handle: IHandle; const FileName: String); overload;
    destructor Destroy; override;
  end;

  TSilRemoteDirectoryReader = class (TSilBaseDirectoryReader)
  private
    FProt: ILayerProtocol;
    FHandle: Integer;
    procedure DoCreate(const Include, Exclude: TFileAttributes);
    procedure DoDestroy;
  protected // IDirectoryReader
    function Read: Boolean; override;
  public
    constructor Create(const Prot: ILayerProtocol; const PathName: String; const Include, Exclude: TFileAttributes); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SilLiPacket,
  SilLiFiler,
  SilOmObjectHandle, SilLtList;

{ TSilClientSideFileProtocol }

function TSilClientSideFileProtocol.CreateDirectory(const PathName: String; Force: Boolean): Boolean;
var
  Packet, Reply: IPacket;
begin
  Packet := Protocol.CreatePacket(CM_CREATEDIR);
  Packet.Writer.WriteString(PathName);
  Packet.Writer.WriteBoolean(Force);

  Protocol.SendPacket(Packet, Reply, CM_CREATEDIR_REPLY);
  Result := Reply.Reader.ReadBoolean;
end;

function TSilClientSideFileProtocol.CreateFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): IFile;
var
  Packet, Reply: IPacket;
  Handle: Integer;
begin
  Packet := Protocol.CreatePacket(CM_CREATEFILE);
  Packet.Writer.WriteString(FileName);
  Packet.Writer.Write(Access, SizeOf(Access));
  Packet.Writer.Write(Share, SizeOf(Share));
  Packet.Writer.WriteBoolean(MustCreate);

  Protocol.SendPacket(Packet, Reply, CM_CREATEFILE_REPLY);
  Handle := Reply.Reader.ReadInteger;
  Result := TSilRemoteFile.Create(Protocol, Handle, FileName);
end;

function TSilClientSideFileProtocol.Delete(const FileName: String): Boolean;
var
  Packet, Reply: IPacket;
begin
  Packet := Protocol.CreatePacket(CM_DELETE);
  Packet.Writer.WriteString(FileName);

  Protocol.SendPacket(Packet, Reply, CM_DELETE_REPLY);
  Result := Reply.Reader.ReadBoolean;
end;

function TSilClientSideFileProtocol.GetInfo(const FileName: String): IFileInfo;
var
  Packet, Reply: IPacket;
begin
  Packet := Protocol.CreatePacket(CM_GETINFO);
  Packet.Writer.WriteString(FileName);

  Protocol.SendPacket(Packet, Reply, CM_GETINFO_REPLY);
  DoReadFileInfo(Reply, Result);
end;

function TSilClientSideFileProtocol.Move(const OldName, NewName: String): Boolean;
var
  Packet, Reply: IPacket;
begin
  Packet := Protocol.CreatePacket(CM_MOVE);
  Packet.Writer.WriteString(OldName);
  Packet.Writer.WriteString(NewName);

  Protocol.SendPacket(Packet, Reply, CM_MOVE_REPLY);
  Result := Reply.Reader.ReadBoolean;
end;

function TSilClientSideFileProtocol.OpenFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): IFile;
var
  Packet, Reply: IPacket;
  Handle: Integer;
begin
  Packet := Protocol.CreatePacket(CM_OPENFILE);
  Packet.Writer.WriteString(FileName);
  Packet.Writer.Write(Access, SizeOf(Access));
  Packet.Writer.Write(Share, SizeOf(Share));
  Packet.Writer.WriteBoolean(MustExists);

  Protocol.SendPacket(Packet, Reply, CM_OPENFILE_REPLY);
  Handle := Reply.Reader.ReadInteger;
  Result := TSilRemoteFile.Create(Protocol, Handle, FileName);
end;

function TSilClientSideFileProtocol.ReadDirectory(const PathName: String; const Include: TFileAttributes; const Exclude: TFileAttributes): IDirectoryReader;
begin
  Result := TSilRemoteDirectoryReader.Create(Protocol, PathName, Include, Exclude);
end;

{ TSilRemoteFile }

constructor TSilRemoteFile.Create(const Prot: ILayerProtocol; AHandle: THandle; const FileName: String);
begin
  inherited Create(AHandle, FileName, false);
  FProt := Prot;
end;

destructor TSilRemoteFile.Destroy;
begin
  DoClose;
  FProt := nil;

  inherited;
end;

function TSilRemoteFile.GetPosition: LongWord;
begin
  Result := Seek(0, soFromCurrent);
end;

procedure TSilRemoteFile.SetPosition(Pos: LongWord);
begin
  Seek(Pos, soFromBeginning);
end;

function TSilRemoteFile.GetSize: LongWord;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_GETFILESIZE);
  Packet.Writer.WriteInteger(Handle.Value);
  FProt.SendPacket(Packet, Reply, CM_GETFILESIZE_REPLY);

  Result := Reply.Reader.ReadLongWord;
end;

procedure TSilRemoteFile.SetSize(NewSize: LongWord);
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_SETFILESIZE);
  Packet.Writer.WriteInteger(Handle.Value);
  Packet.Writer.WriteLongWord(NewSize);
  FProt.SendPacket(Packet, Reply, CM_SETFILESIZE_REPLY);
end;

function TSilRemoteFile.Read(var Buffer; Count: LongWord): LongWord;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_READFILE);
  Packet.Writer.WriteInteger(Handle.Value);
  Packet.Writer.WriteLongWord(Count);

  FProt.SendPacket(Packet, Reply, CM_READFILE_REPLY);
  Result := Reply.Reader.ReadLongWord;
  Reply.Reader.Read(Buffer, Result);
end;

function TSilRemoteFile.Write(const Buffer; Count: LongWord): LongWord;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_WRITEFILE);
  Packet.Writer.WriteInteger(Handle.Value);
  Packet.Writer.WriteLongWord(Count);
  Packet.Writer.Write(Buffer, Count);

  FProt.SendPacket(Packet, Reply, CM_WRITEFILE_REPLY);
  Result := Reply.Reader.ReadLongWord;
end;

function TSilRemoteFile.Seek(Offset: Integer; Origin: TSeekOrigin): LongWord;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_SEEKFILE);
  Packet.Writer.WriteInteger(Handle.Value);
  Packet.Writer.WriteInteger(Offset);
  Packet.Writer.Write(Origin, SizeOf(Origin));

  FProt.SendPacket(Packet, Reply, CM_SEEKFILE_REPLY);
  Result := Reply.Reader.ReadLongWord;
end;

procedure TSilRemoteFile.Truncate;
begin
  SetSize(GetPosition);
end;

function TSilRemoteFile.DoCreateHandle(const Value: THandle; const MustFree: Boolean): IHandle;
begin
  Result := TSilObjectHandle.Create(Value, MustFree);
end;

function TSilRemoteFile.DoGetInfo(const FileName: string): IFileInfoDef;
begin
  Result := TSilRemoteFileInfo.Create(FProt, Handle, FileName);
end;

function TSilRemoteFile.DoOpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): THandle;
begin
  Result := 0;
end;

function TSilRemoteFile.DoCreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): THandle;
begin
  Result := 0;
end;

function TSilRemoteFile.FlushBuffer: Boolean;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_FLUSHFILE);
  Packet.Writer.WriteInteger(Handle.Value);

  FProt.SendPacket(Packet, Reply, CM_FLUSHFILE_REPLY);
  Result := Reply.Reader.ReadBoolean;
end;

function TSilRemoteFile.Clone: IUnknown;
begin
  Result := nil;
end;

procedure TSilRemoteFile.DoClose;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_CLOSEFILE);
  Packet.Writer.WriteInteger(Handle.Value);
  FProt.SendPacket(Packet, Reply, CM_CLOSEFILE_REPLY);
end;

{ TSilRemoteFileInfo }

constructor TSilRemoteFileInfo.Create(const FileName: String; const Time: TDateTime; Attributes: TFileAttributes; Size: LongWord);
begin
  inherited;

  FTime := Time;
  FAttributes := Attributes;
  FSize := Size;
  FHasInfo := true;
end;

constructor TSilRemoteFileInfo.Create(const Prot: ILayerProtocol; const Handle: IHandle; const FileName: String);
begin
  inherited Create(Handle, FileName);
  FHandle := Handle.Value;
  FProt := Prot;
end;

destructor TSilRemoteFileInfo.Destroy;
begin
  inherited;
end;

procedure TSilRemoteFileInfo.DoCheckInfo;
var
  Packet, Reply: IPacket;
begin
  if FHasInfo then Exit;

  Packet := FProt.CreatePacket(CM_GETINFO);
  Packet.Writer.WriteString(GetFullName);

  FProt.SendPacket(Packet, Reply, CM_GETINFO_REPLY);
  FTime := Reply.Reader.ReadDate;
  Reply.Reader.Read(FAttributes, SizeOf(TFileAttributes));
  FSize := Reply.Reader.ReadInteger;
  FHasInfo := true;
end;

function TSilRemoteFileInfo.GetAttributes: TFileAttributes;
begin
  DoCheckInfo;
  Result := FAttributes;
end;

function TSilRemoteFileInfo.GetSize: LongWord;
begin
  DoCheckInfo;
  Result := FSize;
end;

function TSilRemoteFileInfo.GetTime: TDateTime;
begin
  DoCheckInfo;
  Result := FTime;
end;

function TSilRemoteFileInfo.GetVersion: IVersionInfo;
begin
  // raise
  Result := nil;
end;

procedure TSilRemoteFileInfo.SetAttributes(Value: TFileAttributes);
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_SETFILEATTR);
  Packet.Writer.WriteLongWord(FHandle);
  Packet.Writer.Write(FAttributes, SizeOf(TFileAttributes));

  FProt.SendPacket(Packet, Reply, CM_SETFILEATTR_REPLY);
  Reply.Reader.Read(FAttributes, SizeOf(TFileAttributes));
end;

procedure TSilRemoteFileInfo.SetSize(Value: LongWord);
begin
end;

procedure TSilRemoteFileInfo.SetTime(Time: TDateTime);
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_SETFILETIME);
  Packet.Writer.WriteLongWord(FHandle);
  Packet.Writer.WriteDate(FTime);

  FProt.SendPacket(Packet, Reply, CM_SETFILETIME_REPLY);
  FTime := Reply.Reader.ReadDate;
end;

{ TSilRemoteDirectoryReader }

constructor TSilRemoteDirectoryReader.Create(const Prot: ILayerProtocol; const PathName: String; const Include, Exclude: TFileAttributes);
begin
  inherited Create(PathName, Include, Exclude, '/');

  FProt := Prot;
  DoCreate(Include, Exclude);
end;

destructor TSilRemoteDirectoryReader.Destroy;
begin
  DoDestroy;
  FProt := nil;

  inherited;
end;

procedure TSilRemoteDirectoryReader.DoCreate(const Include, Exclude: TFileAttributes); 
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_CREATEDIRREADER);
  Packet.Writer.WriteString(GetPathName);
  Packet.Writer.Write(Include, SizeOf(TFileAttributes));
  Packet.Writer.Write(Exclude, SizeOf(TFileAttributes));

  FProt.SendPacket(Packet, Reply, CM_CREATEDIRREADER_REPLY);
  FHandle := Reply.Reader.ReadInteger;
end;

function TSilRemoteDirectoryReader.Read: Boolean;
var
  Packet, Reply: IPacket;
  Enum: IEnumerator;
  Item: IFileInfo;
begin
  Packet := FProt.CreatePacket(CM_DIRREAD);
  Packet.Writer.WriteInteger(FHandle);
  Packet.Writer.WriteLongWord(FBufferSize);

  FProt.SendPacket(Packet, Reply, CM_DIRREAD_REPLY);
  Result := Reply.Reader.ReadBoolean;
  FIsComplete := not Result;

  if Result and DoReadFileInfoList(Reply, FRecent) then
    while FRecent.Enumerate(Enum, Item) do
      FList.Add(Item);
end;

procedure TSilRemoteDirectoryReader.DoDestroy;
var
  Packet, Reply: IPacket;
begin
  Packet := FProt.CreatePacket(CM_DESTROYDIRREADER);
  Packet.Writer.WriteInteger(FHandle);
  FProt.SendPacket(Packet, Reply, CM_DESTROYDIRREADER_REPLY);
end;

{ TSilServerSideFileProtocol }

procedure TSilServerSideFileProtocol.Initialize(const Hook: IInterface);
var
  Root: Variant;
begin
  FHandles := Sil.List.InterfaceList(true);
  
  if (Parameters = nil) or not Parameters.Find('root', Root) then
    Root := '';

  if Str.NotEmpty(Root) then
    FRoot := Sil.OS.FileSystem.AddSlash(Root);

  Ref.GetInterface(Hook, IServerSideFileProtocolHook, FHook);
end;

procedure TSilServerSideFileProtocol.Finalize;
begin
  FHook := nil;
  FHandles := nil;
end;

function TSilServerSideFileProtocol.DoAddToList(const List: IInterfaceList; const Item: IInterface): Integer;
begin
  if Assigned(Item) then
  begin
    List.Locked;
    Result := List.IndexOf(nil);

    if Result >= 0 then
      List[Result] := Item else
      Result := List.Add(Item);
  end else
    Result := -1;
end;

procedure TSilServerSideFileProtocol.DoDelFromList(const List: IInterfaceList; Index: Integer);
begin
  List.Locked;
  List[Index] := nil;

  while (List.Count > 0) and not Assigned(List.Last) do
    List.Delete(List.Count - 1);
end;

function TSilServerSideFileProtocol.DoCheckPath(const FilePath: String): String;
begin
  if Str.IsEmpty(FRoot) then
    Result := FilePath else
    Result := Sil.OS.FSys.DeleteSlash(FRoot) + Sil.OS.FSys.TranslatePath(Str.IIf(Str.ToChr(FilePath) <> '/', '/') + FilePath);
              
  Sil.Str.DelControlChars(Result);
end;

procedure TSilServerSideFileProtocol.FireCloseFile(var Msg: RImateProtocolMessage);
var
  Event: RFCloseFileEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;

  FHook.OnCloseFile(Event);

  DoDelFromList(FHandles, Index);
  Reply := Protocol.CreatePacket(CM_CLOSEFILE_REPLY);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireCreateDirectory(var Msg: RImateProtocolMessage);
var
  Event: RFPCreateDirectoryEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.PathName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Event.Result := false;

  FHook.OnCreateDirectory(Event);

  Reply := Protocol.CreatePacket(CM_CREATEDIR_REPLY);
  Reply.Writer.WriteBoolean(Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireCreateFile(var Msg: RImateProtocolMessage);
var
  Event: RFPCreateFileEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.FileName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Msg.Packet.Reader.Read(Event.Access, SizeOf(Event.Access));
  Msg.Packet.Reader.Read(Event.Share, SizeOf(Event.Share));
  Event.Result := nil;

  FHook.OnCreateFile(Event);

  Reply := Protocol.CreatePacket(CM_CREATEFILE_REPLY);
  Reply.Writer.WriteInteger(DoAddToList(FHandles, Event.Result));
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireDelete(var Msg: RImateProtocolMessage);
var
  Event: RFPDeleteEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.FileName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Event.Result := false;

  FHook.OnDelete(Event);

  Reply := Protocol.CreatePacket(CM_DELETE_REPLY);
  Reply.Writer.WriteBoolean(Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireDestroyDirReader(var Msg: RImateProtocolMessage);
var
  Event: RFDirectoryReaderEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Reader := FHandles[Index] as IDirectoryReader;

  FHook.OnDestroyDirectoryReader(Event);

  DoDelFromList(FHandles, Index);
  Reply := Protocol.CreatePacket(CM_DESTROYDIRREADER_REPLY);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireDirectoryRead(var Msg: RImateProtocolMessage);
var
  Event: RFDirectoryReaderEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Reader := FHandles[Index] as IDirectoryReader;
  Event.BufferSize := Msg.Packet.Reader.ReadLongWord;

  FHook.OnDirectoryRead(Event);

  Reply := Protocol.CreatePacket(CM_DIRREAD_REPLY);
  Reply.Writer.WriteBoolean(Event.Result);

  if Assigned(Event.Reader.Recent) and (Event.Reader.Recent.Count > 0) then
    DoWriteFileInfoList(Reply, Event.Reader.Recent, Length(FRoot)) else
    DoWriteFileInfoList(Reply, Event.Reader.List, Length(FRoot));

  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireFlushFile(var Msg: RImateProtocolMessage);
var
  Event: RFFlushFileEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Result := false;

  FHook.OnFlushFile(Event);

  Reply := Protocol.CreatePacket(CM_FLUSHFILE_REPLY);
  Reply.Writer.WriteBoolean(Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireGetFileSize(var Msg: RImateProtocolMessage);
var
  Event: RFPFileSizeEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Size := 0;

  FHook.OnGetFileSize(Event);

  Reply := Protocol.CreatePacket(CM_GETFILESIZE_REPLY);
  Reply.Writer.WriteLongWord(Event.Size);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireGetInfo(var Msg: RImateProtocolMessage);
var
  Event: RFPInfoEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.FileName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Event.Info := nil;

  FHook.OnGetInfo(Event);

  Reply := Protocol.CreatePacket(CM_GETINFO_REPLY);
  DoWriteFileInfo(Reply, Event.Info, Length(FRoot));
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireMove(var Msg: RImateProtocolMessage);
var
  Event: RFPMoveEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.OldName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Event.NewName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Event.Result := false;

  FHook.OnMove(Event);

  Reply := Protocol.CreatePacket(CM_MOVE_REPLY);
  Reply.Writer.WriteBoolean(Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireOpenFile(var Msg: RImateProtocolMessage);
var
  Event: RFPOpenFileEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.FileName := DoCheckPath(Msg.Packet.Reader.ReadString);
  Msg.Packet.Reader.Read(Event.Access, SizeOf(Event.Access));
  Msg.Packet.Reader.Read(Event.Share, SizeOf(Event.Share));
  Event.Result := nil;

  FHook.OnOpenFile(Event);

  Reply := Protocol.CreatePacket(CM_OPENFILE_REPLY);
  Reply.Writer.WriteInteger(DoAddToList(FHandles, Event.Result));
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireReadDirectory(var Msg: RImateProtocolMessage);
var
  Event: RFCreateDirectoryReaderEvent;
  Reply: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Path := DoCheckPath(Msg.Packet.Reader.ReadString);
  Msg.Packet.Reader.Read(Event.Include, SizeOf(TFileAttributes));
  Msg.Packet.Reader.Read(Event.Exclude, SizeOf(TFileAttributes));

  Event.Result := nil;

  FHook.OnCreateDirectoryReader(Event);

  Reply := Protocol.CreatePacket(CM_CREATEDIRREADER_REPLY);
  Reply.Writer.WriteInteger(DoAddToList(FHandles, Event.Result));
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireReadFile(var Msg: RImateProtocolMessage);
var
  Event: RFFileDataEvent;
  Reply: IPacket;
  Index: Integer;
  Buffer: String;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  SetLength(Buffer, Msg.Packet.Reader.ReadLongWord);
  Event.Count := Length(Buffer);
  Event.Buffer := PChar(Buffer);

  FHook.OnReadFile(Event);

  Reply := Protocol.CreatePacket(CM_READFILE_REPLY);
  Reply.Writer.WriteLongWord(Event.Result);
  Reply.Writer.Write(Event.Buffer^, Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireWriteFile(var Msg: RImateProtocolMessage);
var
  Event: RFFileDataEvent;
  Reply: IPacket;
  Buffer: String;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Count := Msg.Packet.Reader.ReadLongWord;

  SetLength(Buffer, Event.Count);
  Event.Buffer := PChar(Buffer);

  Msg.Packet.Reader.Read(Buffer[1], Event.Count);

  FHook.OnWriteFile(Event);

  Reply := Protocol.CreatePacket(CM_WRITEFILE_REPLY);
  Reply.Writer.WriteLongWord(Event.Result);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireSeekFile(var Msg: RImateProtocolMessage);
var
  Event: RFSeekFileEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Offset := Msg.Packet.Reader.ReadInteger;
  Msg.Packet.Reader.Read(Event.Origin, SizeOf(Event.Origin));

  FHook.OnSeekFile(Event);

  Reply := Protocol.CreatePacket(CM_SEEKFILE_REPLY);
  Reply.Writer.WriteLongWord(Event.Source.Stream.Position);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireSetFileAttr(var Msg: RImateProtocolMessage);
var
  Event: RFPFileAttributeEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Msg.Packet.Reader.Read(Event.Attributes, SizeOf(Event.Attributes));

  FHook.OnSetFileAttributes(Event);

  Reply := Protocol.CreatePacket(CM_SETFILEATTR_REPLY);
  Reply.Writer.Write(Event.Attributes, SizeOf(Event.Attributes));
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireSetFileSize(var Msg: RImateProtocolMessage);
var
  Event: RFPFileSizeEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Size := Msg.Packet.Reader.ReadInteger;

  FHook.OnSetFileSize(Event);

  Reply := Protocol.CreatePacket(CM_SETFILESIZE_REPLY);
  Protocol.SendPacket(Reply);
end;

procedure TSilServerSideFileProtocol.FireSetFileTime(var Msg: RImateProtocolMessage);
var
  Event: RFPFileTimeEvent;
  Reply: IPacket;
  Index: Integer;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Index := Msg.Packet.Reader.ReadInteger;
  Event.Source := FHandles[Index] as IFile;
  Event.Time := Msg.Packet.Reader.ReadDate;

  FHook.OnSetFileTime(Event);

  Reply := Protocol.CreatePacket(CM_SETFILETIME_REPLY);
  Reply.Writer.WriteDate(Event.Time);
  Protocol.SendPacket(Reply);
end;

end.
