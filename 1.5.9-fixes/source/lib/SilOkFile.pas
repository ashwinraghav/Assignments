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

unit SilOkFile;

{$I Defines.inc}

interface

uses
  SysUtils,

//  SilLkInterfaced,

  SilLiStream,
  SilLiClone,
  SilLiCompare,
  
  SilOsTypes,
  
  SilOiFile,
  SilOiHandle,
  SilOiVersion,

  SilOkHandled;

type
  TSilFile = class (
    // extends
    TSilHandledObject,
    // implements
    IFile,
    IStream,
    IRandomStream,
    ICloneable)
  protected
    FFileInfo: IFileInfoDef;
  protected
    function DoGetInfo(const FileName: string): IFileInfoDef; virtual; abstract;
    function DoOpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean): THandle; virtual; abstract;
    function DoCreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean): THandle; virtual; abstract;
  protected // IFile
    function GetStream: IRandomStream;
    function GetInfo: IFileInfoDef;
    function FlushBuffer: Boolean; virtual; abstract;
  protected // IRandomStream
    function GetPosition: LongWord; virtual; abstract;
    procedure SetPosition(Pos: LongWord); virtual; abstract;
    function GetSize: LongWord; virtual; abstract;
    procedure SetSize(NewSize: LongWord); virtual; abstract;
    function Read(var Buffer; Count: LongWord): LongWord; virtual; abstract;
    function Write(const Buffer; Count: LongWord): LongWord; virtual; abstract;
    function Seek(Offset: Integer; Origin: TSeekOrigin): LongWord; virtual; abstract;
    procedure Truncate; virtual; abstract;
  protected // ICloneable
    function Clone: IUnknown; virtual; abstract;
  public
    constructor Create(AHandle: THandle; const FileName: String = ''; MustFree: Boolean = true); reintroduce;
    constructor OpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean);
    constructor CreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  SilAfAtoB,
  SilBtStr,
  SilOsClasses,
  SilOtError,
  SilOtHandle,
  SilOtFile;

{ TSilFile }

constructor TSilFile.Create(AHandle: THandle; const FileName: String; MustFree: Boolean);
begin
  inherited Create(AHandle, MustFree);
  FFileInfo := DoGetInfo(FileName);
end;

destructor TSilFile.Destroy;
begin
  FFileInfo := nil;
  inherited;
end;

function TSilFile.GetStream: IRandomStream;
begin
  Result := Self;
end;

function TSilFile.GetInfo: IFileInfoDef;
begin
  Result := FFileInfo;
end;

constructor TSilFile.OpenFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustExists: Boolean);
var
  Handle: THandle;
begin
  Handle := DoOpenFile(FileName, Access, Share, MustExists);
  Create(Handle, FileName);
end;

constructor TSilFile.CreateFile(const FileName: string; Access: TFileAccessMode; Share: TFileShareMode; MustCreate: Boolean);
var
  Handle: THandle;
begin
  Handle := DoCreateFile(FileName, Access, Share, MustCreate);
  Create(Handle, FileName);
end;

end.
