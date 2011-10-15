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

unit SilOiFile; // doc: SilOiFile.dtx

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilLiList,

  SilOiVersion,
  SilOiHandle;

type
  TFileAccessMode = (fmAccessRead, fmAccessWrite, fmAccessReadWrite);
  TFileShareMode = (fmShareNone, fmShareRead, fmShareWrite, fmShareReadWrite);

  TFileAttribute = (faArchive, faDirectory, faHidden, faReadOnly, faSysFile);
  TFileAttributes = set of TFileAttribute;

  TFileInfoKind = (ikName, ikDate, ikSize, ikPath, ikAttributes);

  IFileInfo = interface
    ['{3AA13832-2A0D-11D4-917D-00C0261013CD}']
    function GetPath: String;
    function GetName: String;
    function GetFullName: String;
    function GetTime: TDateTime;
    function GetAttributes: TFileAttributes;
    function GetSize: LongWord;
    function GetVersion: IVersionInfo;
    property Path: String read GetPath;
    property FullName: String read GetFullName;
    property Name: String read GetName;
    property Time: TDateTime read GetTime;
    property Attributes: TFileAttributes read GetAttributes;
    property Size: LongWord read GetSize;
    property Version: IVersionInfo read GetVersion;
  end;
              
  IFileInfoDef = interface (IFileInfo)
    ['{56E248B5-E87A-46C2-A16A-91397CBAFA52}']
    procedure SetPath(const Value: String);
    procedure SetName(const Value: String);
    procedure SetTime(Time: TDateTime);
    procedure SetAttributes(Value: TFileAttributes);
    procedure SetSize(Value: LongWord);
    property Path: String read GetPath write SetPath;
    property Name: String read GetName write SetName;
    property Time: TDateTime read GetTime write SetTime;
    property Attributes: TFileAttributes read GetAttributes write SetAttributes;
    property Size: LongWord read GetSize write SetSize;
  end;

  IFileInfoList = interface (IList)
    ['{3AA13831-2A0D-11D4-917D-00C0261013CD}']
    function Add(const Item: IFileInfo): Integer;
    procedure AddList(const Source: IFileInfoList);
    function IndexOf(const Item: IFileInfo): Integer;
    procedure Insert(Index: Integer; const Item: IFileInfo);
    function Remove(const Item: IFileInfo): Integer;
    function First: IFileInfo;
    function Last: IFileInfo;
    function GetItem(Index: Integer): IFileInfo;
    procedure SetItem(Index: Integer; const Value: IFileInfo);
    property Items[Index: Integer]: IFileInfo read GetItem write SetItem; default;
  end;

type
  IFile = interface (IHandledObject)
    ['{8FE2B643-0B31-11D4-987A-00104B0FA1EF}']
    function GetStream: IRandomStream;
    function GetInfo: IFileInfoDef;
    function FlushBuffer: Boolean;
    property Stream: IRandomStream read GetStream;
    property Info: IFileInfoDef read GetInfo;
  end;

  IFileList = interface (IList)
    ['{80B93885-6F96-11D4-9894-00104B0FA1EF}']
    function Add(const Item: IFile): Integer;
    procedure AddList(const Source: IFileList);
    function IndexOf(const Item: IFile): Integer;
    procedure Insert(Index: Integer; const Item: IFile);
    function Remove(const Item: IFile): Integer;
    function First: IFile;
    function Last: IFile;
    function GetItem(Index: Integer): IFile;
    procedure SetItem(Index: Integer; const Value: IFile);
    property Items[Index: Integer]: IFile read GetItem write SetItem; default;
  end;

type
  IDirectoryReader = interface
    ['{05CEC7D5-1D8C-45AC-AB6A-94B1B3BD05F8}']
    function GetPathName: String;
    function GetList: IFileInfoList;
    function GetRecent: IFileInfoList;
    function GetIsComplete: Boolean;
    function GetBufferSize: LongWord;
    function GetPath: String;
    function GetMask: String;
    procedure SetBufferSize(Value: LongWord);
    function Read: Boolean;
    function ReadAll: Boolean;
    property PathName: String read GetPathName;
    property Recent: IFileInfoList read GetRecent;
    property List: IFileInfoList read GetList;
    property IsComplete: Boolean read GetIsComplete;
    property BufferSize: LongWord read GetBufferSize write SetBufferSize;
    property Path: String read GetPath;
    property Mask: String read GetMask;
  end;

implementation

end.
