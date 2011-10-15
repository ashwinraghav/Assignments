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

unit SilOiVersion;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiFiler,
  SilOcVersion,
  SilOdVersion;

const
  CShortVersion = SilOcVersion.CShortVersion;
  CLongVersion  = SilOcVersion.CLongVersion;
  CRelease      = SilOcVersion.CRelease;       

type
  TVersionItem = (viMajor, viMinor, viRevision, viBuild);
  TVersionItems = set of TVersionItem;

  IVersionNumber = interface
    ['{584553D2-6EC9-11D4-AD98-00902794F778}']
    function GetValue: LargeInt;
    function GetMajor: Word;
    function GetMinor: Word;
    function GetRevision: Word;
    function GetBuild: Word;
    function GetItem(Index: TVersionItem): Word;
    function ToStr(const Formats: string = CShortVersion; const Default: string = ''): string;
  //--------------------------------------------
    property Value: LargeInt read GetValue;
    property Major: Word read GetMajor;
    property Minor: Word read GetMinor;
    property Revision: Word read GetRevision;
    property Build: Word read GetBuild;
    property Item[AIndex: TVersionItem]: Word read GetItem; default;
  end;

  IStandardTags = interface;

  IVersionTags = interface
    ['{584553D3-6EC9-11D4-AD98-00902794F778}']
    function GetTag(const Tag: string): IValueReader;
    function GetStd: IStandardTags;
  //--------------------------------------------
    property Tag[const Tag: string]: IValueReader read GetTag; default;
    property Std: IStandardTags read GetStd;
  end;

  IStandardTags = interface
    ['{584553D4-6EC9-11D4-AD98-00902794F778}']
    function GetProductName: string;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileVersion: IVersionNumber;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetOriginalFilename: string;
    function GetProductVersion: IVersionNumber;
  //--------------------------------------------
    property ProductName: string read GetProductName;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVersion: IVersionNumber read GetFileVersion;
    property InternalName: string read GetInternalName;
    property LegalCopyright: string read GetLegalCopyright;
    property OriginalFilename: string read GetOriginalFilename;
    property ProductVersion: IVersionNumber read GetProductVersion;
  end;

  IVersionInfo = interface
    ['{584553D1-6EC9-11D4-AD98-00902794F778}']
    function GetNumber: IVersionNumber;
    function GetFullName: string;
    function Tags(LocaleID: LongWord = 0): IVersionTags;
  //--------------------------------------------
    property Number: IVersionNumber read GetNumber;
    property FullName: string read GetFullName;
  end;

implementation
end.
