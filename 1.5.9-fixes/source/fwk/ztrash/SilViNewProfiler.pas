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

unit SilViNewProfiler;

interface

uses
  Sil,
	Classes,
	Controls,
  SilViProfileDocument;

type

{ IProfileReader }

  IProfileReader = interface
    ['{BD1465B2-6D43-11D4-AD98-00902794F778}']
    function GetStream: IStream;
    property Stream: IStream read GetStream;
  end;

{ IProfileLoader }

  IProfileLoader = interface
    ['{BD1465B1-6D43-11D4-AD98-00902794F778}']
    function LoadDocument(const Stream: IStream; const Owner: string = ''): IProfileDocument;
    procedure StoreDocument(const Stream: IStream; const Document: IProfileDocument; const Owner: string  = '');
  end;

{ IProfileSource }

  IProfileSource = interface
		['{5973C061-E606-11D3-986E-00104B0FA1EF}']
    function GetReader: IProfileReader;
    procedure SetReader(const Value: IProfileReader);
    function GetLoader: IProfileLoader;
    function ReadDocument(const Owner: string = ''): IProfileDocument;
    procedure WriteDocument(const Document: IProfileDocument; const Owner: string  = '');
    property Reader: IProfileReader read GetReader write SetReader;
    property Loader: IProfileLoader read GetLoader;
  end;

{ IProfiler }

	IProfiler = interface
		['{5973C062-E606-11D3-986E-00104B0FA1EF}']
		function GetSource: IProfileSource;
		procedure SetSource(const Value: IProfileSource);
		function Update(const Owner: String = ''): Boolean; overload;
		function Update(const Document: IProfileDocument; const Owner: String = ''): Boolean; overload;
		function Value(const Name: String): String;
		function OrdValue(Obj: TPersistent; const PropName, ValueName: String): Boolean;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String); overload;
		procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String; Parent: TWinControl); overload;
		procedure CreateForm(InstanceClass: TComponentClass); overload;
		property Source: IProfileSource read GetSource write SetSource;
	end;

implementation

end.
 