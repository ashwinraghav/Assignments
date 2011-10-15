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

unit SilOkFilesystemNotifier;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilOsWaitable,

  SilOeWait,
  SilOeFilesystemNotifier,

  SilOiHandle,
  SilOiWait,
  SilOiFilesystemNotifier;

type
  TSilFilesystemNotifier = class(
  //- extends
    TSilOsWaitableObject,
  //- implements
    IFilesystemChangeNotification )
  protected // IFilesystemChangeNotification
   procedure IFilesystemChangeNotification.Reset = DoReset;
  protected
    function DoOpen(const PathName: String; WatchSubtree: Boolean; NotifyFilter: TFilesystemChangeFilters): IHandle; reintroduce; virtual; abstract;
    procedure DoClose; reintroduce; virtual; abstract;
    procedure DoReset; virtual; abstract;
  public
    constructor Create(const PathName: String; WatchSubtree: Boolean; NotifyFilter: TFilesystemChangeFilters);
		destructor Destroy; override;
  end;

implementation

{ TSilFilesystemNotifier }

constructor TSilFilesystemNotifier.Create(const PathName: String; WatchSubtree: Boolean; NotifyFilter: TFilesystemChangeFilters);
begin
	inherited Create(DoOpen(PathName, WatchSubtree, NotifyFilter));
end;

destructor TSilFilesystemNotifier.Destroy;
begin
  DoClose;
  inherited;
end;

end.
