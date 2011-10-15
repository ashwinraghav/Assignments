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

unit SilVmLockers;

{$I Defines.inc}

interface

uses
  Windows,
  Messages,
  Classes,
  ComCtrls,

  Sil,
  SilClasses;

type
  TDisplayLocker = class(TExtendedLocker)
  private
    FWindow: HWND;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(AWindow: HWND);
  end;

type
  TStringsLocker = class(TExtendedLocker, ILocker)
  private
    FStrings: TStrings;
    FClearOnLock: Boolean;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(AStrings: TStrings; ClearOnLock: Boolean);
  end;

type
  TListItemsLocker = class(TExtendedLocker, ILocker)
  private
    FItems: TListItems;
    FClearOnLock: Boolean;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(AItems: TListItems; ClearOnLock: Boolean);
  end;

type
  TTreeItemsLocker = class(TExtendedLocker, ILocker)
  private
    FItems: TTreeNodes;
    FClearOnLock: Boolean;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(AItems: TTreeNodes; ClearOnLock: Boolean);
  end;

implementation

//=============================================================

constructor TDisplayLocker.Create(AWindow: HWND);
begin
  inherited Create(Pointer(AWindow));
  FWindow := AWindow;
end;

procedure TDisplayLocker.DoLock;
begin
  SendMessage(FWindow, WM_SETREDRAW, 0, 0);
end;

procedure TDisplayLocker.DoUnlock;
begin
  SendMessage(FWindow, WM_SETREDRAW, 1, 0);
end;

//=============================================================

constructor TStringsLocker.Create(AStrings: TStrings; ClearOnLock: Boolean);
begin
  Assert(AStrings <> nil);
  inherited Create(AStrings);
  FStrings := AStrings;
  FClearOnLock := ClearOnLock;
end;

procedure TStringsLocker.DoLock;
begin
  FStrings.BeginUpdate;
  if FClearOnLock then FStrings.Clear;
end;

procedure TStringsLocker.DoUnlock;
begin
  FStrings.EndUpdate;
end;

//=============================================================

constructor TListItemsLocker.Create(AItems: TListItems; ClearOnLock: Boolean);
begin
  Assert(AItems <> nil);
  inherited Create(AItems);
  FItems := AItems;
  FClearOnLock := ClearOnLock;
end;

procedure TListItemsLocker.DoLock(const ALock: ILock);
begin
  FItems.BeginUpdate;
  if FClearOnLock then FItems.Clear;
end;

procedure TListItemsLocker.DoUnlock(const ALock: ILock);
begin
  FItems.EndUpdate;
end;

//=============================================================

constructor TTreeItemsLocker.Create(AItems: TTreeNodes; ClearOnLock: Boolean);
begin
  Assert(AItems <> nil);
  inherited Create(AItems);
  FItems := AItems;
  FClearOnLock := ClearOnLock;
end;

procedure TTreeItemsLocker.DoLock(const ALock: ILock);
begin
  FItems.BeginUpdate;
  if FClearOnLock then FItems.Clear;
end;

procedure TTreeItemsLocker.DoUnlock(const ALock: ILock);
begin
  FItems.EndUpdate;
end;

end.
