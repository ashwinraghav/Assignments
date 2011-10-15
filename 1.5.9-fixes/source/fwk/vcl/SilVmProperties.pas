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

unit SilVmProperties;

{$I Defines.inc}

interface

uses
  Windows, Messages, Classes,
  
  Sil;

const
  PM_SETPROP = WM_USER + 1;

type

{ TPropertySet }

  PPropertyItem = ^TPropertyItem;
  TPropertyItem = record
    Sender: TPersistent;
    PropName: String;
    AsString: String;
    case Integer of
      0: (AsBoolean: Boolean);
      1: (AsInteger: Integer);
      2: (AsFloat: Double);
  end;

  TPropertySet = class
  private
    hWind: IHandle;
    procedure WndProc(var Msg);
    procedure DoValue(Info: PPropertyItem; const DoNow: Boolean);
    function NewSet(Sender: TPersistent; const PropName: String): PPropertyItem;
    class procedure DoCreateInstance(const Sender: IUnknown; Param: Pointer);
  public
    class function App: TPropertySet;
    constructor Create;
    destructor Destroy; override;
    procedure Value(Sender: TPersistent; const PropName: String; const Value: String; const DoNow: Boolean = false); overload;
    procedure Value(Sender: TPersistent; const PropName: String; const Value: Boolean; const DoNow: Boolean = false); overload;
    procedure Value(Sender: TPersistent; const PropName: String; const Value: Integer; const DoNow: Boolean = false); overload;
    procedure Value(Sender: TPersistent; const PropName: String; const Value: Double; const DoNow: Boolean = false); overload;
  end;

implementation

uses
  TypInfo,
  Forms;

var
  PropertySet: TPropertySet = nil;
              
{ TPropertySet }

class procedure TPropertySet.DoCreateInstance(const Sender: IUnknown; Param: Pointer);
begin
  PropertySet := TPropertySet.Create;
end;

class function TPropertySet.App: TPropertySet;
begin
  if PropertySet = nil then
  begin
    if not OS.Thread.Current.IsMain then
      OS.Thread.SyncCall(DoCreateInstance) else
      PropertySet := TPropertySet.Create;
  end;

  Result := PropertySet;
end;

constructor TPropertySet.Create;
begin
  inherited Create;
  hWind := OS.ToolWindow.Create(WndProc);
end;

destructor TPropertySet.Destroy;
begin
  hWind := nil;
  inherited Destroy;
end;

procedure TPropertySet.WndProc(var Msg);
var
  PropInfo: PPropInfo;
  Item: PPropertyItem;
begin
  with TMessage(Msg) do
  begin
    if Msg = PM_SETPROP then
    begin
      Item := PPropertyItem(WParam);
      PropInfo := GetPropInfo(Item.Sender.ClassInfo, Item.PropName);

      if (PropInfo <> nil) and not Application.Terminated then
        try
          case PropInfo^.PropType^.Kind of
            tkString, tkLString, tkWString:
              SetStrProp(Item.Sender, PropInfo, Item.AsString);

            tkInteger, tkChar, tkEnumeration, tkWChar:
              SetOrdProp(Item.Sender, PropInfo, Item.AsInteger);

            tkFloat:
              SetFloatProp(Item.Sender, PropInfo, Item.AsFloat);

            tkClass:
              if (GetTypeData(PropInfo^.PropType^)^.ClassType = TStrings) then
                with TStrings(GetOrdProp(Item.Sender, PropInfo)) do
                  SetText(PChar(Item.AsString));
          end;
        except end;
       
      Dispose(Item);
    end else
    if not Application.Terminated and (hWind <> nil) then
      DefWindowProc(hWind.Value, Msg, WParam, LParam);
  end;
end;

function TPropertySet.NewSet(Sender: TPersistent; const PropName: String): PPropertyItem;
begin
  New(Result);
  Result.Sender := Sender;
  Result.PropName := PropName;
end;

procedure TPropertySet.DoValue(Info: PPropertyItem; const DoNow: Boolean);
begin
   if DoNow then SendMessage(hWind.Value, PM_SETPROP, WParam(Info), 0) else
  if not PostMessage(hWind.Value, PM_SETPROP, WParam(Info), 0) then Dispose(Info);
end;

procedure TPropertySet.Value(Sender: TPersistent; const PropName: String; const Value: String; const DoNow: Boolean);
var
  Info: PPropertyItem;
begin
  if Application.Terminated then Exit;
  Info := NewSet(Sender, PropName);
  Info.AsString := Value;
  DoValue(Info, DoNow);
end;

procedure TPropertySet.Value(Sender: TPersistent; const PropName: String; const Value: Double; const DoNow: Boolean);
var
  Info: PPropertyItem;
begin
  if Application.Terminated then Exit;
  Info := NewSet(Sender, PropName);
  Info.AsFloat := Value;
  DoValue(Info, DoNow);
end;

procedure TPropertySet.Value(Sender: TPersistent; const PropName: String; const Value: Integer; const DoNow: Boolean);
var
  Info: PPropertyItem;
begin
  if Application.Terminated then Exit;
  Info := NewSet(Sender, PropName);
  Info.AsInteger := Value;
  DoValue(Info, DoNow);
end;

procedure TPropertySet.Value(Sender: TPersistent; const PropName: String; const Value, DoNow: Boolean);
var
  Info: PPropertyItem;
begin
  if Application.Terminated then Exit;
  Info := NewSet(Sender, PropName);
  Info.AsBoolean := Value;
  DoValue(Info, DoNow);
end;

initialization

finalization
  PropertySet.Free;

end.
