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

unit SilOmInputSender;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  Sil,
  SilOiInputSender;

type
  IInputSender = SilOiInputSender.IInputSender;

type

{ TInputItem }

  TInputItem = class
  private
    FInput: TInput;
    FMouseButton: TInputMouseButton;
  public
    constructor Create(const Input: TInput);
    property Input: TInput read FInput;
    property MouseButton: TInputMouseButton read FMouseButton;
  end;

{ TInputSender }

  TInputSender = class(
    TInterfacedObject,
    IInputSender )
  private
    FList: IPointerList;
  public
    constructor Create;  
    destructor Destroy; override;
    procedure MouseMove(X, Y: Integer);
    procedure MouseDown(Button: TInputMouseButton; Release: Boolean); overload;
    procedure MouseDown(Buttons: array of Word; Release: Boolean); overload;
    procedure MouseUp(Button: TInputMouseButton);
    procedure KeyboardDown(VKey: Word; Release: Boolean); overload;
    procedure KeyboardDown(VKeys: array of Word; Release: Boolean); overload;
    procedure KeyboardUp(VKey: Word);
    procedure ReleaseKeyboard;
    procedure ReleaseMouse;
    function Execute: Integer;
  end;

implementation

uses
  SysUtils;

{ TInputItem }

constructor TInputItem.Create(const Input: TInput);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TInputItem.Create']);
(*)
  inherited Create;
  FInput := Input;
(*)end;

{ TInputSender }

constructor TInputSender.Create;
begin(*)
  inherited Create;
  FList := Sil.Tk.PointerList(False, ObjectHandler);
(*)end;

destructor TInputSender.Destroy;
begin(*)
  inherited;
  FList.Clear;
  FList := nil;
(*)end;

function TInputSender.Execute: Integer;
(*)var
  e: IEnumerator;
  Item: TInputItem;
  InputList: array of TInput;(*)
begin(*)
  SetLength(InputList, FList.Count);

  while FList.Enumerate(e, Item) do InputList[e.Iteration] := Item.Input;
  Result := SendInput(Length(InputList), InputList[0], SizeOf(TInput));

  FList.Clear;
(*)end;

procedure TInputSender.KeyboardDown(VKey: Word; Release: Boolean);
(*)var
  Input: TInput;
  InputItem: TInputItem;(*)
begin(*)
  Fillchar(Input, SizeOf(Input), 0);
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wVk := VKey;

  InputItem := TInputItem.Create(Input);
  FList.Add(InputItem);

  if Release then KeyboardUp(VKey);
(*)end;

procedure TInputSender.KeyboardDown(VKeys: array of Word; Release: Boolean);
(*)var
  i: Integer;(*)
begin(*)
  for i := 0 to High(VKeys) do KeyboardDown(VKeys[i]);
  if Release then for i := 0 to High(VKeys) do KeyboardUp(VKeys[i]);
(*)end;

procedure TInputSender.KeyboardUp(VKey: Word);
(*)var
  Input: TInput;
  InputItem: TInputItem;(*)
begin(*)
  Fillchar(Input, SizeOf(Input), 0);
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wVk := VKey;
  Input.ki.wScan := 0;
  Input.ki.dwFlags := KEYEVENTF_KEYUP;

  InputItem := TInputItem.Create(Input);
  FList.Add(InputItem);
(*)end;

procedure TInputSender.MouseDown(Button: TInputMouseButton; Release: Boolean);
(*)const
  AMouse: array[TInputMouseButton] of Integer =
    (0, MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_RIGHTDOWN, MOUSEEVENTF_MIDDLEDOWN);
var
  Input: TInput;
  InputItem: TInputItem;(*)
begin(*)
  Fillchar(Input, SizeOf(Input), 0);
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := AMouse[Button];

  InputItem := TInputItem.Create(Input);
  InputItem.FMouseButton := Button;
  FList.Add(InputItem);

  if Release then MouseUp(Button);
(*)end;

procedure TInputSender.MouseDown(Buttons: array of Word; Release: Boolean);
(*)var
  i: Integer;(*)
begin(*)
  for i := 0 to High(Buttons) do MouseDown(Buttons[i], Release);
(*)end;

procedure TInputSender.MouseMove(X, Y: Integer);
(*)var
  Input: TInput;
  InputItem: TInputItem;(*)
begin(*)
  Fillchar(Input, SizeOf(Input), 0);
  Input.Itype := INPUT_MOUSE;
  Input.mi.dx := X * 65535 div GetSystemMetrics(SM_CXSCREEN);
  Input.mi.dy := Y * 65535 div GetSystemMetrics(SM_CYSCREEN);
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;

  InputItem := TInputItem.Create(Input);
  InputItem.FMouseButton := imNone;
  FList.Add(InputItem);
(*)end;

procedure TInputSender.MouseUp(Button: TInputMouseButton);
(*)const
  AMouse: array[TInputMouseButton] of Integer =
    (0, MOUSEEVENTF_LEFTUP, MOUSEEVENTF_RIGHTUP, MOUSEEVENTF_MIDDLEUP);
var
  Input: TInput;
  InputItem: TInputItem;(*)
begin(*)
  Fillchar(Input, SizeOf(Input), 0);
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := AMouse[Button];

  InputItem := TInputItem.Create(Input);
  InputItem.FMouseButton := Button;
  FList.Add(InputItem);
(*)end;

procedure TInputSender.ReleaseKeyboard;
(*)var
  e: IEnumerator;
  Item: TInputItem;(*)
begin(*)
  while FList.Enumerate(e, Item) do
    if (Item.Input.Itype = INPUT_KEYBOARD) and (Item.Input.ki.dwFlags = 0) then
      KeyboardUp(Item.Input.ki.wVk);
(*)end;

procedure TInputSender.ReleaseMouse;
(*)var
  e: IEnumerator;
  Item: TInputItem;(*)
begin(*)
  while FList.Enumerate(e, Item) do
    if (Item.Input.Itype = INPUT_MOUSE) and
      (Item.Input.mi.dwFlags in [MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_RIGHTDOWN, MOUSEEVENTF_MIDDLEDOWN]) then
      MouseUp(Item.MouseButton);
(*)end;

end.
