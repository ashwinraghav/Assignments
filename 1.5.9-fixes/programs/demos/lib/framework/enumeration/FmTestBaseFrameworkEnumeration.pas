(****************************************************************************)
(*                                                                          *)
(*     Standard Interface Library  - Application Framework Services         *)
(*                                                                          *)
(* Demonstration of the Sil.Tk.Enumeration tool                             *)
(*                                                                          *)
(* This program shows how is used the clone mechanism for arbitrary objects *)
(*  The only requisite is that the target object implements the interface   *)
(*  ICloneable.                                                             *)
(*                                                                          *)
(* There are many kinds of objects which already implements this interface, *)
(* for example: MemoryStream, DataRowsets, etc.                             *)
(*                                                                          *)
(****************************************************************************)

unit FmTestBaseFrameworkEnumeration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SIL, SilClasses, StdCtrls, SilVCL;

type
  IObjetito = interface
    ['{E432E902-3B34-11D4-AD7B-00902794F778}']
    function Name: string;
  end;

  TFormTestEnumeration = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FList: IInterfaceList; //IStringList;
  public
  end;

  TSampleObject = class(
    TSilInterfacedObject,
    IObjetito )
  private
    FName: string;
    function Name: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    class function New(const AName: string): IObjetito;
  end;

var
  FormTestEnumeration: TFormTestEnumeration;

implementation

{$R *.DFM}

procedure TFormTestEnumeration.FormCreate(Sender: TObject);
begin
  FList := SIL.List.InterfaceList();
  FList.Add(TSampleObject.New('object one'));
  FList.Add(TSampleObject.New('object two'));
  FList.Add(TSampleObject.New('object three'));
  FList.Add(TSampleObject.New('object four'));
  FList.Add(TSampleObject.New('object five'));
  FList.Add(TSampleObject.New('object six'));
  FList.Add(TSampleObject.New('object seven'));
  FList.Add(TSampleObject.New('object eight'));
  FList.Add(nil);
end;

procedure TFormTestEnumeration.FormDestroy(Sender: TObject);
begin
  FList := nil;
end;

procedure TFormTestEnumeration.Button1Click(Sender: TObject);
var
  e: IEnumerator;
  i: IObjetito;
begin
  Memo1.Lines.Clear;
  with SIL.Tk.Enumeration(FList) do
    while Enumerate(e, i) do
      if i <> nil then
      Memo1.Lines.Add(i.Name) else
      Memo1.Lines.Add('nil');
end;

{ TSampleObject }

constructor TSampleObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

destructor TSampleObject.Destroy;
begin
  inherited;
end;

function TSampleObject.Name: string;
begin
  Result := FName;
end;

class function TSampleObject.New(const AName: string): IObjetito;
begin
  Result := Create(AName);
end;

end.
