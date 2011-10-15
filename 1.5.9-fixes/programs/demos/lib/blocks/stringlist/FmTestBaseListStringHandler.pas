unit FmTestBaseListStringHandler;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, StdCtrls;

type
  ISampleObject = interface
    ['{E432E902-3B34-11D4-AD7B-00902794F778}']
    function Name: string;
  end;

  PSampleRecord = ^TSampleRecord;
  TSampleRecord = record
    s: string;
    v: OleVariant;
  end;

  TSampleClass = class
    c: TSampleRecord;
    constructor Create(const s: string);
    destructor Destroy; override;
  end;

  SampleHandler = class(DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

  TFormTestHandler = class(TForm)
    Listbox: TListBox;
    btStringList: TButton;
    btStringPtr: TButton;
    btVariantPtr: TButton;
    btVariantList: TButton;
    btInterfaceList: TButton;
    btInterfacePtr: TButton;
    btVoidPtr: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure btStringListClick(Sender: TObject);
    procedure btStringPtrClick(Sender: TObject);
    procedure btVariantPtrClick(Sender: TObject);
    procedure btVariantListClick(Sender: TObject);
    procedure btInterfaceListClick(Sender: TObject);
    procedure btInterfacePtrClick(Sender: TObject);
    procedure btVoidPtrClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TObjetito = class(
    TSilObject,
    ISampleObject,
    IComparable )
  private
    FName: string;
  protected // ISampleObject
    function Name: string;
  protected // IComparable 
    function CompareTo(const Item; Arg: Pointer = nil): Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  public
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public 
    class function New(const AName: string): ISampleObject;
  end;

var
  FormTestHandler: TFormTestHandler;

implementation

uses
  TypInfo,
  SilLiList,
  SilBtVoidPtr,
  SilBtStringPtr,
  SilBtVariantPtr,
  SilBtInterfacePtr, SilBtVart;

{$R *.DFM}

procedure TFormTestHandler.FormClick(Sender: TObject);
begin
  Listbox.Items.Clear;
end;

procedure TFormTestHandler.btStringListClick(Sender: TObject);
var
  l: IStringList;
  s: String;
  e: IEnumerator;
  p: PString;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.StringList(False, StringHandler);

  s := 'uno';
  l.Add('0', @s);

  s := 'dos';
  l.Add('1', @s);

  s := 'tres';
  l.Add('2', @s);

  s := 'reemplazar!!';
  l.Add('3', @s);

  s := 'tres3';

  l.Ptrs[3] := @s;

  while l.Enumerate(e, s) do
  begin
    p := l.Ptrs[e.Iteration];
    if p <> nil then
      s := s + '-' + PString(p)^;
    Listbox.Items.Add(s);
  end;

end;


procedure TFormTestHandler.btVariantListClick(Sender: TObject);
var
  l: IStringList;
  v: Variant;
  s: String;
  e: IEnumerator;
  p: PVariant;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.StringList(False, VariantHandler);

  v := TObjetito.New('uno');
  l.Add('0', @v);

  v := TObjetito.New('dos');;
  l.Add('1', @v);

  v := TObjetito.New('tres');;
  l.Add('2', @v);

  v := TObjetito.New('cuatro');;
  l.Add('3', @v);

  v := TObjetito.New('reemplazar!!');;
  l.Add('4', @v);

  v := Sil.Vart.Unassigned;
  l.Add('5', @v);

  v := TObjetito.New('cinco');;
  l.Ptrs[4] := @v;

  while l.Enumerate(e, s) do
  begin
    p := l.Ptrs[e.Iteration];
    
    if Vart.VType(p^) = varUnknown then
      s := (Vart.ToUnknown(p^) as ISampleObject).Name else
      s := Vart.ToStr(p^, 'EMPTY');

    Listbox.Items.Add(s);
  end;
  
  Caption := Int.ToStr(l.IndexOfPtr(@v));
end;

procedure TFormTestHandler.btInterfaceListClick(Sender: TObject);
var
  l: IStringList;
  s: string;
  o: IUnknown;
  e: IEnumerator;
  p: Pointer;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.StringList(False, InterfaceHandler);

  o := TObjetito.New('uno');
  l.Add('0', @o);

  o := TObjetito.New('dos');
  l.Add('0', @o);

  o := TObjetito.New('tres');
  l.Add('0', @o);

  o := TObjetito.New('tres3');
  l.Ptrs[2] := @o;

  while l.Enumerate(e, s) do
  begin
    p := l.Ptrs[e.Iteration];

    if p <> nil then
      s := (IUnknown(p) as ISampleObject).Name else
      s := 'EMPTY';

    Listbox.Items.Add(s);
  end;

  o := TObjetito.New('dos');
  Caption := Int.ToStr(l.IndexOfPtr(@o));
end;

procedure TFormTestHandler.Button6Click(Sender: TObject);

  function DoNew(const text: String): PChar;
  begin
    result := allocmem(length(text) + 1);
    move(text[1], result^, length(text));
  end;

var
  l: IStringList;
  o: PChar;
  e: IEnumerator;
  s: string;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.StringList(False, PointerHandler);

  o := DoNew('cero');
  l.Add('0', o);

  o := DoNew('uno');
  l.Add('1', o);

  o := DoNew('dos');
  l.Add('2', o);

  o := DoNew('dos');
  l.Add('3', o);

  o := DoNew('newbie');
  l.Ptrs[3] := o;

  while l.Enumerate(e, s) do
  begin
    o := l.Ptrs[e.Iteration];

    if o <> nil then
      s := o else
      s := 'EMPTY';

    Listbox.Items.Add(s);
  end;

  Caption := Int.ToStr(l.IndexOfPtr(o));
  l := nil;
end;

procedure TFormTestHandler.Button5Click(Sender: TObject);
var
  l: IStringList;
  s: string;
  o: TSampleClass;
  e: IEnumerator;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.StringList(False, ObjectHandler);

  o := TSampleClass.Create('cero');
  l.Add('0', o);

  o := TSampleClass.Create('uno');
  l.Add('1', o);

  o := TSampleClass.Create('dos');
  l.Add('2', o);

  o := TSampleClass.Create('dos');
  l.Add('3', o);

  o := TSampleClass.Create('newbie');
  l.Ptrs[3] := o;

  while l.Enumerate(e, s) do
  begin
    o := l.Ptrs[e.Iteration];

    if o <> nil then
      s := o.c.s else
      s := 'EMPTY';

    Listbox.Items.Add(s);
  end;

  o := l.Ptrs[2];
  Caption := Int.ToStr(l.IndexOfPtr(o));
end;

procedure TFormTestHandler.Button1Click(Sender: TObject);
var
  l: IPointerList;
  c: TSampleRecord;
  e: IEnumerator;
  p: TSampleRecord;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.PointerList(False, SampleHandler);

  c.s := '1';
  c.v := 'uno';
  l.Add(@c);

  c.s := '2';
  c.v := 'dos';
  l.Add(@c);

  c.s := '3';
  c.v := 'tres';
  l.Add(@c);

  c.s := '5';
  c.v := 'uno';
  l[2] := @c;


  Caption := PSampleRecord(l.First).s;

  while l.Enumerate(e, p) do
  begin
    Listbox.Items.Add(p.s);
  end;

(*  o := TObjetito.New('dos');
  Caption := Int.ToStr(l.IndexOfPtr(@o)); *)
end;

procedure TFormTestHandler.Button2Click(Sender: TObject);
var
  l: IPointerList;
  e: IEnumerator;
  p: TSampleClass;
  s: string;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.PointerList(False, ObjectHandler);

  l.Add(TSampleClass.Create('0'));

  l.Add(TSampleClass.Create('1'));

  l.Add(TSampleClass.Create('2'));

  l.Add(TSampleClass.Create('2'));

  l[3] := TSampleClass.Create('newbie');

  Caption := TSampleClass(l.First).c.s;

  while l.Enumerate(e, p) do
  begin
    if p <> nil then
      s := p.c.s else
      s := 'nil';

    Listbox.Items.Add(s);
  end;

(*  o := TObjetito.New('dos');
  Caption := Int.ToStr(l.IndexOfPtr(@o)); *)
end;

procedure TFormTestHandler.Button4Click(Sender: TObject);
var
  l: IPointerList;
  e: IEnumerator;
  o: ISampleObject;
  s: string;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.PointerList(False, InterfaceHandler);

  o := TObjetito.New('0');
  l.Add(@o);

  o := TObjetito.New('1');
  l.Add(@o);

  o := TObjetito.New('2');
  l.Add(@o);

  o := TObjetito.New('3');
  l.Add(@o);

  o := TObjetito.New('newbie');
  l[3] := @o;

  Caption := (IUnknown(l.First) as ISampleObject).Name;

  while l.Enumerate(e, o) do
  begin
    s := o.Name;
    Listbox.Items.Add(s);
  end;

(*  o := TObjetito.New('dos');
  Caption := Int.ToStr(l.IndexOfPtr(@o)); *)
end;

procedure TFormTestHandler.Button3Click(Sender: TObject);
var
  l: IPointerList;
  e: IEnumerator;
  s: string;
begin
  Listbox.Items.Clear;
  
  l := Sil.List.PointerList(False, StringHandler);

  s := '1';
  l.Add(@s);

  s := '2';
  l.Add(@s);

  s := '3';
  l.Add(@s);

  s := '4';
  l.Add(@s);

  s := 'newbie';
  l[3] := @s;

  Caption := PString(l.First)^;

  while l.Enumerate(e, s) do
  begin
    Listbox.Items.Add(s);
  end;

(*  o := TObjetito.New('dos');
  Caption := Int.ToStr(l.IndexOfPtr(@o)); *)
end;

procedure TFormTestHandler.btStringPtrClick(Sender: TObject);
var
  s1, s2: string;
  p, q: pointer;
begin
  Listbox.Items.Clear;
  
  s1 := 'testing';

  StringHandler.ToPtr(s1, p);

  StringHandler.ToObj(P, s2);

  StringHandler.ToPtr(s2, q);

  StringHandler.Free(P);

  StringHandler.Free(q);

  StringHandler.Clear(s2);

  StringHandler.Clear(s1);

end;

procedure TFormTestHandler.btVariantPtrClick(Sender: TObject);
var
  v1, v2: Variant;
  p, q: pointer;
begin
  Listbox.Items.Clear;
  
  v1 := TObjetito.New('testing');

  VariantHandler.ToPtr(v1, p);

  VariantHandler.ToObj(P, v2);

  VariantHandler.ToPtr(v2, q);

  VariantHandler.Free(P);

  VariantHandler.Free(q);

  VariantHandler.Clear(v2);

  VariantHandler.Clear(v1);

end;

procedure TFormTestHandler.btInterfacePtrClick(Sender: TObject);
var
  i1, i2: IUnknown;
  p, q: pointer;
begin
  Listbox.Items.Clear;
  
  i1 := TObjetito.New('testing');

  InterfaceHandler.ToPtr(i1, p);

  InterfaceHandler.ToObj(P, i2);

  InterfaceHandler.ToPtr(i2, q);

  InterfaceHandler.Free(P);

  InterfaceHandler.Free(q);

  InterfaceHandler.Clear(i2);

  InterfaceHandler.Clear(i1);
end;

procedure TFormTestHandler.btVoidPtrClick(Sender: TObject);
var
  p1, p2: Pointer;
  p, q: pointer;
begin
  Listbox.Items.Clear;
  
  p1 := Sender;

  VoidHandler.ToPtr(p1, p);

  VoidHandler.ToObj(P, p2);

  VoidHandler.ToPtr(p2, q);

  VoidHandler.Free(P);

  VoidHandler.Free(q);

  VoidHandler.Clear(p2);

  VoidHandler.Clear(p1);
end;

function Objetito(const Ptr): ISampleObject;
begin
  Result := IUnknown(Ptr) as ISampleObject;
end;


{ TObjetito }

constructor TObjetito.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

destructor TObjetito.Destroy;
begin
  inherited;
end;

function TObjetito.Name: string;
begin
  Result := FName;
end;

function TObjetito.CompareTo(const Item; Arg: Pointer): Integer;
begin
  Result := Sil.Str.Compare(FName, (IUnknown(Item) as ISampleObject).Name);
end;

class function TObjetito.New(const AName: string): ISampleObject;
begin
  Result := Create(AName);
end;

function TObjetito._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TObjetito._Release: Integer;
begin
  Result := inherited _Release;
end;

{ SampleHandler }

class procedure SampleHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PSampleRecord(Result));
end;

class procedure SampleHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PSampleRecord(Value));
end;

class procedure SampleHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PSampleRecord(Dest)^ := PSampleRecord(Source)^;
end;

class procedure SampleHandler.Clear(var Obj; const Data: Pointer);
begin
end;

class function SampleHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := 0;
end;

{ TSampleClass }

constructor TSampleClass.Create(const s: string);
begin
  c.s := s;
end;

destructor TSampleClass.Destroy;
begin
  inherited;
end;

{ RecordHandler }

(*)
class procedure RecordHandler.ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer);
begin
  New(Pointer(Obj), Ptr, Data);
end;

class procedure RecordHandler.ToObj(const Ptr: Pointer; var Obj; const Data: Pointer);
begin
  Copy(Ptr, @Obj, Data);
end;
(*)

end.
