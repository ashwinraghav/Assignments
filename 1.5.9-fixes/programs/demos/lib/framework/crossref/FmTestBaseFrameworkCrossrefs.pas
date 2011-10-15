(****************************************************************************)
(*                                                                          *)
(*     Standard Interface Library  - Application Framework Services         *)
(*                                                                          *)
(* Demonstration of the TSilInterfacedObject services:                      *)
(*   ICrossReference                                                        *)
(*                                                                          *)
(* It often happens that some objects needs to reference each other, but    *)
(*  they are otherwise unrelated. When both of them can be released at      *)
(*  an unknown moment in time and without a coordinating controller it      *)
(*  normally becomes really difficult to provide cleanup code that handles  *)
(*  the destruction in a gracefull way. In any case, this burden is         *)
(*  necessary in several places and is distributed among different units.   *)
(*                                                                          *)
(* This mechanism provides a way of take soft references of any objects,    *)
(*  only provided it implements the ICrossReference, as                     *)
(*  TSilInterfacedObject does, and the runtime then becomes responsible of  *)
(*  taking care to clean up those references when an object is released     *)
(*  outside the control of the referencing object.                          *)
(*                                                                          *)
(****************************************************************************)

unit FmTestBaseFrameworkCrossrefs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, StdCtrls;

type
  IMemo = interface
    ['{AA1CB6BB-D3B2-4111-8A90-A5FB7CEB3321}']
    procedure Put(const Message: string);
  end;
  
  ISecondObject = interface
    ['{9799E32D-198E-43B5-8D6B-7E386562D2CF}']
    function GetFirst: IUnknown;
    property First: IUnknown read GetFirst;
  end;

  TSecondObject = class(
    TSilInterfacedObject,
    ISecondObject )
  private
    FMemo: IMemo;
    FFirst: Pointer;
  protected // ISecondObject
    function GetFirst: IUnknown;
  protected
    property Memo: IMemo read FMemo;
  public
    constructor Create(const Memo: IMemo; const First: IUnknown);
    destructor Destroy; override;
  end;

  TFirstObject = class(TSilInterfacedObject)
  private
    FMemo: IMemo;
  protected
    property Memo: IMemo read FMemo;
  public
    constructor Create(const Memo: IMemo);
    destructor Destroy; override;
  end;

  TFormTestCrossrefs = class(
    TForm,
    IUnknown,
    IMemo )
    btCreateFirst: TButton;
    btCreateSecond: TButton;
    btDestroyFirst: TButton;
    btDestroySecond: TButton;
    btCheckRreference: TButton;
    Memo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure btCreateFirstClick(Sender: TObject);
    procedure btDestroySecondClick(Sender: TObject);
    procedure btCreateSecondClick(Sender: TObject);
    procedure btDestroyFirstClick(Sender: TObject);
    procedure btCheckRreferenceClick(Sender: TObject);
  private
    FFirst: IUnknown;
    FSecond: ISecondObject;
  protected // IMemo
    procedure Put(const Message: string);
  end;

var
  FormTestCrossrefs: TFormTestCrossrefs;

implementation

{$R *.dfm}

{ TFormTestCrossrefs }

procedure TFormTestCrossrefs.FormDestroy(Sender: TObject);
begin
  FFirst := nil;
end;

procedure TFormTestCrossrefs.btCreateFirstClick(Sender: TObject);
begin
  Put('Form: creating first');
  // the first object: it is hold here outside the control of the second
  FFirst := TFirstObject.Create(Self);
end;

procedure TFormTestCrossrefs.btCreateSecondClick(Sender: TObject);
begin
  if Assigned(FFirst) then  // this only makes sense in presence of the first
  begin
    Put('Form: creating second');
    // the second: takes a reference to the first. See the constructor code below.
    FSecond := TSecondObject.Create(Self, FFirst);
  end else
    Put('Form: cannot create if there is no first!!! create it and try again');
end;

procedure TFormTestCrossrefs.btDestroyFirstClick(Sender: TObject);
begin
  Put('Form: releasing first');
  // releasing the first: is the second is created then its ref will be cleaned out.
  FFirst := nil;
end;

procedure TFormTestCrossrefs.btDestroySecondClick(Sender: TObject);
begin
  Put('Form: releasing second');
  // done with the second.
  FSecond := nil;
end;

procedure TFormTestCrossrefs.btCheckRreferenceClick(Sender: TObject);
var
  o: IUnknown;
begin
  Put('Form: checking the cross ref');
  if Assigned(FSecond) then // checking only makes sense when the second is created
  begin
    // query the reference that the second holds on ...
    o := FSecond.First;
    if Assigned(o) then // it is nil??
      Put('Form: the reference is still there') else      // Nope. The first already is created 
      Put('Form: the reference has been nil''d out!!!')   // Yes! Who's cleaned up this??
  end else
    Put('Form: second is not created');   // create the second obeject and try again.
end;

procedure TFormTestCrossrefs.Put(const Message: string);
begin
  Memo.Lines.Add(Message);
end;

{ TFirstObject }

constructor TFirstObject.Create(const Memo: IMemo);
begin
  inherited Create;
  FMemo := Memo;
  Memo.Put('First: created');
end;

destructor TFirstObject.Destroy;
begin
  Memo.Put('First: destroyed');
  FMemo := nil;
  inherited;
end;

{ TSecondObject }

constructor TSecondObject.Create(const Memo: IMemo; const First: IUnknown);
begin
  inherited Create;
  FMemo := Memo;
  // Note: this will take a pointer to the object First
  //  without adding a reference to it
  MakeRef(First, @FFirst);
  Memo.Put('Second: created'); 
end;

destructor TSecondObject.Destroy;
begin
  Memo.Put('Second: destroyed');
  // This is normally unnecessary, because the destructor
  //  of TSilInterfacedObject takes care of the needed drops
  DropRef(@FFirst);
  FMemo := nil;
  inherited;
end;

function TSecondObject.GetFirst: IUnknown;
begin
  Result := IUnknown(FFirst);
end;

end.
