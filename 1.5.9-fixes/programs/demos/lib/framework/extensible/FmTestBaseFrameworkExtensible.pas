(***************************************************************************)
(*                                                                         *)
(*     Standard Interface Library  - Application Framework Services        *)
(*                                                                         *)
(* This is a demonstration of how to use a TSilExtensibleObject.           *)
(*                                                                         *)
(* To better understand the Aggregation model see the                      *)
(*   TestBaseFrameworkAggregation demo.                                    *)
(*                                                                         *)
(* This demo is very similar to the above mentioned. But it provides a     *)
(*  different look at some aspects of the aggregation mechanism.           *)
(* Keep in mind that the IAggregation is a lower level implementation      *)
(*  concept. And a TSilExtensibleObject is just another descendant of      *)
(*  TSilObject which in addition provides extension capabilities.          *)
(*                                                                         *)
(* The idea is to use the TSilAggregationObject as a building block for    *)
(*  other objects, in this case this demo covers the other objects that    *)
(*  we provide that keeps an aggregation inside them an publish its        *)
(*  interface with a protected property, so any one who want to            *)
(*  implement interfaces and aggregate others can derive from it and       *)
(*  add any needed extensions by using this protected api.                 *)
(*                                                                         *)
(***************************************************************************)

unit FmTestBaseFrameworkExtensible;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilLhAggregation // this is private so we need to add it here
  ;

type
  // simple interface that we gonna use to send text to the Memo
  ITest = interface
    ['{3A504219-BC52-45AF-ABC2-299A24CEA66C}']
    procedure Add(const Text: string);
  end;

  // The standard TSilExtensibleObject
  //  doesn't provides a public interface
  //  and we're needing to get its Aggregation
  IExtensibleObject = interface
    ['{0B7664C8-0125-4D86-B164-04EEAB7BB860}']
    function GetExtension: IAggregation;
    property Aggregation: IAggregation read GetExtension;
  end;

  // This is the main form. We use it as a controller object, the one that we want to "extend"
  //    with arbitrary interfaces.
  TFormTestExtensible = class(
    TForm,
    ITest // implementing the ITest for the aggregation extensions
    )
    Memo: TMemo;
    TestEnumerate: TButton;
    AddEnumerable: TButton;
    RemoveEnumerable: TButton;
    AddConnectable: TButton;
    RemoveConnectable: TButton;
    TestDisconnect: TButton;
    TestConnect: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure TestEnumerateClick(Sender: TObject);
    procedure AddEnumerableClick(Sender: TObject);
    procedure RemoveEnumerableClick(Sender: TObject);
    procedure AddConnectableClick(Sender: TObject);
    procedure RemoveConnectableClick(Sender: TObject);
    procedure TestConnectClick(Sender: TObject);
    procedure TestDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    // This holds the main IUnknown of the extensible object.
    //  It is *critical* that we keep its main IUnknown instead of
    //  any of its interfaces, because we are aggregating ourselves to
    //  it, that is, if were called QueryInterface for any interface, and keeped it
    //  intead of its main IUnknown, then it were called _AddRef on our IUnknown
    //  so it we're going to be destroyed at end of scope with us
    //  holding a pointer to our IUnknown!
    FExtensible: IUnknown;
  private
    // helper to get IExtensibleObject from the above IUnknown
    function DoGetExtensible: IExtensibleObject;
  protected // ITest
    //  this simply writes out the Text to the Memo
    procedure Add(const Text: string);
  protected
    // we override QueryInterface to delegate the interfaces that we don't know
    //  to the extensible object
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  protected
    // helper property to get the IExtensibleObject interface
    property Extensible: IExtensibleObject read DoGetExtensible;
  end;

  // Here we provide a stub class to support
  //  the IExtensibleObject interface.
  //  We aren't required to write a GetExtension method
  //  because there is already a version of it
  //  in the TSilExtensibleObject
  TExtensibleObject = class(
    TSilExtensibleObject,
    IExtensibleObject )
  end;

var
  FormTestExtensible: TFormTestExtensible;

implementation

{$R *.dfm}

type
  // This is an IEnumerable extension.
  TEnumerableImplementation = class(
    TSilAggregableObject,
    IEnumerable )
  private
    // get hold of our owner.
    //  But be extracarefull!!
    //  Do not keep references to it!!!
    //  So, we're going to keep it as a plain pointer
    FOwner: Pointer;
  private 
    // helper to obtain the ITest interface from our owner
    function DoGetOwner: ITest;
  protected // implementing IEnumerable
    // simulate giving an IEnumerator away ...
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected
    // helper to obtain the ITest interface
    property Owner: ITest read DoGetOwner;
  public
    // the constructor is virtual because it is called by the aggregation object
    //  without needing to know the exact class involved.
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); override;
    destructor Destroy; override;
  end;

type
  TConnectableImplementation = class(
    TSilAggregableObject,
    IConnectable )
  private
    // see above!!!
    FOwner: Pointer;
  private
    // helper to obtain the ITest interface
    function DoGetOwner: ITest;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean = true);
    procedure RemoveListener(const Listener: IUnknown);
  protected
    // helper to obtain the ITest interface
    property Owner: ITest read DoGetOwner;
  public
    // see above!!!
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); override;
    destructor Destroy; override;
  end;

{ TFormTestExtensible }

procedure TFormTestExtensible.FormCreate(Sender: TObject);
begin
  // creates the ExtensibleObject and keeps its main IUnknown
  FExtensible := TExtensibleObject.Create(Self);
end;

procedure TFormTestExtensible.FormDestroy(Sender: TObject);
begin
  // Releasing its main IUnknown will cause its destruction.
  FExtensible := nil;
end;

function TFormTestExtensible.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // First, try to resolve the queried interface by ourselves
  Result := inherited QueryInterface(IID, Obj);
  if Result <> 0 then
    // ok, if we don't know the interface lets try to get it from the extensible object
    Result := FExtensible.QueryInterface(IID, Obj);
end;

procedure TFormTestExtensible.Add(const Text: string);
begin
  // Put the text in the memo!
  Memo.Lines.Add(Text);
end;

function TFormTestExtensible.DoGetExtensible: IExtensibleObject;
begin
  // casting the IUnknown we hold to obtain the interface we want is an
  //  expensive solution, so we use here only because this is a demo!!
  Result := FExtensible as IExtensibleObject;
end;

procedure TFormTestExtensible.AddEnumerableClick(Sender: TObject);
begin
// extend our services by adding an IEnumerable implementation
//  note the second argument: this is the Owner for the extension object
  Extensible.Aggregation.Add(TEnumerableImplementation, Self);
end;

procedure TFormTestExtensible.RemoveEnumerableClick(Sender: TObject);
begin
// thats all, IEnumerable were not needed anymore so remove it
  Extensible.Aggregation.Remove(TEnumerableImplementation);
end;

procedure TFormTestExtensible.AddConnectableClick(Sender: TObject);
begin
// extend our services by adding an IConnectable implementation
//  note the second argument: this is the Owner for the extension object
  Extensible.Aggregation.Add(TConnectableImplementation, Self);
end;

procedure TFormTestExtensible.RemoveConnectableClick(Sender: TObject);
begin
//  thats all, IConnectable were not needed anymore so remove it
  Extensible.Aggregation.Remove(TConnectableImplementation);
end;

procedure TFormTestExtensible.TestEnumerateClick(Sender: TObject);
var
  Enumerable: IEnumerable;
  Enumerator: IEnumerator;
begin
  try
  // Try to get an enumerator and then use it!
    Enumerable := Self as IEnumerable;
    if Enumerable.GetEnumerator(Enumerator, False) then
      Add('Yes!! I can enumerate') else
      Add('No!! I can''t enumerate');
  except
    Add('Sorry! It appears that it doesn''t support it!! Add it and try again!!')
  end;
end;

procedure TFormTestExtensible.TestConnectClick(Sender: TObject);
begin
// We're connecting the aggregation to ourselves to see
//  if we're supporting IConnectable right now ...
  Sil.Sink.Connect(Self, FExtensible);
end;

procedure TFormTestExtensible.TestDisconnectClick(Sender: TObject);
begin
// We're done here, so disconnect all
  Sil.Sink.Disconnect(Self, FExtensible);
end;

{ TEnumerableImplementation }

constructor TEnumerableImplementation.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
  // Owner is the MainForm as was passed by the Aggregation.Add above
  //  to, take a Pointer to its ITest rather than the ITest itself
  //  to prevent adding a reference
  FOwner := Pointer(Owner as ITest);
  
  // Let us be known in the outside world!!!
  Self.Owner.Add('TEnumerableImplementation.Create');
end;

destructor TEnumerableImplementation.Destroy;
begin
  // We're dying, man!!!
  Self.Owner.Add('TEnumerableImplementation.Destroy');
  FOwner := nil;  // It is really not required. Only a safe-keeping assurance.
  inherited;
end;

function TEnumerableImplementation.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  // Anyone's asking for an Enum ... 
  Self.Owner.Add('TEnumerableImplementation.GetEnumerator');
  Result := False;
end;

function TEnumerableImplementation.DoGetOwner: ITest;
begin
// It is safe to cast directly because we're asked specifically for this interface
  Result := ITest(FOwner);
end;

{ TConnectableImplementation }

constructor TConnectableImplementation.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
  // same as above.
  FOwner := Pointer(Owner as ITest);
  Self.Owner.Add('TConnectableImplementation.Create');
end;

destructor TConnectableImplementation.Destroy;
begin
  Self.Owner.Add('TConnectableImplementation.Destroy');
  FOwner := nil;
  inherited;
end;

procedure TConnectableImplementation.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
// Tell the world that someone connected with us
  Self.Owner.Add('TConnectableImplementation.AddListener');
end;

procedure TConnectableImplementation.RemoveListener(const Listener: IInterface);
begin
// It's really sad when a connection is broken!!!
  Self.Owner.Add('TConnectableImplementation.RemoveListener');
end;

function TConnectableImplementation.DoGetOwner: ITest;
begin
  // Don't panic. Same as above!
  Result := ITest(FOwner);
end;

end.
