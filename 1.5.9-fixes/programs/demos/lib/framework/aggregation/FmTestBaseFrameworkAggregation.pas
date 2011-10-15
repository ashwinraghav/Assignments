(***************************************************************************)
(*                                                                         *)
(*     Standard Interface Library  - Application Framework Services        *)
(*                                                                         *)
(* This is a demonstration of how we use a TSilAggregationObject.          *)
(*                                                                         *)
(*    In this program we show how we can "extend" an object services with  *)
(*  extra interfaces provided by aggregating extension objects to          *)
(*  the main object.                                                       *)
(*                                                                         *)
(*    We provide a helper object to contain the extensions and to allow us *)
(*  to query for interfaces implemented by them in a simple way.           *)
(***************************************************************************)

unit FmTestBaseFrameworkAggregation;

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

  // This is the main form. We use it as a controller object, the one that we want to "extend"
  //    with arbitrary interfaces.
  TFormTestAggregation = class(
    TForm,
    ITest // implementing the ITest for the aggregation extensions
    )
    Lock: TButton;
    Memo: TMemo;
    AddLockable: TButton;
    RemoveLockable: TButton;
    AddConnectable: TButton;
    RemoveConnectable: TButton;
    Connect: TButton;
    Disconnect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LockClick(Sender: TObject);
    procedure AddLockableClick(Sender: TObject);
    procedure RemoveLockableClick(Sender: TObject);
    procedure AddConnectableClick(Sender: TObject);
    procedure RemoveConnectableClick(Sender: TObject);
    procedure ConnectClick(Sender: TObject);
    procedure DisconnectClick(Sender: TObject);
  private
    // This holds the main IUnknown of the aggregation object.
    //  It is *critical* that we keep its main IUnknown instead of
    //  its IAggregation's IUnknown, because we are aggregating ourselves to
    //  it, that is, if were called QueryInterface for IAggregation, and keeped it
    //  intead of its main IUnknown, then it were called _AddRef on our IUnknown
    //  so it we're going to be destroyed at end of scope with us
    //  holding a pointer to our IUnknown!
    FAggregation: IUnknown;
  private
    function DoGetAggregation: IAggregation;
  protected // ITest
    //  this simply writes out the Text to the Memo
    procedure Add(const Text: string);
  protected
    // we override QueryInterface to delegate the interfaces that we don't know
    //  to the aggregation object
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  protected
    // helper property to get the IAggregation interface
    property Aggregation: IAggregation read DoGetAggregation;
  end;

var
  FormTestAggregation: TFormTestAggregation;

implementation

{$R *.dfm}

type
  // this is a lockable extension that simulates the locking
  //  semantics of a critical section.
  //  
  TLockableImplementation = class(
    TSilAggregableObject,
    ILockable )
  private
    // get hold of our owner.
    //  But be extracarefull!!
    //  Do not keep references to it!!!
    //  So, we're going to keep it as a plain pointer
    FOwner: Pointer;
  private 
    // helper to obtain the ITest interface from our owner
    function DoGetOwner: ITest;
  protected // implement ILockable
    // simulate locking
    procedure Lock;
    // simulate unlocking
    procedure Unlock;
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

{ TFormTestAggregation1 }

procedure TFormTestAggregation.FormCreate(Sender: TObject);
begin
  // creates the Aggregation object passing Self as Controller
  //  bear in mind that the IUnknown retrieved here is the main
  //  IUnknown of the aggregation object.
  FAggregation := TSilAggregationObject.Create(Self);
end;

procedure TFormTestAggregation.FormDestroy(Sender: TObject);
begin
  // Releasing its main IUnknown will cause its destruction.
  FAggregation := nil;
end;

procedure TFormTestAggregation.Add(const Text: string);
begin
  // Put the text in the memo!
  Memo.Lines.Add(Text);
end;

function TFormTestAggregation.DoGetAggregation: IAggregation;
begin
  // casting the IUnknown we hold to obtain the interface we want is an
  //  expensive solution, so we use here only because this is a demo!!
  Result := FAggregation as IAggregation;
end;

function TFormTestAggregation.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // First, try to resolve the queried interface by ourselves
  Result := inherited QueryInterface(IID, Obj);
  if Result <> 0 then
    // ok, if we don't know the interface lets try to get it from the aggregation
    Result := FAggregation.QueryInterface(IID, Obj);
end;

procedure TFormTestAggregation.AddLockableClick(Sender: TObject);
begin
// extend our services by adding an ILockable implementation
//  note the second argument: this is the Owner for the extension object
  Aggregation.Add(TLockableImplementation, Self);
end;

procedure TFormTestAggregation.RemoveLockableClick(Sender: TObject);
begin
// thats all, ILockable were not needed anymore so remove it
  Aggregation.Remove(TLockableImplementation);
end;

procedure TFormTestAggregation.AddConnectableClick(Sender: TObject);
begin
// extend our services by adding an IConnectable implementation
//  note the second argument: this is the Owner for the extension object
  Aggregation.Add(TConnectableImplementation, Self);
end;

procedure TFormTestAggregation.RemoveConnectableClick(Sender: TObject);
begin
//  thats all, IConnectable were not needed anymore so remove it
  Aggregation.Remove(TConnectableImplementation);
end;

procedure TFormTestAggregation.LockClick(Sender: TObject);
var
  Lock: ILock;
begin
// Try to lock ourselves to see if someone supports the ILockable out there
  Lock := Sil.Lock.Take(Self, True);
  if Assigned(Lock) then
    // So, there's has to be an extension out there!
    Add('TFormTestAggregation1.QueryLockableClick: Locked') else
    // Don't worry, add an extension and try again!
    Add('TFormTestAggregation1.QueryLockableClick: Lock failed');
end;

procedure TFormTestAggregation.ConnectClick(Sender: TObject);
begin
// We're connecting the aggregation to ourselves to see
//  if we're supporting IConnectable right now ...
  Sil.Sink.Connect(Self, FAggregation);
end;

procedure TFormTestAggregation.DisconnectClick(Sender: TObject);
begin
// We're done here, so disconnect all
  Sil.Sink.Disconnect(Self, FAggregation);
end;

{ TLockableImplementation }

constructor TLockableImplementation.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
  // Owner is the MainForm as was passed by the Aggregation.Add above
  //  to, take a Pointer to its ITest rather than the ITest itself
  //  to prevent adding a reference
  FOwner := Pointer(Owner as ITest);

  // Let us be known in the outside world!!!
  Self.Owner.Add('TLockableImplementation.Create');
end;

destructor TLockableImplementation.Destroy;
begin
  // We're dying, man!!!
  Self.Owner.Add('TLockableImplementation.Destroy');
  // It is really not required. Only a safe-keeping assurance.
  FOwner := nil;
  inherited;
end;

procedure TLockableImplementation.Lock;
begin
// We were lockdown!! Say this to the public!
  Owner.Add('TLockableImplementation.Lock: LOCK');
end;

procedure TLockableImplementation.Unlock;
begin
// Unlocked!! Say this to the public!
  Owner.Add('TLockableImplementation.Unlock: UNLOCK');
end;

function TLockableImplementation.DoGetOwner: ITest;
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
