(****************************************************************************)
(*                                                                          *)
(*     Standard Interface Library  - Base Library Services                  *)
(*                                                                          *)
(* This demo shows the GlobalServiceList in action.                         *)
(*                                                                          *)
(*  The GlobalServices is a component that the library provides to:         *)
(*                                                                          *)
(* - To prevent the proliferation of global variables holding shared        *)
(*    objects used by many parts of the library.                            *)
(*    As an example we can name:                                            *)
(*    the ThreadList, which holds all the threads spawned by an application *)
(*    and the wrappers of external threads created by the library.          *)
(* - To ease the communication of globals between modules of an application *)
(*     this permits a DLL to register a global services and deregister ir   *)
(*     automatically when it is unloaded despite the fact of being          *)
(*     referenced by some other modules. When this modules want to use the  *)
(*     already acquired services they will note that their pointer to it    *)
(*     has been freed (zeroed out) and will request the service again.      *)
(* - To prevent the use of initialization/finalization blocks inside the    *)
(*   library itself. This prevents unwanted code to being linked and used   *)
(*   behind the scenes (like the VCL does). And allows the linker to cut    *)
(*   out all the unreferenced pieces of code during the linking phase.      *)
(*   In this way, the only code generated is for some finalization blocks   *)
(*   that simply nils out some variables or frees a reference to a global   *)
(*   service.                                                               *)
(*                                                                          *)
(* All global services are registered by some a priori unknown module and   *)
(* any reference to them are resolved without knowing or caring about the   *)
(* location of this service. They are referenced by GUID that are defined   *)
(* when registered. Normaly the code that is responsible for implementing   *)
(* the service can be detached from the code using it. But, for the sake of *)
(* ease of implementation it happens that the same code that needs a service*)
(* is able of providing it by registering it in the same request operation  *)
(* thus allowing to a service of being provided by several sources instead  *)
(* of having to register them before use. This is the case of tha global    *)
(* ThreadList and many more service of the like.                            *)
(*                                                                          *)
(****************************************************************************)

unit FmTestBaseServicesGlobalservice;

{$INCLUDE Defines.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  Dialogs, StdCtrls,

  Sil,
  SilTool,
  SilVCL,
  SilUtDll;

type
  // just some useful interface
  IDll = interface
    ['{7D8BC991-9A59-4FEE-8C02-85BF68BDE4C0}']
    function ID: string;
    function Lib: ISharedLibrary;
    function Module: IModule2;
  end;

  TFormTestGlobalservices = class(
    TForm,
    IRunnable,
    IGlobalServicesHookV2)
    GetV1: TButton;
    RegisterV1: TButton;
    UnregisterV1: TButton;
    RegisterV2: TButton;
    UnregisterV2: TButton;
    GetV2: TButton;
    ReleaseV1: TButton;
    ReleaseV2: TButton;
    EnumV1: TButton;
    EnumV2: TButton;
    RefsV1: TButton;
    RefsV2: TButton;
    pnButtons: TPanel;
    pnModule: TPanel;
    Panel1: TPanel;
    pnModules: TPanel;
    Panel2: TPanel;
    LoadDll: TButton;
    UnloadDll: TButton;
    edDll: TEdit;
    Spawn: TButton;
    Stop: TButton;
    Label1: TLabel;
    Panel3: TPanel;
    lvDlls: TListView;
    Panel4: TPanel;
    Panel5: TPanel;
    lvGlobalServices: TListView;
    Panel6: TPanel;
    Panel7: TPanel;
    lvLocalServices: TListView;
    Panel8: TPanel;
    Panel9: TPanel;
    lvRefs: TListView;
    Panel10: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    procedure LoadDllClick(Sender: TObject);
    procedure UnloadDllClick(Sender: TObject);
    procedure SpawnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnumV1Click(Sender: TObject);
    procedure GetV1Click(Sender: TObject);
    procedure RegisterV1Click(Sender: TObject);
    procedure UnregisterV1Click(Sender: TObject);
    procedure RegisterV2Click(Sender: TObject);
    procedure UnregisterV2Click(Sender: TObject);
    procedure GetV2Click(Sender: TObject);
    procedure ReleaseV1Click(Sender: TObject);
    procedure ReleaseV2Click(Sender: TObject);
    procedure EnumV2Click(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure lvServicesDeletion(Sender: TObject; Item: TListItem);
    procedure lvDllsDeletion(Sender: TObject; Item: TListItem);
    procedure lvServicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvDllsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FDlls: IStringList;
    FEvent: IEvent;
    FThread: IThread;
    FInstance: IUnknown;
  private
    procedure DoLoadDlls;
    procedure DoRefreshAll(const Module: IModule2);
    procedure DoRefresh(const List: IGlobalServiceListV2; View: TListView); overload;
    procedure DoRefresh(const List: IGlobalServicesV1; View: TListView); overload;
    procedure DoAdd(const Obj: IGlobalService; View: TListView); overload;
    procedure DoAdd(const Obj: GlobalServiceType; View: TListView); overload;
    procedure DoRemove(const Obj: IGlobalService; View: TListView);
    procedure DoUpdate(const Obj: IGlobalService; View: TListView);
    procedure DoLoadRefs(const Obj: IGlobalService);
    function DoGetDll(Item: TListItem; out Dll: IDll): Boolean;
    function DoGetProc(Item: TListItem; out Proc): Boolean;
  protected // IGlobalServicesHookV2
    procedure ServiceInitialize(const Service: IGlobalServiceV2);
    procedure ServiceFinalize(const Service: IGlobalServiceV2);
    procedure ServiceCreate(const Service: IGlobalServiceV2);
    procedure ServiceDestroy(const Service: IGlobalServiceV2);
    procedure ServiceAddRef(const Service: IGlobalServiceV2; Ref: PUnknown);
    procedure ServiceDropRef(const Service: IGlobalServiceV2; Ref: PUnknown);
    procedure ServiceLink(const Service: IGlobalServiceV2; const Link: IGlobalServiceV2);
    procedure ServiceUnlink(const Service: IGlobalServiceV2; const Link: IGlobalServiceV2);
  protected //-
    procedure Run(const Thread: IThread);
  public
  end;

  // this object will hols a loaded DLL
  //  its only purpouse is to live within an
  //  interface list and unload the DLL on
  //  destruction
  TDll = class(
    TSilObject,
    IDll )
  private
    FLibrary: ISharedLibrary;
    FModule: IModule2;
  protected // IDll
    function ID: string;
    function Lib: ISharedLibrary;
    function Module: IModule2; 
  public
    constructor Create(const Dll: ISharedLibrary);
    destructor Destroy; override;
  end;

var
  FormTestGlobalservices: TFormTestGlobalservices;

implementation

uses
  UfDemoService,
  SilScSharedSignature,
  SilShSharedObject;

{$R *.dfm}

function Spawn(const Runnable: IRunnable): IThread; stdcall;
begin
  Result := Sil.Os.Thread.Spawn('EXE', Runnable);
end;

exports
  Spawn;
  
{ TDll }

constructor TDll.Create(const Dll: ISharedLibrary);
begin
  inherited Create;
  FLibrary := Dll;
  // This will load a IModule2 from the Handle of the DLL loaded
  //  we will use this to get the DLL info, such as name, version, size, path, etc. 
  FModule :=  Sil.Os.Module.Get(FLibrary.Handle.Value) as IModule2;
end;

destructor TDll.Destroy;
begin
  FModule := nil;
  FLibrary := nil;
  inherited;
end;

function TDll.Lib: ISharedLibrary;
begin
  Result := FLibrary;
end;

function TDll.ID: string;
begin
  // The ID of the object is simply the hex value of the handle translated to string
  Result := Sil.Int.ToHex(FLibrary.Handle.Value, 8);
end;

function TDll.Module: IModule2;
begin
  Result := FModule;
end;

{ TForm1 }

procedure TFormTestGlobalservices.FormCreate(Sender: TObject);
begin
  // create the list to hold the loaded dynamic modules
  FDlls := Sil.List.StringList(False, InterfaceHandler);
  // connect to global list to receive the service notification: see IGlobalServicesHookV2 above
  Sil.Sink.Connect(Sil.Global.List, Self);
  // create a Event object to synchronize the threads spawned here and terminate them when signaling it
  FEvent := Sil.Os.IPC.Event();
  // Iterate through the list of loaded modules and find the ones having the signature mark.
  DoLoadDlls;
  // Now refresh the services already in the global list
  DoRefreshAll(Sil.Os.Module.Current as IModule2);
end;

procedure TFormTestGlobalservices.FormDestroy(Sender: TObject);
begin
  // destroy de Event object
  FEvent := nil;
  // disconnect from the global list
  Sil.Sink.Disconnect(Sil.Global.List, Self);
  // clean up the interface list which holds the DLLs
  FDlls := nil;
end;

procedure TFormTestGlobalservices.DoLoadDlls;
// Iterates the loaded modules.
//  for each of them:
//    check if it have the marking signature.
//      if does have it:
//          add it to the list of modules.
var
  Enum: IEnumerator;
  Item: IModule2;
  Signature: TSilSignature;
  X: Integer;
begin
  lvDlls.Clear;

  with Sil.Os.Process.Current.Modules do  // request the list of loaded modules by our process
    while Enumerate(Enum, Item) do  // for each of them
      if Sil.OS.SharedLibrary.GetAddress(CSilLibrarySignatureName, Signature, Item.Handle.Value) then
        // does it have the marking signature, then add it to the list
        with lvDlls.Items.Add do
        begin
          Caption := Sil.Int.ToHex(Item.Handle.Value, 8);
          Subitems.Add(Item.Info.Name);
          Subitems.Add(Item.Info.Path);
          X := FDlls.IndexOf(Caption);
          if X <> -1 then Data := Sil.Ref.AddRef(IUnknown(FDlls.Ptrs[X]));
          Subitems.Add(Sil.Str.Iif(Data = nil, 'S', 'D'));
        end;
end;

procedure TFormTestGlobalservices.DoRefreshAll(const Module: IModule2);
// Refresh the list of global services
//  then get the local services of the module passed by argument
//    if we can get the list then:
//      Refresh the services V2 provided by it
//      Refresh the services V1 provided by it
var
  Services: RServices;
begin
  DoRefresh(Sil.Global.ListV2, lvGlobalServices);
  if GetServices(Services, Module.Handle.Value, skLocal, True) then
  try
    if Assigned(Services.V2) then DoRefresh(Services.V2, lvLocalServices) else
    if Assigned(Services.V1) then DoRefresh(Services.V1, lvLocalServices);
  finally
    Finalize(Services);
  end;
end;

procedure TFormTestGlobalservices.DoRefresh(const List: IGlobalServiceListV2; View: TListView);
// Refresh the list (V2) of global services on the list view taken as argument
var
  Enum: IEnumerator;
  Item: IGlobalService;
begin
  View.Clear;
  with List do
    while Enumerate(Enum, Item) do
      DoAdd(Item, View);
end;

procedure TFormTestGlobalservices.DoRefresh(const List: IGlobalServicesV1; View: TListView);
// Refresh the list (V1) of global services on the list view taken as argument
var
  Enum: IEnumerator;
  Item: GlobalServiceType;
begin
  View.Clear;
  with List do
    while Enumerate(Enum, Item) do
      DoAdd(Item, View);
end;

procedure TFormTestGlobalservices.DoAdd(const Obj: IGlobalService; View: TListView);
// Adds the Service information to the list passed as argument 
var
  Item: TListItem;
begin
  Item := View.Items.Add;
  Item.Caption := Obj.Service.Name^;
  Item.Subitems.Add(Sil.Guid.ToStr(Obj.Service.ID^));
  Item.Subitems.Add(Sil.Os.Module.Get(Obj.Service.Module).Name);
  Item.Subitems.Add(Sil.Enum.Name(TypeInfo(TGlobalServiceKind), Ord(Obj.Kind), 'sk'));
  Item.Subitems.Add(Sil.Str.Iif(Obj.IsCreated, 'True', 'False'));
  Item.Subitems.Add(Sil.Int.ToStr(Obj.References.Count));
  Item.Data := Pointer(Obj);
end;

procedure TFormTestGlobalservices.DoAdd(const Obj: GlobalServiceType; View: TListView);
// Adds the Service information to the list passed as argument 
var
  Item: TListItem;
begin
  Item := View.Items.Add;
  Item.Caption := Obj.ClassName;
  Item.Subitems.Add(Sil.Guid.ToStr(Obj.ID));
  Item.Subitems.Add(Sil.Os.Module.Get(Obj).Name);
  Item.Data := Pointer(Obj);
end;

procedure TFormTestGlobalservices.DoRemove(const Obj: IGlobalService; View: TListView);
// Finds a service V2 in the List view and removes the item which holds him
var
  Item: TListItem;
begin
  Item := View.FindData(0, Pointer(Obj), True, False);
  if Assigned(Item) then Item.Delete;
end;

procedure TFormTestGlobalservices.DoUpdate(const Obj: IGlobalService; View: TListView);
// Finds a service V1 in the List view and removes the item which holds him
var
  Item: TListItem;
begin
  Item := View.FindData(0, Pointer(Obj), True, False);
  Item.Subitems[3] := Sil.Str.Iif(Obj.IsCreated, 'True', 'False');
  Item.Subitems[4] := Sil.Int.ToStr(Obj.References.Count);
  Item.Data := Pointer(Obj);
end;

procedure TFormTestGlobalservices.DoLoadRefs(const Obj: IGlobalService);
// Enumerates the references to the service Obj.
//  for each reference:
//    add an item in the list view holding their information
var
  Enum: IEnumerator;
  Ref: RGlobalRef;
begin
  lvRefs.Clear;
  with Obj.References do
    while Enumerate(Enum, Ref) do
      with lvRefs.Items.Add do
      begin
        Caption := Sil.Int.ToHex(Integer(Ref.Ref), 8);
        Subitems.Add(Sil.Os.Module.Name(Ref.Module));
      end;
end;

function TFormTestGlobalservices.DoGetDll(Item: TListItem; out Dll: IDll): Boolean;
// tries to get the Dll pointed by a List Item
begin
  Result := Assigned(Item) and Sil.Ref.Extract(Item.Data, IDll, Dll);
end;

function TFormTestGlobalservices.DoGetProc(Item: TListItem; out Proc): Boolean;
// tries to get entry point exported by the DLL pointed out by the List item
var
  Dll: IDll;
begin
  Result := DoGetDll(Item, Dll);
  if Result then
    Result := Dll.Lib.Bind('Spawn', 0, Proc) else
    Result := Assigned(Item) and GetProcAddress('Spawn', Proc, Sil.Hex.ToInt(Item.Caption));
end;

procedure TFormTestGlobalservices.RegisterV1Click(Sender: TObject);
// registers the DemoService using the V1 API
begin
  Sil.Global.ServicesV1.Register(Demo1);
end;

procedure TFormTestGlobalservices.UnregisterV1Click(Sender: TObject);
// unregisters the DemoService using the V1 API
begin
  Sil.Global.ServicesV1.Unregister(Demo1);
end;

procedure TFormTestGlobalservices.RegisterV2Click(Sender: TObject);
// registers the DemoService using the V2 API
begin
  Sil.Global.ListV2.Register(Demo2, HInstance);
end;

procedure TFormTestGlobalservices.UnregisterV2Click(Sender: TObject);
// unregisters the DemoService using the V2 API
begin
  Sil.Global.ListV2.Unregister(Demo2, HInstance);
end;

procedure TFormTestGlobalservices.GetV1Click(Sender: TObject);
// tries to get an instance to the Demoservice using the V1 API
begin
  Sil.Global.ServicesV1.Get(Demo1, IUnknown, @FInstance, ClassType);
end;

procedure TFormTestGlobalservices.ReleaseV1Click(Sender: TObject);
// releases the instance to the Demoservice using the V1 API
begin
  Sil.Global.ServicesV1.Release(Demo1, @FInstance);
end;

procedure TFormTestGlobalservices.GetV2Click(Sender: TObject);
// tries to get an instance to the Demoservice using the V2 API
begin
  Sil.Global.ListV2.Get(Demo2, IUnknown, @FInstance, HInstance);
end;

procedure TFormTestGlobalservices.ReleaseV2Click(Sender: TObject);
// releases the instance to the Demoservice using the V2 API
begin
  Sil.Global.ListV2.Release(Demo2, @FInstance, HInstance);
end;                        

procedure TFormTestGlobalservices.EnumV1Click(Sender: TObject);
// enumerates the global & local services using the V1 API
begin
  DoRefresh(Sil.Global.ServicesV1, lvGlobalServices);
  DoRefresh(Sil.Local.ServicesV1, lvLocalServices);
end;

procedure TFormTestGlobalservices.EnumV2Click(Sender: TObject);
// enumerates the global & local services using the V2 API
begin
  DoRefreshAll(Sil.Os.Module.Current as IModule2);
end;

procedure TFormTestGlobalservices.LoadDllClick(Sender: TObject);
// Tries to load the DLL specified by the edDLL edit box
var
  Dll: IDll;
begin
  Dll := TDll.Create(Sil.Os.SharedLibrary.Load(edDll.Text));
  FDlls.Add(Dll.ID, @Dll);
  DoLoadDlls;
end;

procedure TFormTestGlobalservices.UnloadDllClick(Sender: TObject);
// Unloads the DLL pointed by the selected item of the modules list view.
var
  Dll: IDll;
  X: Integer;
begin
  with lvDlls do
    if Assigned(Selected) then
      if Assigned(Selected.Data) then
      begin
        if Sil.Ref.Extract(Selected.Data, IDll, Dll) then
        begin
          X := FDlls.IndexOfPtr(@Dll);
          if X <> -1 then
            FDlls.Delete(X);
        end;
        Selected.Delete;
      end;
end;

procedure TFormTestGlobalservices.SpawnClick(Sender: TObject);
// Tries to get a reference to the exported spawn function
//  and if found the procedes to call it.
var
  Spawn: function(const Runnable: IRunnable): IThread; stdcall;
begin
  with lvDlls do
    if DoGetProc(Selected, Spawn) then
    begin
      FEvent.Reset;
      FThread := Spawn(Self);
    end;
end;

procedure TFormTestGlobalservices.StopClick(Sender: TObject);
// Signals the Event to stop the execution of the spawned thread
var
  Termination: IWaitable;
begin
  if Assigned(FThread) then
  begin
    Termination := FThread.Termination;
    try
      FEvent.Signal;
      Termination.WaitFor(INFINITE, True);
    finally
      Termination := nil;
      FThread := nil;
    end;
  end;
end;

procedure TFormTestGlobalservices.Run(const Thread: IThread);
// This is the thread's main loop: here we just wait that someone signals the event. 
begin
  VCL.SetProp.Value(Self, 'Caption', 'waiting ...');
  FEvent.WaitFor(INFINITE);
  VCL.SetProp.Value(Self, 'Caption', 'finished');
end;

procedure TFormTestGlobalservices.lvServicesDeletion(Sender: TObject; Item: TListItem);
// Deletion of an item from the any of the Services Lists
begin
  Item.Data := nil;
end;

procedure TFormTestGlobalservices.lvDllsDeletion(Sender: TObject; Item: TListItem);
// Deletion of an item from modules list
begin
  if Assigned(Item.Data) then
    Item.Data := Sil.Ref.Release(Item.Data);
end;

procedure TFormTestGlobalservices.lvServicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
// An item from the any of the services lists was selected so, try to refresh the information. 
var
  Service: IGlobalServiceV2;
begin
  if Selected and Assigned(Item.Data) and Sil.Ref.Extract(Item.Data, IGlobalServiceV2, Service) then
    DoLoadRefs(Service);
end;

procedure TFormTestGlobalservices.lvDllsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
// An item from the modules list was selected so, try to refresh the information. 
var
  Handle: LongWord;
begin
  if Selected then
  begin
    Handle := Sil.Hex.ToInt(Item.Caption);
    DoRefreshAll(Sil.Os.Module.Get(Handle) as IModule2);
  end;
end;

procedure TFormTestGlobalservices.ServiceInitialize(const Service: IGlobalServiceV2);
// Hook invoked by the services runtime to signal the registration of a new service 
begin
  case Service.Kind of
    skGlobal: DoAdd(Service, lvGlobalServices);
    skLocal: DoAdd(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceFinalize(const Service: IGlobalServiceV2);
// Hook invoked by the services runtime to signal the deregistration of a service 
begin
  case Service.Kind of
    skGlobal: DoRemove(Service, lvGlobalServices);
    skLocal: DoRemove(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceCreate(const Service: IGlobalServiceV2);
// Hook invoked by the services runtime to signal the instantation of a service 
begin
  case Service.Kind of
    skGlobal: DoUpdate(Service, lvGlobalServices);
    skLocal: DoUpdate(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceDestroy(const Service: IGlobalServiceV2);
// Hook invoked by the services runtime to signal the destruction of a service 
begin
  case Service.Kind of
    skGlobal: DoUpdate(Service, lvGlobalServices);
    skLocal: DoUpdate(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceAddRef(const Service: IGlobalServiceV2; Ref: PUnknown);
// Hook invoked by the services runtime to signal creation of new service reference 
begin
  case Service.Kind of
    skGlobal: DoUpdate(Service, lvGlobalServices);
    skLocal: DoUpdate(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceDropRef(const Service: IGlobalServiceV2; Ref: PUnknown);
// Hook invoked by the services runtime to signal destruction of new service reference 
begin
  case Service.Kind of
    skGlobal: DoUpdate(Service, lvGlobalServices);
    skLocal: DoUpdate(Service, lvLocalServices);
  end;
end;

procedure TFormTestGlobalservices.ServiceLink(const Service, Link: IGlobalServiceV2);
begin
end;

procedure TFormTestGlobalservices.ServiceUnlink(const Service, Link: IGlobalServiceV2);
begin
end;

end.
