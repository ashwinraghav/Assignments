unit FmSchema;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Sil, SilSiFirebird;

type
  TFormSchema = class;

  INodeLoader = interface
    ['{F59F6A6B-4E2D-4FDB-BF99-86106894FB17}']
    procedure LoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
  end;

  TFormSchema = class(TForm)
    vwSchema: TTreeView;
    procedure vwSchemaExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure vwSchemaDeletion(Sender: TObject; Node: TTreeNode);
    procedure vwSchemaCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
  private
    FCounter: IPerformanceCounter;
    FSession: IFbSession;
  private
    procedure DoLoadSchema;
    procedure DoLoadProcedures;
  public
    constructor Create(const Session: IFbSession); reintroduce;
    destructor Destroy; override;
  public
    property Counter: IPerformanceCounter read FCounter;
    property Session: IFbSession read FSession;
  end;

  TLoadCursor = class(
    TSilObject,
    INodeLoader )
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); virtual; abstract;
  protected
    procedure LoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
  end;

  TLoadDomains = class(TLoadCursor)
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); override; 
  end;

  TLoadProcedures = class(TLoadCursor)
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); override;
  end;

  TLoadTables = class(TLoadCursor)
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); override;
  end;

  TLoadViews = class(TLoadCursor)
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); override;
  end;

  TLoadProcedureParameters = class(TLoadCursor)
  protected // INodeLoader
    procedure DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean); override;
  end;
  
var
  FormSchema: TFormSchema;

implementation

uses
  SilSgFirebirdSchemaQueries, SilScFirebirdSQL, SilSfFirebirdSQL,
  SilSfFirebird, SilBtFloat;

{$R *.dfm}

{ TFormSchema }

constructor TFormSchema.Create(const Session: IFbSession);
begin
  inherited Create(nil);
  FCounter := Sil.Os.Performance.Create;
  FSession := Session;
  DoLoadSchema;
end;

destructor TFormSchema.Destroy;
begin
  FSession := nil;
  FCounter := nil;
  inherited;
end;

procedure TFormSchema.DoLoadSchema;
begin
  DoLoadProcedures;
end;

procedure TFormSchema.DoLoadProcedures;
var
  Item: TTreeNode;
begin
  Item := vwSchema.Items.Add(nil, 'domains');
  Item.Data := Sil.Ref.AddRef(TLoadDomains.Create() as INodeLoader);;
  vwSchema.Items.AddChild(Item, 'cargando ...');
  Item := vwSchema.Items.Add(nil, 'procedures');
  Item.Data := Sil.Ref.AddRef(TLoadProcedures.Create() as INodeLoader);;
  vwSchema.Items.AddChild(Item, 'cargando ...');
  Item := vwSchema.Items.Add(nil, 'tables');
  Item.Data := Sil.Ref.AddRef(TLoadTables.Create() as INodeLoader);;
  vwSchema.Items.AddChild(Item, 'cargando ...');
  Item := vwSchema.Items.Add(nil, 'views');
  Item.Data := Sil.Ref.AddRef(TLoadViews.Create() as INodeLoader);;
  vwSchema.Items.AddChild(Item, 'cargando ...');
end;

procedure TFormSchema.vwSchemaDeletion(Sender: TObject; Node: TTreeNode);
begin
  Sil.Ref.Release(Node.Data);
end;

procedure TFormSchema.vwSchemaCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
var
  Loader: INodeLoader;
begin
  if Sil.Ref.Extract(Node.Data, INodeLoader, Loader) then
  begin
    Node.DeleteChildren;
    vwSchema.Items.AddChild(Node, 'cargando ...');
  end;
end;

procedure TFormSchema.vwSchemaExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  Loader: INodeLoader;
begin
  if Sil.Ref.Extract(Node.Data, INodeLoader, Loader) then
    Loader.LoadChilds(Self, Node, AllowExpansion);
end;

{ TLoadCursor }

procedure TLoadCursor.LoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
begin
  Owner.Counter.Reset;
  try
    DoLoadChilds(Owner, Node, Expand);
  finally
    Owner.Caption := Sil.Float.ToStr(Owner.Counter.ToSeconds());
  end;
end;                                        

{ TLoadDomains }

procedure TLoadDomains.DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
var
  Enum: IEnumerator;
  Domains: IFbSchemaDomains;
  Domain: IFbSchemaDomain;
begin
  try
    Domains := Owner.Session.Database.Schema.Domains(
        nil,
        SQLCond(CSysFieldRelationSystemFlag, SQLParam(CSysFieldRelationSystemFlag)),
        Params([Param(CSysFieldRelationSystemFlag, 0)]),
        CSysFieldDomainName);
    Node.DeleteChildren;
    try
      with Domains do
        while Enumerate(Enum, Domain) do
        try
          Owner.vwSchema.Items.AddChild(Node, Domain.Name.AnsiString.Value);
        finally
          Domain := nil;
        end;
    finally
      Domains := nil;
    end;
  except on Ex: Exception do
    begin
      ShowMessage(Ex.Message);
      Expand := False;
    end;
  end;
end;

{ TLoadTables }

procedure TLoadTables.DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
var
  Item: TTreeNode;
  Enum: IEnumerator;
  Tables: IFbSchemaTables;
  Table: IFbSchemaTable;
begin
  try
    Tables := Owner.Session.Database.Schema.Tables(
        nil,
        SQLCond(CSysFieldRelationSystemFlag, SQLParam(CSysFieldRelationSystemFlag)),
        Params([Param(CSysFieldRelationSystemFlag, 0)]),
        CSysFieldRelationName);
    Node.DeleteChildren;
    try
      with Tables do
        while Enumerate(Enum, Table) do
        try
          Item := Owner.vwSchema.Items.AddChild(Node, Table.Name.AnsiString.Value);
          Owner.vwSchema.Items.AddChild(Item, 'fields');
          Owner.vwSchema.Items.AddChild(Item, 'indexes');
          Owner.vwSchema.Items.AddChild(Item, 'triggers');
        finally
          Table := nil;
        end;
    finally
      Tables := nil;
    end;
  except on Ex: Exception do
    begin
      ShowMessage(Ex.Message);
      Expand := False;
    end;
  end;
end;

{ TLoadViews }

procedure TLoadViews.DoLoadChilds(const Owner: TFormSchema;
  const Node: TTreeNode; var Expand: Boolean);
var
  Item: TTreeNode;
  Enum: IEnumerator;
  Views: IFbSchemaViews;
  View: IFbSchemaView;
begin
  try
    Views := Owner.Session.Database.Schema.Views(
        nil,
        SQLCond(CSysFieldRelationSystemFlag, SQLParam(CSysFieldRelationSystemFlag)),
        Params([Param(CSysFieldRelationSystemFlag, 0)]),
        CSysFieldRelationName);
    Node.DeleteChildren;
    try
      with Views do
        while Enumerate(Enum, View) do
        try
          Item := Owner.vwSchema.Items.AddChild(Node, View.Name.AnsiString.Value);
          Owner.vwSchema.Items.AddChild(Item, 'fields');
        finally
          View := nil;
        end;
    finally
      Views := nil;
    end;
  except on Ex: Exception do
    begin
      ShowMessage(Ex.Message);
      Expand := False;
    end;
  end;
end;

{ TLoadProcedures }

procedure TLoadProcedures.DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
var
  Item: TTreeNode;
  Enum: IEnumerator;
  Procs: IFbSchemaProcedures;
  Proc: IFbSchemaProcedure;
begin
  try
    Procs := Owner.Session.Database.Schema.Procedures(nil, '', nil, CSysFieldProcedureName);
    Node.DeleteChildren;
    try
      with Procs do
        while Enumerate(Enum, Proc) do
        try
          Item := Owner.vwSchema.Items.AddChild(Node, Proc.Name.AnsiString.Value);
          Item.Expanded := True;
          Item := Owner.vwSchema.Items.AddChild(Item, 'Parameters');
          Item.Data := Sil.Ref.AddRef(TLoadProcedureParameters.Create() as INodeLoader);
          Owner.vwSchema.Items.AddChild(Item, 'cargando ...');
        finally
          Proc := nil;
        end;
    finally
      Procs := nil;
    end;
  except on Ex: Exception do
    begin
      ShowMessage(Ex.Message);
      Expand := False;
    end;
  end;
end;

{ TLoadProcedureParameters }

procedure TLoadProcedureParameters.DoLoadChilds(const Owner: TFormSchema; const Node: TTreeNode; var Expand: Boolean);
var
  Enum: IEnumerator;
  Params: IFbSchemaProcedureParameters;
  Param: IFbSchemaProcedureParameter;
begin
  try
    Params := Owner.Session.Database.Schema.Parameters(Node.Parent.Text);
    Node.DeleteChildren;
    try
      with Params do
        while Enumerate(Enum, Param) do
        try
          Owner.vwSchema.Items.AddChild(Node, Param.Name.AnsiString.Value);
        finally
          Param := nil;
        end;
    finally
      Params := nil;
    end;
  except on Ex: Exception do
    begin
      ShowMessage(Ex.Message);
      Expand := False;
    end;    
  end;
end;

end.
