unit UmOuchProfile;

interface

uses
  Sil, TypInfo, UiOuch;

type
  TOuchProfile = class(
    TSilObject,
    IOuchProfile )
  private
    FData: IXmlTree;
  private 
    function DoFindNode(const Instance: IOuchWindow): IXmlTag;
  protected // IOuchProfile
    procedure Load(const Instance: IOuchWindow);
    procedure Store(const Instance: IOuchWindow);    
    procedure Save(const FileName: string);
  public
    constructor Create(const Data: IXmlTree);
    destructor Destroy; override; 
  end; 

  TOuchProfileFiler = class(TSilObject)
  private
    FOwner: TOuchProfile;
    FNode: IXmlTag;
  public
    constructor Create(Owner: TOuchProfile; const Node: IXmlTag);
    destructor Destroy; override;
  public
    property Owner: TOuchProfile read FOwner;
    property Node: IXmlTag read FNode;
  end;

  TOuchProfileReader = class(
    TOuchProfileFiler,
    IOuchProfileReader )
  protected // IOuchProfileReader
    function ReadProperty(Instance: TObject; const Prop: string): Boolean; overload;
  end;

  TOuchProfileWriter = class(
    TOuchProfileFiler,
    IOuchProfileWriter )
  protected // IOuchProfileWriter
    procedure WriteProperty(Instance: TObject; const Prop: string); overload;
  end;

implementation

uses
  SysUtils, FkOuchBase;

{ TOuchProfile }

constructor TOuchProfile.Create(const Data: IXmlTree);
begin
  inherited Create;
  FData := Data;
end;

destructor TOuchProfile.Destroy;
begin
  FData := nil;
  inherited;
end;

procedure TOuchProfile.Load(const Instance: IOuchWindow);
var
  Reader: IOuchProfileReader;
  Streamable: IOuchStreamable;
begin
  if Supports(Instance, IOuchStreamable, Streamable) then
  begin
    Reader := TOuchProfileReader.Create(Self, DoFindNode(Instance));
    try
      Streamable.Load(Reader);
    finally
      Reader := nil;
    end;
  end;
end;

procedure TOuchProfile.Store(const Instance: IOuchWindow);
var
  Writer: IOuchProfileWriter;
  Streamable: IOuchStreamable;
begin
  if Supports(Instance, IOuchStreamable, Streamable) then
  begin
    Writer := TOuchProfileWriter.Create(Self, DoFindNode(Instance));
    try
      Streamable.Store(Writer);
    finally
      Writer := nil;
    end;
  end;
end;

function TOuchProfile.DoFindNode(const Instance: IOuchWindow): IXmlTag;
begin
  with FData.GetTag('profile/windows', True) do
  begin
    if not AsTag.FindArgument('window', 'id', Instance.Component.ClassName, Result) then
    begin
      Result := Childs.Add(nkTag).AsTag;
      Result.Name := 'window';
      Result.TagKind := tkBlock;
      Result.Arguments.WriteString('id', Instance.Component.ClassName);
    end;
  end;
end;

procedure TOuchProfile.Save(const FileName: string);
begin
  if FData.Modified then
  begin
    Sil.Xml.WriteFile(FData, FileName);
  end;
end;

{ TOuchProfileFiler }

constructor TOuchProfileFiler.Create(Owner: TOuchProfile; const Node: IXmlTag);
begin
  inherited Create;
  FOwner := Owner;
  FNode := Node;
end;

destructor TOuchProfileFiler.Destroy;
begin
  FOwner := nil;
  FNode := nil;
  inherited;
end;

{ TOuchProfileReader }

function TOuchProfileReader.ReadProperty(Instance: TObject; const Prop: string): Boolean; 
var
  Data: IXmlTag;
  Value: string;
begin
  Result := Node.FindArgument('property', 'id', Prop, Data);
  if Result then
  begin
    Value := TypInfo.GetPropValue(Instance, Prop);
    Value := Data.Arguments.ReadString('value', Value, True);
    TypInfo.SetPropValue(Instance, Prop, Value);
  end;
end;

{ TOuchProfileWriter }

procedure TOuchProfileWriter.WriteProperty(Instance: TObject; const Prop: string);
var
  Data: IXmlTag;
  Value: string;
begin
  if Node.FindArgument('property', 'id', Prop, Data, True) then
  begin
    Value := TypInfo.GetPropValue(Instance, Prop);
    Data.Arguments.WriteString('value', Value);
  end;
end;

end.
