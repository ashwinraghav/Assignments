unit SilSmModel;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiModel;

type
  TSilModel = class(
    TSilObject,
    IModel )
  private
    FOwner: TObject;
    FTags: ITagList;
    FEntities: IEntityList;
    FRules: IRuleList;
  protected // IModel
    function GetOwner: TObject;
    function GetEntities: IEntityList;
    function GetRules: IRuleList;
    function GetTags: ITagList;
  public
    constructor Create(Owner: TObject = nil);
    destructor Destroy; override;
  end;

implementation

uses
  SilSmTags, SilSmEntities, SilSmRules;

{ TSilModel }

constructor TSilModel.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
  FTags := TSilTagList.Create('globals', Owner);
  FEntities := TSilEntityList.Create(Self, Owner);
  FRules := TSilRuleList.Create(Self, nil, Owner); 
end;

destructor TSilModel.Destroy;
begin
  FRules := nil;
  FEntities := nil;
  FTags := nil;
  FOwner := nil;
  inherited;
end;

function TSilModel.GetEntities: IEntityList;
begin
  Result := FEntities;
end;

function TSilModel.GetOwner: TObject;
begin
  Result := FOwner;
end;

function TSilModel.GetRules: IRuleList;
begin
  Result := FRules;
end;

function TSilModel.GetTags: ITagList;
begin
  Result := FTags;
end;

end.
 