unit SilSiGlData;

interface

uses
  Sil,
  SilSiGlTypes;

type
  IGl3dVector = interface
    ['{150DA575-D057-46E7-9719-0F8D27614299}']
    function GetPosition: IGl3dPosition;
    function GetDirection: IGl3dDirection;
    property Position: IGl3dPosition read GetPosition;
    property Direction: IGl3dDirection read GetDirection;
  end;

  IGl3dProjection = interface
    ['{551BEE23-0D5A-45F4-B749-1C42ADB54D18}']
    function
  end;

implementation
end.
 