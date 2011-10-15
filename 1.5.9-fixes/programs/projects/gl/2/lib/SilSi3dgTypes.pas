unit SilSiGlTypes;

interface

uses
  Sil,
  SilSeGlTypes;

type
  IGlPosition = interface
    ['{72E7DB89-B59C-44B0-8050-6EA6B151A798}']
    function GetValue: P3dPoint;
    property Value: P3dPoint read GetValue;
  end;

  IGlDirection = interface
    ['{9463CB76-A10B-4BC3-A1AE-DE08696A3D6B}']
    function GetValue: P3dVector;
    property Value: P3dVector read GetValue;
  end;

  IGl

implementation
end.
 