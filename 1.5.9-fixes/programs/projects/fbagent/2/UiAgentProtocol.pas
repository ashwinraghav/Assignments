unit UiAgentProtocol;

interface

{$include Defines.inc}

uses
  Sil;

type
  IAgentProtocolServer = interface
    ['{407BA49D-8426-4AE4-8575-8B9D8797FB67}']
  end;

  IAgentProtocolClient = interface
    ['{FB2CE309-45B7-444F-A73A-657F4D34FF9B}']
    function Login(const User, Password: String): Boolean;
    function Configure(const Parameters: IParameters): Boolean;
    function StartJob(JobId: Integer): Boolean;
    function StopJob(JobId: Integer): Boolean;
    function BrowseInfo(const DefaultPath: String; const Shares: IStringList): Boolean;
    function GetAliases(out List: IStringList): Boolean;
  end;

implementation

end.
 