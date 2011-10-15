unit UiEventLog;

interface

type
  TEventType = (etError, etWarning, etInformation);

  IEventLog = interface
    ['{DBC2911C-5B85-458C-9A40-CE98E319AEB1}']
    procedure LogMessage(const Text: String; EventType: TEventType);
  end;

implementation

end.
 