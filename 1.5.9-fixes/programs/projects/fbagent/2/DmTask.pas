unit DmTask;

interface

uses
  SysUtils, Classes, DB, IBCustomDataSet, IBQuery, IBDatabase, IBStoredProc,

  Sil;

type
  TdaTask = class(TDataModule)
    doCategoryDel: TIBStoredProc;
    taCategory: TIBTransaction;
    quCategory: TIBQuery;
    doCategoryCheck: TIBStoredProc;
    doHistoryDel: TIBStoredProc;
    quHistory: TIBQuery;
    quHistoryITEM_ORDER: TIntegerField;
    quHistoryNAME: TIBStringField;
    quHistorySTART_TIME: TDateTimeField;
    quHistoryEND_TIME: TDateTimeField;
    quHistorySTATUS: TIBStringField;
    quHistoryTEXT_MESSAGE: TIBStringField;
    taHistory: TIBTransaction;
    taSchedule: TIBTransaction;
    quSchedule: TIBQuery;
    quScheduleSCHEDULE_ID: TIntegerField;
    quScheduleNAME: TIBStringField;
    quScheduleENABLED: TSmallintField;
    quScheduleDEL_KIND: TSmallintField;
    quScheduleINCREMENT: TDateTimeField;
    quScheduleDAY_FILTER_KIND: TSmallintField;
    quScheduleDAY_FILTER: TIntegerField;
    quScheduleMONTH_FILTER: TIntegerField;
    quScheduleDOW_POS_FILTER: TIntegerField;
    quScheduleSTART_TIME: TTimeField;
    quScheduleSTART_DATE: TDateField;
    quScheduleEND_TIME: TTimeField;
    quScheduleEND_DATE: TDateField;
    quScheduleRUN_COUNT: TIntegerField;
    quStep: TIBQuery;
    quStepSTEP_ID: TIntegerField;
    quStepNAME: TIBStringField;
    quStepENABLED: TSmallintField;
    quStepITEM_ORDER: TIntegerField;
    quStepON_SUCCESS: TIntegerField;
    quStepON_FAILURE: TIntegerField;
    quStepDATA: TMemoField;
    taStep: TIBTransaction;
    procedure GetEnabledText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure quHistoryEND_TIMEGetText(Sender: TField; var Text: String; DisplayText: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  daTask: TdaTask;

implementation

uses
  DmAgent,
  UeAgent;

{$R *.dfm}

{ TdaTask }

procedure TdaTask.quHistoryEND_TIMEGetText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  if quHistory.IsEmpty then
    Text := '' else
    Text := DateTime.ToStr(quHistoryEND_TIME.AsDateTime - quHistorySTART_TIME.AsDateTime, 'hh:nn:ss');
end;

procedure TdaTask.GetEnabledText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  if quStep.IsEmpty then
    Text := '' else
    Text := CYesNo[Sender.AsInteger = 1];
end;

end.
