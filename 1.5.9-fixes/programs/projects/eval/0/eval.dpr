program eval;

{%File 'expression.txt'}
{%File 'data.txt'}
{%File 'lex.txt'}
{%File 'variables.txt'}
{%File 'statement.txt'}
{%ToDo 'eval.todo'}
{%File 'grammar.txt'}

(*)
  SilTokens
    SilSiToken
    SilSeTokens
    SilStTokens
    SilSgTokens
    SilSfTokens
    SilSmToken

  SilLexer
    SilStLexCondition
    SilSiLexDictionary
    SilSmLexDefinition
    SilSmLexDictionary
    SilSmLexMatches
    SilSmLexCondition
    SilStLexDictionary
    SilSiTokenLexer
    SilSmTokenLexer
    SilSmTokenTable
    SilStTokenLexer

  SilEval
    SilScEvalTokens
    SilSfEvalParser
    SilSiEvaluator
    SilSfEvalTokens
    SilSmEvaluator
    SilSmEvalOperators
    SilSgEvalOperators
    SilSfEvalParserHelpers
    SilStEvaluator

  SilScript
    SilScScriptTokens
    SilStParser
    SilStScript
    SilSfScriptParser


  __SilParser__
    SilSiTokenSyntaxTree
    SilSiTokenAnalyzer
(*)



uses
  Forms,
  FmEvalMain in 'FmEvalMain.pas' {FormTester},
  FmEvalCode in 'FmEvalCode.pas' {FormCode},
  FmEvalDisplay in 'FmEvalDisplay.pas' {FormDisplay};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTester, FormTester);
  Application.Run;
end.
