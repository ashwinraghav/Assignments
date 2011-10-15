{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilOtEnvironment;

{$INCLUDE Defines.inc}

interface

uses
  SilLiKey,
  SilLiField,
  SilLiValueList,
  
  SilOiEnvironment,
  SilOjEnvironment;

type
  WindowsEnvironment = class(SilEnvironment)
  private
    class procedure DoAddItem(const Context; const Name: string);
  public
    class procedure Load(const Key: INamedKey); override;
    class function Expand(const Value: String; const Default: String = ''; const Name: String = ''): String; override;
    class function GetValue(const Name: String): Variant; override;
    class procedure SetValue(const Name: String; const Value: Variant); override;
    class function List: IEnvironmentList; override;
    class function Variable(const Name: String): IFieldAccess; override;
    class function Variables: IValueList; override;
  end;

implementation

uses
  SilBtStr,
  SilBtText,
  SilBtVart,
  SilLiEnumerator,
  SilOsClasses,
  SilOtTool,
  SilLtList,
  SilOdEnvironment,
  SilOfEnvironment;

{ WindowsEnvironment }

class procedure WindowsEnvironment.Load(const Key: INamedKey);
var
  Enum: IEnumerator;
  KeyName, KeyValue: string;
begin
  SetValue('SELF_PROCESS', Os.Filesystem.DeleteSlash(Os.Process.Current.Info.Path));

  with Key.Keys.Get('Environment', True) do
    while Values.Enumerate(Enum, KeyName) do
    begin
      KeyValue := Expand(Values.ReadString(KeyName), '', KeyName);
      SetValue(KeyName, KeyValue);
    end;
end;

class function WindowsEnvironment.Expand(const Value: String; const Default: String; const Name: String): String;
var
  VariableName: String;
  VariableValue: Variant;
  VariableText: string;
  Index: Integer;
begin
  Index := 1;
  Result := Value;
  while Str.Between(Result, '%', '%', VariableName, Index, @Index) do
    if Str.IsEmpty(Name) or (Text.Compare(VariableName, Name) = 0) then
    begin
      VariableValue := GetValue(VariableName);
      if Vart.IsOK(VariableValue) then
        begin
          VariableText := VariableValue;
          Result := Str.Replace(Result, '%' + VariableName + '%', VariableText);
          Index := 1;
        end
      else if Str.NotEmpty(Default) then
        Result := Str.Replace(Result, '%' + VariableName + '%', Default);
    end;
end;

class function WindowsEnvironment.GetValue(const Name: String): Variant;
begin
  if not SilOfEnvironment.GetValue(Name, Result) then
    Result := Vart.Unassigned;
end;

class procedure WindowsEnvironment.SetValue(const Name: String; const Value: Variant);
begin
  SilOfEnvironment.SetValue(Name, Value);
end;

class function WindowsEnvironment.List: IEnvironmentList;
begin
  Result := TSilOsEnvironmentVariables.Create();
end;

class procedure WindowsEnvironment.DoAddItem(const Context; const Name: string);
var
  List: IValueList absolute Context;
  Item: IFieldAccess;
begin
  Item := TSilOsEnvironmentVariable.Create(Name);
  List.Add(Item);
end;

class function WindowsEnvironment.Variable(const Name: String): IFieldAccess;
begin
  Result := TSilOsEnvironmentVariable.Create(Name);
end;

class function WindowsEnvironment.Variables: IValueList;
var
  Buffer: PChar;
begin
  if SilOfEnvironment.GetStrings(Buffer) then
  try
    Result := ListTool.ValueList(true);
    SilOfEnvironment.ParseStrings(Result, Buffer, DoAddItem);
  finally
    SilOfEnvironment.FreeStrings(Buffer);
  end;
end;

end.
