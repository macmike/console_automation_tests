unit uCmdLineAutomation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCmdLineAutomation }

  TCmdLineAutomation = class(TObject)
  private
    FCommandLine: string;
    FCurrentDirectoy: string;
    FIsRunning: boolean;
    FLastPrompt: string;
    FOnOutputRecieved: TNotifyEvent;
    FOnPrompt: TNotifyEvent;
    FOutput: TOutputStream;
    procedure SetCommandLine(AValue: string);
    procedure SetCurrentDirectoy(AValue: string);
    procedure SetIsRunning(AValue: boolean);
    procedure SetLastPrompt(AValue: string);
    procedure SetOnOutputRecieved(AValue: TNotifyEvent);
    procedure SetOnPrompt(AValue: TNotifyEvent);
    procedure SetOutput(AValue: TOutputStream);
  public
    property CurrentDirectoy : string read FCurrentDirectoy write SetCurrentDirectoy;
    property IsRunning : boolean read FIsRunning write SetIsRunning;
    property CommandLine : string read FCommandLine write SetCommandLine; //or cmd + args

    //input
    property OnPrompt : TNotifyEvent read FOnPrompt write SetOnPrompt;
    property LastPrompt : string read FLastPrompt write SetLastPrompt;
    procedure SendInput(const inputStr : String);

    //output
    property OnOutputRecieved : TNotifyEvent read FOnOutputRecieved write SetOnOutputRecieved;
    property Output : TOutputStream read FOutput write SetOutput;

    //exec
    procedure Start(const aCmdLine : string; params : string);
  end;

implementation



{ TCmdLineAutomation }

procedure TCmdLineAutomation.SetCommandLine(AValue: string);
begin
  if FCommandLine=AValue then Exit;
  FCommandLine:=AValue;
end;

procedure TCmdLineAutomation.SetCurrentDirectoy(AValue: string);
begin
  if FCurrentDirectoy=AValue then Exit;
  FCurrentDirectoy:=AValue;
end;

procedure TCmdLineAutomation.SetIsRunning(AValue: boolean);
begin
  if FIsRunning=AValue then Exit;
  FIsRunning:=AValue;
end;

procedure TCmdLineAutomation.SetLastPrompt(AValue: string);
begin
  if FLastPrompt=AValue then Exit;
  FLastPrompt:=AValue;
end;

procedure TCmdLineAutomation.SetOnOutputRecieved(AValue: TNotifyEvent);
begin
  if FOnOutputRecieved=AValue then Exit;
  FOnOutputRecieved:=AValue;
end;

procedure TCmdLineAutomation.SetOnPrompt(AValue: TNotifyEvent);
begin
  if FOnPrompt=AValue then Exit;
  FOnPrompt:=AValue;
end;

procedure TCmdLineAutomation.SetOutput(AValue: TOutputStream);
begin
  if FOutput=AValue then Exit;
  FOutput:=AValue;
end;

procedure TCmdLineAutomation.SendInput(const inputStr: String);
begin

end;

procedure TCmdLineAutomation.Start;
begin

end;

end.

