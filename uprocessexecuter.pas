unit uProcessExecuter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, ExtCtrls, process;

type

  { TProcessExecuter }

  TProcessExecuter = class(TObject)
  private
    FOnOutputChanged : TNotifyEvent;
    FOutput : TStringList;
    FReadOutputIdleTimer: TIdleTimer;
    FProcess: TProcess;
    FEnvironmentVariables : TStringList; static;
    function GetRunning : boolean;
    procedure OnIdleTimer(Sender: TObject);
  protected
    procedure DoOnOutputChanged;
  public
    //non-interactive
    class function EnvironmentVariables : TStringList;
    class function ExecuteCommand(const Fmt: string; const Args: array of const;
      const workingDir: string;
      var output: string;
      const waitForExecuteComplete : boolean = true;
      const showWindow : boolean = false;
      const quitOnNoMoreData: boolean = false): boolean;
    class function ExecuteCommand(const cmd: string;
      const workingDir: string;
      var output: string;
      const waitForExecuteComplete : boolean = true;
      const showWindow : boolean = false;
      const quitOnNoMoreData: boolean = true): boolean;
    class function ExecuteCommand(const Fmt : string; const Args:array of const) : string;
    class function ExecuteCommand(const cmd : string) : string;
    class procedure ReloadEnvironmentVariables;
    //interactive
    procedure StartProcess(const Fmt: string; const Args: array of const; const workingDir : string);
    procedure StartProcess(const cmd: string; const workingDir : string);
    procedure StopProcess;
    procedure SendInput(const input : string);
    property Output : TStringList read FOutput;
    property OnOutputChanged : TNotifyEvent read FOnOutputChanged write FOnOutputChanged;
    property Running : boolean read GetRunning;
    //const and dest
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses LCLIntf;

{ TProcessExecuter }



procedure TProcessExecuter.StartProcess(const Fmt : string;
  const Args : array of const; const workingDir : string);
var
  cmd : String;
begin
  cmd := Format(Fmt,Args);
  StartProcess(cmd,workingDir);
end;


procedure TProcessExecuter.OnIdleTimer(Sender: TObject);
var
  NoMoreOutput: boolean;

  procedure ReadProcessOutput;
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
    partialOutput : string;
  begin
    if assigned(FProcess.Output) then
    begin
      BytesAvailable := FProcess.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := FProcess.OutPut.Read(Buffer[1], BytesAvailable);
        partialOutput := copy(Buffer,1, BytesRead);
        //if Settings.LogCmdLine then
        //  Debug('[ProcExec] Partial output: %s', [partialOutput]);
        FOutput.Add(partialOutput);
        BytesAvailable := FProcess.Output.NumBytesAvailable;
        NoMoreOutput := false;
      end;
    end;
  end;

begin
  try
    if not assigned(FProcess) then Exit;
    repeat
      NoMoreOutput := true;
      ReadProcessOutput;
    until noMoreOutput;

    FReadOutputIdleTimer.Enabled := false;


    if (*Settings.LogCmdLine and*) (FOutput.Text <> '') then
       //Debug('[ProcExec] Done Success: %s', [FOutput.Text])
    else if assigned(FProcess.Stderr) then
    begin
      FOutput.LoadfromStream(FProcess.Stderr);
      //if Settings.LogCmdLine then
      //  Debug('[ProcExec] Done Error: %s', [FOutput.Text]);
    end;

    DoOnOutputChanged;

  except on e: Exception do
    begin
      FOutput.Add(e.message);
      //if Settings.LogCmdLine then
      //  Debug('[ProcExec] Done Exception: %s', [e.message]);
    end;
  end;

end;

class procedure TProcessExecuter.ReloadEnvironmentVariables;
begin
  FreeAndNil(FEnvironmentVariables);
end;


class function TProcessExecuter.EnvironmentVariables: TStringList;
var
  n: Integer;
  envString: String;
begin
  if not assigned(FEnvironmentVariables) then
  begin
    FEnvironmentVariables := TStringList.Create;
    for n := 0 to GetEnvironmentVariableCount - 1 do
    begin
      envString := GetEnvironmentString(n);
      //Debug('[EnvVar %d] %s',[n,envString]);
      FEnvironmentVariables.Add(envString);
    end;
  end;
  Result := FEnvironmentVariables;
end;


class function TProcessExecuter.ExecuteCommand(const Fmt: string;
  const Args: array of const): string;
begin
  Result := ExecuteCommand(Format(Fmt,Args));
end;

class function TProcessExecuter.ExecuteCommand(const cmd: string): string;
begin
  Result := '';
  ExecuteCommand(cmd,'',Result,true,false,false);
end;





class function TProcessExecuter.ExecuteCommand(const Fmt: string;
  const Args: array of const; const workingDir: string; var output: string;
  const waitForExecuteComplete: boolean; const showWindow: boolean;
  const quitOnNoMoreData: boolean): boolean;
begin
  Result := ExecuteCommand(Format(Fmt,Args),workingDir,output,waitForExecuteComplete,showWindow,quitOnNoMoreData);
end;




class function TProcessExecuter.ExecuteCommand(const cmd: string;
  const workingDir: string; var output: string;
  const waitForExecuteComplete: boolean; const showWindow: boolean;
  const quitOnNoMoreData: boolean): boolean;
var
  proc: TProcess;
  response: TStringList;
  startTime : DWORD;
  duration : integer;
  waitCount : integer;
  loopCount: Integer;
  currExitStatus: Integer;
  exMessage: String;
begin
  waitCount := 1;
  //if Settings.LogCmdLine then
  //begin
  //  Debug('');
  //  Debug('[ProcExec] Executing command: %s', [cmd]);
  //end;
  proc := TProcess.Create(nil);
  proc.Options := proc.Options + [poUsePipes,poStderrToOutPut];
  response := TStringList.Create;
  try
    proc.CurrentDirectory := workingDir;
    {$IFDEF DARWIN}
    proc.Environment.Text := EnvironmentVariables.Text;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    proc.Environment.Text := EnvironmentVariables.Text;
    if showWindow then
    begin
      proc.ShowWindow := swoShow;
      proc.Options := [poNewConsole];
      proc.StartupOptions := [suoUseShowWindow];
    end
    else
    begin
      proc.ShowWindow := swoHIDE;
      //proc.Options := proc.Options + [poNoConsole];
      proc.StartupOptions := [suoUseShowWindow];
    end;
    {$ENDIF}
    proc.{%H-}CommandLine := cmd;

    Result := False;
    try
      startTime := GetTickCount;
      loopCount := 0;
      proc.Execute;
      if not waitForExecuteComplete then Exit;

      //Execution wait loop
      while proc.Running do
      begin
        Sleep(10);
        Inc(loopcount);

        duration := GetTickCount - StartTime;
        currExitStatus := proc.ExitStatus;

        //Debug('[PROC] Process running %d ticks - exit code : %d',[duration,currExitStatus]);
        if quitOnNoMoreData and (duration >200) and (currExitStatus = 259) then
        begin
          if assigned(proc.Output) then
            response.LoadFromStream(proc.Output);
          Exit;
        end;


        //seems to crash :(
        if (duration > (waitcount * waitcount * 5000 (*Settings.OperationTimeout*))) then
        begin
          //Debug('Slow ExecuteCommand: "%s" exitcode: %d - shall we wait?', [cmd,currExitStatus]);
          //Dmitry: this UserMessages.ConfirmKeepGoingLongOperation is a singleton managed confirmation dialog.
          if not false (*UserMessages.ConfirmKeepGoingLongOperation(cmd,Settings.OperationTimeout)*) then
          begin
            if assigned(proc.Output) then
              response.LoadFromStream(proc.Output);
            //Dmitry: this exception is silly but it's here because my Exception framework isn't the point of this stuff
            raise Exception.Create('Big wait');
            //raise ESPSilentException.Create(response.Text);
          end;
          Inc(waitCount);
        end;
      end;

      //Execution finished one way or another
      if assigned(proc.Output) then
        response.LoadFromStream(proc.Output);

      if (response.Text = '') and assigned(proc.Stderr) then
      begin
        response.LoadfromStream(proc.Stderr);
        Result := False;
        //if Settings.LogCmdLine then
        //  Debug('[ProcExec] Done Error: %s', [response.Text]);
      end
      else
      begin
        Result := True;
        //if Settings.LogCmdLine then
        //  Debug('[ProcExec] Done Success: %s', [response.Text]);
      end;

    except
      on e: Exception do
      begin
        exMessage := e.message;
        response.Text := exMessage;
        Result := False;
        //if Settings.LogCmdLine then
        //  Debug('[ProcExec] Done Exception: ExitCode:%d Exception:%s', [proc.ExitStatus,response.Text]);
      end;
    end;

  finally
    proc.Free;
    output := response.Text;
    response.Free;
  end;
end;


procedure TProcessExecuter.DoOnOutputChanged;
begin
  if assigned(FOnOutputChanged) then
    FOnOutputChanged(Self);
end;

function TProcessExecuter.GetRunning : boolean;
begin
  Result := FProcess.Running;
end;

procedure TProcessExecuter.StartProcess(const cmd : string;
  const workingDir : string);
begin
  if not FProcess.Running then
  begin
    FOutput.Clear;
    FProcess.CurrentDirectory := workingDir;
    FProcess.{%H-}CommandLine := cmd;
    //if Settings.LogCmdLine then
    //  Debug('[ProcExec] Executing command: %s', [cmd]);
    FReadOutputIdleTimer.Enabled := true;
    FProcess.Execute;
  end;
end;

procedure TProcessExecuter.StopProcess;
begin
  FProcess.Terminate(0);
  FReadOutputIdleTimer.Enabled := false;
  OnIdleTimer(FReadOutputIdleTimer);
end;

procedure TProcessExecuter.SendInput(const input : string);
begin
  if FProcess.Running then
  begin
    //if Settings.LogCmdLine then
    //  Debug('[ProcExec] Send input: %s', [input]);
    FProcess.Input.Write(input[1],Length(input));
  end;
end;

constructor TProcessExecuter.Create;
begin
  FOutput := TStringList.Create;
  FProcess := TProcess.Create(nil);
  FProcess.Options := [poUsePipes,poStderrToOutPut];
  FReadOutputIdleTimer := TIdleTimer.Create(nil);
  FReadOutputIdleTimer.OnTimer := @OnIdleTimer;
end;

destructor TProcessExecuter.Destroy;
begin
  FReadOutputIdleTimer.Free;
  FProcess.Free;
  FOutput.Free;
  inherited Destroy;
end;



finalization
  if assigned(TProcessExecuter.FEnvironmentVariables) then
    FreeAndNil(TProcessExecuter.FEnvironmentVariables);

end.
