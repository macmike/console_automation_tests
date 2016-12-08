unit ScriptExecutor;

interface
{$mode objfpc}{$H+}
uses
  SysUtils, Classes, ConsoleCtrlCmp;

type
  TTextEvent = procedure (SEnder: TObject; const txt: string) of object;

  { TScriptExecutor }

  // todo: TScriptExecutor shouldn't be working over TConsoleControlThread
  //       because their nature is the same - a high level component
  //       ...hmm.. maybe make it a helper?
  TScriptExecutor = class(TObject)
  private
    fCommand: String;
    fWorkDir: String;
    fcmp  : TConsoleControlComponent;
    procedure SetCommand(const ascriptfile:string);
    procedure DoOut(Sender: TObject; const txt: string);
    procedure DoErr(Sender: TObject; const txt: string);
    procedure DoStart(Sender : TObject);
    procedure DoFinish(Sender : TObject);
  public
    OnOutput:  TTextEvent;
    OnError  : TTextEvent;
    OnStart : TNotifyEvent;
    OnFinish : TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure SendInput(const text: string);
    property Command: String read fCommand write SetCommand;
    property WorkDir: string read fWorkDir write fWorkDir;
  end;

const
  LINUX_TERM = 'xterm-256color'; // setups up an environmental variable on process start
                                 // unix environmetnal variables cannot be modified in run-time

implementation

function GetScript: string;
var
  s : string;
begin
  {$ifdef unix}
  Result:=GetEnvironmentVariable('SHELL');
  {$else}
  Result:=GetEnvironmentVariable('SHELL');
  s:=GetEnvironmentVariable('SYSTEMROOT');
  if s =''then s:=GetEnvironmentVariable('WINDIR');
  Result:=IncludeTrailingPathDelimiter(s)+'System32\cmd.exe';
  {$endif}
end;

{ TScriptExecutor }

procedure TScriptExecutor.SetCommand(const ascriptfile: string);
var
  tmp: string;
  fs: TfileStream;
const
  ScriptExt = {$ifdef mswindows}'.bat'{$else}'.sh'{$endif};
begin
  tmp:=ChangeFileExt(GetTempFileName, ScriptExt);
  ForceDirectories(ExtractFileDir(tmp));
  fs:=TFileStream.Create(tmp, fmCreate);
  try
    if length(ascriptfile)>0 then
      fs.Write(ascriptfile[1], length(ascriptfile));
  finally
    fs.Free;
  end;

  {$ifdef unix}
  fcmp.EnvVars.Clear;
  fcmp.EnvVars.Add('TERM='+LINUX_TERM);
  {$endif}

  fcmp.CommandLine:=GetScript;
  {$ifdef mswindows}
  fcmp.Params.Add('/Q');
  fcmp.Params.Add('/C');
  {$endif}
  fcmp.Params.Add(tmp);
  fcmp.WorkDir:=fWorkDir;
  fcmp.Start;

end;

procedure TScriptExecutor.DoOut(Sender: TObject; const txt: string);
begin
  if Assigned(OnOutput) then OnOutput(Self, txt);
end;

procedure TScriptExecutor.DoErr(Sender: TObject; const txt: string);
begin
  if Assigned(OnError) then OnError(Self, txt);
end;

procedure TScriptExecutor.DoStart(Sender: TObject);
begin
  if assigned(OnStart) then OnStart(Self);
end;

procedure TScriptExecutor.DoFinish(Sender: TObject);
begin
  if assigned(OnFinish) then OnFinish(Self);
end;

constructor TScriptExecutor.Create;
begin
  inherited Create;
  fcmp:=TConsoleControlComponent.Create(nil);
  fcmp.OnStdErr:=@DoErr;
  fcmp.OnStdOut:=@DoOut;
  fcmp.OnStart:=@DoStart;
  fcmp.OnFinish:=@DoFinish;
end;

destructor TScriptExecutor.Destroy;
begin
  fcmp.Free;
  inherited Destroy;
end;

procedure TScriptExecutor.SendInput(const text: string);
begin
  fcmp.SendInput(text+LineEnding);
end;

end.
