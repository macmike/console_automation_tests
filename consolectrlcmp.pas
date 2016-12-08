unit ConsoleCtrlCmp;

interface

{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils, Classes,
  consolectrl, consolelogic;

type

  TConsoleControlComponent = class;

  { TConsoleControlThread }

  TConsoleControlThread = class(TThread)
  protected
    fOwner    : TConsoleControlComponent;
    syncBuf   : string;
    syncwrite : TConsoleLogicWrite;
    procedure Execute; override;
  public
    lg     : TConsoleLogic;
    cc     : TConsoleControl;
    exe    : string;
    cmds   : array of string;
    exeDir : string;
    envVars: TStringList;
    TermCode : Integer;
    isTerminated : Boolean;
    constructor Create(AOwner: TConsoleControlComponent; ALogic: TConsoleLogic);
    destructor Destroy; override;
  end;

  TPromptInfo = record
    userDisplay: string;    // the text from the rule
    consoleDisplay: string; // the text from the cmd-line
    rule: TConsoleLogicRule;
  end;

  TPromptEvent = procedure (Sender: TObject; const PromptInfo: TPromptInfo) of object;
  TTextOutputEvent = procedure (Sender: TObject; const txt: string) of object;

  { TConsoleControlComponent }

  TConsoleControlComponent = class(TComponent)
  private
    fCommandLine: String;
    fOnFinish: TNotifyEvent;
    fOnStdOut: TTextOutputEvent;
    fOnStdErr: TTextOutputEvent;
    fOnPrompt: TPromptEvent;
    fOnStart : TNotifyEvent;
    fThread  : TConsoleControlThread;
    fLogic   : TConsoleLogic;
    fWorkDir : string;
    fParams  : TStringList;
    fEnvVars : TStringList;
    function GetEnvVars: TStrings;
  protected
    procedure DoStart;
    procedure DoFinish;
    procedure DoPrompt;
    procedure DoStdOut;
    procedure DoStdErr;
    function GetParams: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop(AExitCode: Integer = 1);
    procedure SendInput(const v: string);

    property Params: TStrings read GetParams;
    property EnvVars: TStrings read GetEnvVars;
    property CommandLine: String read fCommandLine write fCommandLine;
    property WorkDir: string read fWorkDir write fWorkDir;

    property OnStart: TNotifyEvent read fOnStart write fOnStart;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnPrompt: TPromptEvent read fOnPrompt write fOnPrompt;

    property OnStdOut: TTextOutputEvent read fOnStdOut write fOnStdOut;
    property OnStdErr: TTextOutputEvent read fOnStdErr write fOnStdErr;

    property ParseRules: TConsoleLogic read fLogic;
  end;

implementation

{ TConsoleControlComponent }

function TConsoleControlComponent.GetEnvVars: TStrings;
begin
  Result:=fEnvVars;
end;

procedure TConsoleControlComponent.DoStart;
begin
  if Assigned(OnStart) then OnStart(Self);
end;

procedure TConsoleControlComponent.DoFinish;
begin
  if Assigned(OnFinish) then OnFinish(Self);
  // don't wait here! it's synchronized handler!
  // todo: do not free, until all data has been copied!
  fThread.FreeOnTerminate:=true;
  fThread:=nil;
end;

procedure TConsoleControlComponent.DoPrompt;
var
  pi : TPromptInfo;
  wr : TConsoleLogicWrite;
begin
  if Assigned(fOnPrompt) and Assigned(fThread) and Assigned(fThread.syncwrite) then begin
    wr:=fThread.syncwrite;
    FillChar(pi, sizeof(pi), 0);
    pi.userDisplay:=wr.rule.display;
    pi.rule:=wr.rule;
    fOnPrompt(Self, pi);
  end;
end;

procedure TConsoleControlComponent.DoStdOut;
begin
  if Assigned(OnStdOut) and Assigned(fThread) then begin
    OnStdOut(SElf, fThread.syncBuf);
  end;
end;

procedure TConsoleControlComponent.DoStdErr;
begin
  if Assigned(OnStdErr) and Assigned(fThread) then begin
    OnStdErr(SElf, fThread.syncBuf);
  end;
end;

function TConsoleControlComponent.GetParams: TStrings;
begin
  Result:=fParams;
end;

constructor TConsoleControlComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLogic:=TConsoleLogic.Create;
  fParams:=TStringList.Create;
  fEnvVars:=TStringList.Create;
end;

destructor TConsoleControlComponent.Destroy;
begin
  Stop;
  fEnvVars.Free;
  fLogic.Free;
  fParams.Free;
  inherited DEstroy;
end;

procedure TConsoleControlComponent.Start;
var
  i : integer;
begin
  if not Assigned(fThread) then begin
    fLogic.ClearTextBufs;;
    fThread:=TConsoleControlThread.Create(Self, fLogic);
    fThread.lg:=fLogic;
    fThread.exe:=CommandLine;
    fThread.exeDir:=WorkDir;
    SetLength(fThread.cmds, fParams.Count);
    for i:=0 to fParams.Count-1 do
      fThread.cmds[i]:=fParams[i];
    fThread.envVars.Assign(fEnvVars);
    fThread.Start;
  end;
end;

procedure TConsoleControlComponent.Stop(AExitCode: Integer = 1);
var
  t: TConsoleControlThread;
begin
  if Assigned(fThread) then begin
    t:=fThread;
    fThread:=nil;
    t.TermCode:=AExitCode;
    t.isTerminated:=true;
    t.WaitFor;
    t.Free;
    if Assigned(OnFinish) then OnFinish(Self);
  end;
end;

procedure TConsoleControlComponent.SendInput(const v: string);
begin
  if not Assigned(fThread) then Exit;
  fThread.cc.Send(v);
end;

{ TConsoleControlThread }

procedure TConsoleControlThread.Execute;
var
  s : string;
  WriteTerm : Boolean;

  procedure ProcOutput(const s: string; asource: TOutputType);
  var
    i : integer;
    w : TConsoleLogicWrite;
  begin
    lg.Consume(s, asource);
    for i:=0 to lg.Writes.Count-1 do begin
      w := TConsoleLogicWrite(lg.Writes[i]);
      if w.writeBuf<>'' then cc.Send( w.writeBuf );
      if w.isTerminate then WriteTerm:=true;
      if w.isPrompt then begin
        syncwrite:=w;
        Synchronize(fOwner.DoPrompt);
      end;
    end;
  end;


begin
  WriteTerm:=false;
  Synchronize(fOwner.DoStart);
  cc.Run(exe, cmds, exeDir, envVars);
  while not WriteTerm and not isTerminated and (cc.isRunning) do begin
    s:=cc.GetOut;
    if s<>'' then begin
      if Assigned(fOwner.OnStdOut) then begin
        syncBuf:=s;
        Synchronize( fOwner.DoStdOut );
      end;
      ProcOutput(s, otOutput)
    end;

    s:=cc.GetErr;
    if s<>'' then begin
      if Assigned(fOwner.OnStdErr) then begin
        syncBuf:=s;
        Synchronize( fOwner.DoStdErr );
      end;
      ProcOutput(s, otError)
    end;
  end;
  if isTerminated then
    cc.Terminate(TermCode);
  ConsCtrlWaitFor(cc);
  if not isTerminated then
    Synchronize(fOwner.DoFinish);
end;

constructor TConsoleControlThread.Create(AOwner: TConsoleControlComponent; ALogic: TConsoleLogic);
begin
  inherited Create(true);
  fOwner:=AOwner;
  //lg:=TConsoleLogic.Create;
  lg:=ALogic;
  cc:=TConsoleControl.Create;
  envvars:=TStringList.Create;
end;

destructor TConsoleControlThread.Destroy;
begin
  envvars.Free;
  cc.Free;
  inherited Destroy;
end;

end.
