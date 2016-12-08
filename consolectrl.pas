unit consolectrl;

interface

{$mode delphi}{$H+}

uses
  SysUtils, Classes, rwprocess;

type

  { TConsoleThread }

  TConsoleThread = class(TThread)
  protected
    outbuf   : string;
    errbuf   : string;
    rdcs : TRTLCriticalSection; // read critical section

    wbcs : TRTLCriticalSection; // write critical section
    wbuf : array of byte;
    wbsz : Integer; // size
    procedure Execute; override;
  public
    exeName  : string;
    exeDir   : string;
    cmds     : array of string;
    cmdCount : Integer;
    envVars  : TStringList;
    fproc    : TRWProcess;
    isFinished : Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure AddWriteBuf(const Data; Size: Integer);
    procedure PopOut(var data: string);
    procedure PopErr(var data: string);

    function hasData: Boolean;
  end;

  { TConsoleControl }

  TConsoleControl = class(TObject)
  private
    fTP : TConsoleThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(const exename: string; const cmd: array of string; const dir: string; envVars: TStrings = nil);
    procedure Send(const adata: string);
    procedure Terminate(const AExitCode: integer = 1);
    function GetOut: string;
    function GetErr: string;
    function isRunning: Boolean;
  end;

procedure ConsCtrlWaitFor(cc: TConsoleControl);

implementation

{ TConsoleControl }

constructor TConsoleControl.Create;
begin
  inherited Create;
  fTP:=TConsoleThread.Create;
end;

destructor TConsoleControl.Destroy;
begin
  if not fTP.Terminated then fTP.Terminate;
  fTP.WaitFor;
  inherited Destroy;
end;

procedure TConsoleControl.Run(const exename: string;
  const cmd: array of string; const dir: string;
  envVars: TStrings);
var
  i : integer;
begin
  fTP.exeName:=exename;
  fTP.exeDir:=dir;
  SetLength(fTP.cmds, length(cmd));
  fTP.cmdCount:=length(cmd);
  for i:=0 to length(cmd)-1 do fTP.cmds[i]:=cmd[i];
  if Assigned(envVars) then
     fTP.envVars.Assign(envVars);
  fTP.Start;
end;

procedure TConsoleControl.Send(const adata: string);
begin
  if adata<>'' then
    fTP.AddWriteBuf(adata[1], length(adata));
end;

procedure TConsoleControl.Terminate(const AExitCode: integer = 1);
begin
  fTP.fproc.Terminate(AExitCode);
  fTP.Terminate;
end;

function TConsoleControl.GetOut: string;
begin
  fTP.PopOut(Result);
end;

function TConsoleControl.GetErr: string;
begin
  fTP.PopErr(Result);
end;

function TConsoleControl.isRunning: Boolean;
begin
  Result:=(not fTP.isFinished) or fTP.hasData;
end;

{ TConsoleThread }

procedure TConsoleThread.Execute;
var
  res : Integer;
  i   : integer;
  w   : array of byte;
  wz  : integer;
  ext : integer;
  rcv : integer;
begin
  w:=nil;
  try
    fproc.Run(exeName, Slice(cmds, cmdCount), exeDir, envVars);
    while not Terminated do begin

      EnterCriticalsection(wbcs);
      try
        if wbsz>0 then begin
          wz:=wbsz;
          // move and quickly leave critical seciton
          // writting to a process might take a while
          if length(w)<wz then SetLength(w, wz);
          Move(wbuf[0], w[0], wz);
          wbsz:=0;
        end else
          wz:=0;
      finally
        LeaveCriticalsection(wbcs);
      end;

      if wz>0 then begin
        fproc.Send(w[0], wz);
        wz:=0;
      end;

      res:=fproc.Await(SLEEP_PERIOD);
      if res<0 then begin
        // process is terminated
        Break;
      end;
      if ((res and AWAIT_OUT)>0) then begin
        // std out read
        EnterCriticalsection(rdcs);
        try
          ext:=RWOutLen(fproc);
          if ext>0 then begin
            i:=length(outbuf);
            SetLength(outbuf, i+ext);
            rcv:=fproc.RecvOut(outbuf[i+1], ext);
          end;
        finally
          LeaveCriticalsection(rdcs);
        end;
      end;
      if ((res and AWAIT_ERR)>0) then begin
        // std error read
        EnterCriticalsection(rdcs);
        try
          ext:=RWErrLen(fproc);
          if ext>0 then begin
            i:=length(errbuf);
            SetLength(errbuf, i+ext);
            fproc.RecvErr(errbuf[i+1], ext);
          end;
        finally
          LeaveCriticalsection(rdcs);
        end;
      end;
    end;
  except
  end;
  isFinished := true;
end;

constructor TConsoleThread.Create;
begin
  inherited Create(true);
  fproc:=TRWProcess.Create;
  envVars:=TStringList.Create;
  InitCriticalSection(wbcs);
  InitCriticalSection(rdcs);
end;

destructor TConsoleThread.Destroy;
begin
  DoneCriticalsection(wbcs);
  DoneCriticalsection(rdcs);
  envVars.Free;
  inherited Destroy;
end;

procedure TConsoleThread.AddWriteBuf(const Data; Size: Integer);
var
  l : integer;
begin
  EnterCriticalsection(wbcs);
  try
    l:=length(wbuf);
    if wbsz+Size>l then begin
      while wbsz+Size>l do begin
        if l=0 then l:=1024
        else l:=l*2;
      end;
      SetLength(wbuf, l);
    end;
    Move(Data, wbuf[wbsz], Size);
    inc(wbsz, Size);
  finally
    LeaveCriticalsection(wbcs);
  end;
end;

procedure TConsoleThread.PopOut(var data: string);
begin
  data:='';
  EnterCriticalsection(rdcs);
  try
    data:=outbuf;
    outbuf:='';
  finally
    LeaveCriticalsection(rdcs);
  end;
end;

procedure TConsoleThread.PopErr(var data: string);
begin
  data:='';
  EnterCriticalsection(rdcs);
  try
    data:=errbuf;
    errbuf:='';
  finally
    LeaveCriticalsection(rdcs);
  end;
end;

function TConsoleThread.hasData: Boolean;
begin
  EnterCriticalsection(rdcs);
  try
    Result:=(errbuf<>'') or (outbuf<>'');
  finally
    LeaveCriticalsection(rdcs);
  end;
end;

procedure ConsCtrlWaitFor(cc: TConsoleControl);
begin
  if not Assigned(cc) then Exit;
  cc.fTP.WaitFor;
end;

end.
