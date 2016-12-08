unit rwprocess;

interface

{$mode delphi}{$H+}

uses
  SysUtils, Classes, process
    {$ifdef unix},nixprocess{$endif}
  ;

type

  { TRWProcess }

  TRWProcess = class(TObject)
  protected
    process      : TProcess;
    outbytes     : integer;
    errbytes     : integer;
    outbuf       : string;
    errbuf       : string;
    exitstatus   : Integer;
    procedure CloseProcess;
    function ReadProc(TimeOut: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Send(const data; dataSize: Integer): Boolean;
    function RecvOut(var data; dataSize: Integer): Integer;
    function RecvErr(var data; dataSize: Integer): Integer;
    function Await(TimeOut: integer = -1): Integer;
    procedure Run(const exename: string; const commands: array of string;
      const curdir: string; envVars: TStrings = nil);
    procedure Terminate(exitCode: integer);
    function isRunning: Boolean;
  end;

const
  SLEEP_PERIOD = 30;

  AWAIT_ERROR  = -1;
  AWAIT_NONE   = 0;
  AWAIT_OUT    = 1;
  AWAIT_ERR    = 2;
  AWAIT_OUTERR = AWAIT_OUT or AWAIT_ERR;

function RWOutLen(rw: TRWProcess): Integer;
function RWErrLen(rw: TRWProcess): Integer;

implementation

function RWOutLen(rw: TRWProcess): Integer;
begin
  if Assigned(rw)
    then Result:=rw.outbytes
    else Result:=0;
end;

function RWErrLen(rw: TRWProcess): Integer;
begin
  if Assigned(rw)
    then Result:=rw.errbytes
    else Result:=0;
end;


Const
  READ_BYTES = 65536;

{ TRWProcess }

procedure TRWProcess.Run(const exename: string;
  const commands: array of string; const curdir: string; envVars: TStrings);
var
  i : integer;
begin
  if Assigned(process) then CloseProcess;

  process:=TProcess.create(nil);
  process.Executable:=exename;
  if curdir<>'' then process.CurrentDirectory:=curdir;

  if high(commands)>=0 then
    for i:=low(commands) to high(commands) do
    begin
      process.Parameters.add(commands[i]);
    end;
  process.ShowWindow:=swoHIDE; // this is required to hide console on Windows
  process.Options := [poUsePipes];
  if Assigned(envVars) then
    process.Environment.Assign(envVars);
  process.Execute;
end;

procedure TRWProcess.Terminate(exitCode: integer);
begin
  if Assigned(process) then process.Terminate(exitCode);
end;

function TRWProcess.isRunning: Boolean;
begin
  Result:=Assigned(process) and (process.Running);
end;

procedure TRWProcess.CloseProcess;
begin
  process.Free;
  process:=nil;
end;

function TRWProcess.ReadProc(TimeOut: Integer): Integer;
var
  numbytes        : integer;
  available       : integer;
  outputlength    : integer;
  stderrlength    : integer;
  l : integer;
  anydata : Boolean;
begin
  Result:=AWAIT_NONE;

  outputlength:=0;
  stderrlength:=0;
  try

    while process.Running do begin
      // Only call ReadFromStream if Data from corresponding stream
      // is already available, otherwise, on  linux, the read call
      // is blocking, and thus it is not possible to be sure to handle
      // big data amounts bboth on output and stderr pipes. PM.
      anydata:=false;
      available:=process.Output.NumBytesAvailable;
      if available > 0 then begin
        anydata:=true;
        while (outbytes + available > length(outbuf)) do begin
          outputlength:=length(outbuf) + READ_BYTES;
          l:=length(outbuf);
          Setlength(outbuf,outputlength);
          FillChar(outbuf[l+1], length(outbuf)-l, #0);
        end;
        NumBytes := process.Output.Read(outbuf[1+outbytes], available);
        if NumBytes > 0 then begin
          Inc(outbytes, NumBytes);
          Result:=Result or AWAIT_OUT;
        end;
      end;
      // The check for assigned(P.stderr) is mainly here so that
      // if we use poStderrToOutput in p.Options, we do not access invalid memory.
      if assigned(process.stderr) and (process.StdErr.NumBytesAvailable > 0) then begin
        anydata:=true;
        available:=process.StdErr.NumBytesAvailable;
        while (errbytes + available > length(errbuf)) do begin
          stderrlength:=length(errbuf) + READ_BYTES;
          l:=length(errbuf);
          Setlength(errbuf,stderrlength);
          FillChar(errbuf[l+1], length(errbuf)-l, #0);
        end;
        NumBytes := process.StdErr.Read(errbuf[1+errbytes], available);
        if NumBytes > 0 then begin
          Inc(errbytes, NumBytes);
          Result:=Result or AWAIT_ERR;
        end;
      end;

      if not anydata then begin
        Sleep(SLEEP_PERIOD);
        if TimeOut>0 then begin
          TimeOut:=TimeOut-SLEEP_PERIOD;
          if TimeOut<=0 then Exit;
          // This is Exit, not Break to prevent reading "available"
        end;
      end else
        Exit;
    end;

    // Get left output after end of execution
    available:=process.Output.NumBytesAvailable;
    while available > 0 do begin
      if (outbytes + available > outputlength) then begin
        outputlength:=outbytes + READ_BYTES;
        Setlength(outbuf,outputlength);
      end;
      NumBytes := process.Output.Read(outbuf[1+outbytes], available);
      if NumBytes > 0 then begin
        Inc(outbytes, NumBytes);
        Result:=Result or AWAIT_OUT;
      end;
      available:=process.Output.NumBytesAvailable;
    end;

    setlength(outbuf,outbytes);
    while assigned(process.stderr) and (process.Stderr.NumBytesAvailable > 0) do begin
      available:=process.Stderr.NumBytesAvailable;
      if (errbytes+ available > stderrlength) then begin
        stderrlength:=errbytes + READ_BYTES;
        Setlength(errbuf,stderrlength);
      end;
      NumBytes := process.StdErr.Read(errbuf[1+errbytes], available);
      if NumBytes > 0 then begin
        Inc(errbytes, NumBytes);
        Result:=Result or AWAIT_ERR;
      end;
    end;
    exitstatus:=process.exitstatus;
  except
  end;

end;

constructor TRWProcess.Create;
begin
  inherited Create;
end;

destructor TRWProcess.Destroy;
begin
  CloseProcess;
  inherited Destroy;
end;

function TRWProcess.Send(const data; dataSize: integer): Boolean;
begin
  if not Assigned(process) or (not process.Running) then begin
    Result:=false;
    Exit;
  end;
  Result:=true;
  if dataSize>0 then
    process.Input.Write(data, dataSize);
end;

function TRWProcess.RecvErr(var data; dataSize: Integer): Integer;
var
  i: integer;
begin
  if not Assigned(process) or (errbytes=0) then begin
    Result:=0;
    Exit;
  end;

  if dataSize>errbytes then dataSize:=errbytes;
  Result:=errbytes;
  Move(errbuf[1], data, Result);

  i:=errbytes - Result;
  if i>0 then
    Move(errbuf[Result+1], errbuf[1], i);
  dec(errbytes, Result);
end;

function TRWProcess.RecvOut(var data; dataSize: Integer): Integer;
var
  i: integer;
begin
  if not Assigned(process) or (outbuf='') then begin
    Result:=0;
    Exit;
  end;

  if dataSize>outbytes then dataSize:=outbytes;
  Result:=dataSize;
  Move(outbuf[1], data, Result);

  i:=outbytes - Result;
  if i>0 then Move(outbuf[Result+1], outbuf[1], i);
  dec(outbytes, Result);
end;

function TRWProcess.Await(TimeOut: integer): Integer;
begin
  if not Assigned(process) then begin
    Result:=AWAIT_ERROR;
    Exit;
  end;

  if (process.Running) or (process.Output.NumBytesAvailable>0)
    or (process.Stderr.NumBytesAvailable>0)
  then
    Result:=ReadProc(TimeOut)
  else
    Result:=AWAIT_ERROR;
end;

end.
