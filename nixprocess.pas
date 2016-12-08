unit nixprocess;

interface

Uses Classes,
     pipes,
     ctypes,
     UnixType,
     Unix,
     Baseunix,
     TermIO,
     SysUtils;

Type
  TProcessOption = (poRunSuspended,poWaitOnExit,
                    poUsePipes,poStderrToOutPut,
                    poNoConsole,poNewConsole,
                    poDefaultErrorMode,poNewProcessGroup,
                    poDebugProcess,poDebugOnlyThisProcess);

  TShowWindowOptions = (swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow,
                        swoShowDefault,swoShowMaximized,swoShowMinimized,
                        swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal);

  TStartupOption = (suoUseShowWindow,suoUseSize,suoUsePosition,
                    suoUseCountChars,suoUseFillAttribute);

  TProcessPriority = (ppHigh,ppIdle,ppNormal,ppRealTime);

  TProcessOptions = set of TProcessOption;
  TStartupOptions = set of TStartupOption;

  {$ifdef darwin}

  { TInputPipeStream }

  TInputPipeStream = Class(THandleStream)
    Private
      FPos : Int64;
      function GetNumBytesAvailable: DWord;
    protected
      function GetPosition: Int64; override;
      procedure InvalidSeek; override;
    public
      destructor Destroy; override;
      Function Write (Const Buffer; Count : Longint) :Longint; Override;
      function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
      Function Read (Var Buffer; Count : Longint) : longint; Override;
      property NumBytesAvailable: DWord read GetNumBytesAvailable;
    end;

  {$endif}

Type
  {$ifdef UNIX}
  TProcessForkEvent = procedure(Sender : TObject) of object;
  {$endif UNIX}

  { TProcess }

  TProcess = Class (TComponent)
  Private
    FProcessOptions : TProcessOptions;
    FStartupOptions : TStartupOptions;
    FProcessID : Integer;
    FTerminalProgram: String;
    FThreadID : Integer;
    FProcessHandle : Thandle;
    FThreadHandle : Thandle;
    FFillAttribute : Cardinal;
    FApplicationName : string;
    FConsoleTitle : String;
    FCommandLine : String;
    FCurrentDirectory : String;
    FDesktop : String;
    FEnvironment : Tstrings;
    FExecutable : String;
    FParameters : TStrings;
    FShowWindow : TShowWindowOptions;
    FInherithandles : Boolean;
    {$ifdef UNIX}
    FForkEvent : TProcessForkEvent;
    {$endif UNIX}
    FProcessPriority : TProcessPriority;
    dwXCountchars,
    dwXSize,
    dwYsize,
    dwx,
    dwYcountChars,
    dwy : Cardinal;
    FXTermProgram: String;
    FPipeBufferSize : cardinal;
    Procedure FreeStreams;
    Function  GetExitStatus : Integer;
    Function  GetExitCode : Integer;
    Function  GetRunning : Boolean;
    Function  GetWindowRect : TRect;
    procedure SetCommandLine(const AValue: String);
    procedure SetParameters(const AValue: TStrings);
    Procedure SetWindowRect (Value : TRect);
    Procedure SetShowWindow (Value : TShowWindowOptions);
    Procedure SetWindowColumns (Value : Cardinal);
    Procedure SetWindowHeight (Value : Cardinal);
    Procedure SetWindowLeft (Value : Cardinal);
    Procedure SetWindowRows (Value : Cardinal);
    Procedure SetWindowTop (Value : Cardinal);
    Procedure SetWindowWidth (Value : Cardinal);
    procedure SetApplicationName(const Value: String);
    procedure SetProcessOptions(const Value: TProcessOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetEnvironment(const Value: TStrings);
    Procedure ConvertCommandLine;
    function  PeekExitStatus: Boolean;
  Protected
    FRunning : Boolean;
    FExitCode : Cardinal;
    FInputStream  : TOutputPipeStream;
    FOutputStream : TInputPipeStream;
    FStderrStream : TInputPipeStream;
    procedure CloseProcessHandles; virtual;
    Procedure CreateStreams(InHandle,OutHandle,ErrHandle : Longint);virtual;
    procedure FreeStream(var AStream: THandleStream);
    procedure Loaded; override;
  Public
    Constructor Create (AOwner : TComponent);override;
    Destructor Destroy; override;
    Procedure Execute; virtual;
    procedure CloseInput; virtual;
    procedure CloseOutput; virtual;
    procedure CloseStderr; virtual;
    Function Resume : Integer; virtual;
    Function Suspend : Integer; virtual;
    Function Terminate (AExitCode : Integer): Boolean; virtual;
    Function WaitOnExit : Boolean;
    Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
    Property Handle : THandle Read FProcessHandle;
    Property ProcessHandle : THandle Read FProcessHandle;
    Property ThreadHandle : THandle Read FThreadHandle;
    Property ProcessID : Integer Read FProcessID;
    Property ThreadID : Integer Read FThreadID;
    Property Input  : TOutputPipeStream Read FInputStream;
    Property Output : TInputPipeStream  Read FOutputStream;
    Property Stderr : TinputPipeStream  Read FStderrStream;
    Property ExitStatus : Integer Read GetExitStatus;
    Property ExitCode : Integer Read GetExitCode;
    Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
    {$ifdef UNIX}
    property OnForkEvent : TProcessForkEvent Read FForkEvent Write FForkEvent;
    {$endif UNIX}
  Published
    property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
    Property Active : Boolean Read GetRunning Write SetActive;
    Property ApplicationName : String Read FApplicationName Write SetApplicationName; deprecated;
    Property CommandLine : String Read FCommandLine Write SetCommandLine ; deprecated;
    Property Executable : String Read FExecutable Write FExecutable;
    Property Parameters : TStrings Read FParameters Write SetParameters;
    Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
    Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
    Property Desktop : String Read FDesktop Write FDesktop;
    Property Environment : TStrings Read FEnvironment Write SetEnvironment;
    Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
    Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
    Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
    Property Running : Boolean Read GetRunning;
    Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
    Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
    Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
    Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
    Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
    Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
    Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
    Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
    Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
  end;

  EProcess = Class(Exception);

Procedure CommandToList(S : String; List : TStrings);

{$ifdef unix}
Var
  TryTerminals : Array of string;
  XTermProgram : String;
  Function DetectXTerm : String;
{$endif unix}

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer):integer;
function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string):boolean;
function RunCommandInDir(const curdir,cmdline:string;var outputstring:string):boolean; deprecated;

function RunCommand(const exename:string;const commands:array of string;var outputstring:string):boolean;
function RunCommand(const cmdline:string;var outputstring:string):boolean; deprecated;


type
  Ptermios = ^termios;
  Pwinsize = ^winsize;

const
  ptylib = {$ifdef linux}'util'{$else}'c'{$endif};
function openpty(amaster:pcint; aslave:pcint;
  name:Pchar; termp:Ptermios; winp:Pwinsize):longint;cdecl;external ptylib name 'openpty';


implementation

{$DEFINE OS_HASEXITCODE}

Resourcestring
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrNoSuchProgram     = 'Executable not found: "%s"';
  SErrNoTerminalProgram = 'Could not detect X-Terminal program';
  SErrCannotFork        = 'Failed to Fork process';
  SErrCannotCreatePipes = 'Failed to create pipes';

Const
  PriorityConstants : Array [TProcessPriority] of Integer =
                      (20,20,0,-20);

Const
  GeometryOption : String = '-geometry';
  TitleOption : String ='-title';
{$ifdef darwin}
{ TInputPipeStream }

function TInputPipeStream.GetNumBytesAvailable: DWord;
begin
  // this is the reason why TInputPipeStream is reimplemented
  if fpioctl(Handle, TIOCOUTQ, @Result)<0 then
    Result := 0;
end;

function TInputPipeStream.GetPosition: Int64;
begin
  Result:=FPos;
end;

procedure TInputPipeStream.InvalidSeek;
begin
  Raise EPipeSeek.Create (ENoSeekMsg);
end;

destructor TInputPipeStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

function TInputPipeStream.Write(const Buffer; Count: Longint): Longint;
begin
  WriteNotImplemented;
  Result := 0;
end;

function TInputPipeStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  FakeSeekForward(Offset,Origin,FPos);
  Result:=FPos;
end;

function TInputPipeStream.Read(var Buffer; Count: Longint): longint;
begin
  Result:=Inherited Read(Buffer,Count);
  Inc(FPos,Result);
end;

{$endif}

procedure TProcess.CloseProcessHandles;

begin
 // Do nothing. Win32 call.
end;

Function TProcess.GetExitCode : Integer;

begin
  if (Not Running) and wifexited(FExitCode) then
    Result:=wexitstatus(FExitCode)
  else
    Result:=0;
end;

Function TProcess.PeekExitStatus : Boolean;
var
  res: cint;
begin
  repeat
    res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
  until (res<>-1) or (fpgeterrno<>ESysEINTR);
  result:=res=Handle;
  If Not Result then
    FexitCode:=cardinal(-1); // was 0, better testable for abnormal exit.
end;

Type
  TPCharArray = Array[Word] of pchar;
  PPCharArray = ^TPcharArray;

Function StringsToPCharList(List : TStrings) : PPChar;

Var
  I : Integer;
  S : String;

begin
  I:=(List.Count)+1;
  GetMem(Result,I*sizeOf(PChar));
  PPCharArray(Result)^[List.Count]:=Nil;
  For I:=0 to List.Count-1 do
    begin
    S:=List[i];
    Result[i]:=StrNew(PChar(S));
    end;
end;

Procedure FreePCharList(List : PPChar);

Var
  I : integer;

begin
  I:=0;
  While List[i]<>Nil do
    begin
    StrDispose(List[i]);
    Inc(I);
    end;
  FreeMem(List);
end;



Function DetectXterm : String;

  Function TestTerminal(S : String) : Boolean;

  begin
    Result:=FileSearch(s,GetEnvironmentVariable('PATH'),False)<>'';
    If Result then
      XTermProgram:=S;
  end;

  Function TestTerminals(Terminals : Array of String) : Boolean;

  Var
    I : integer;
  begin
    I:=Low(Terminals);
    Result:=False;
    While (Not Result) and (I<=High(Terminals)) do
      begin
      Result:=TestTerminal(Terminals[i]);
      inc(i);
      end;
  end;

Const
  Konsole   = 'konsole';
  GNomeTerm = 'gnome-terminal';
  DefaultTerminals : Array [1..6] of string
                   = ('x-terminal-emulator','xterm','aterm','wterm','rxvt','xfce4-terminal');

Var
  D :String;

begin
  If (XTermProgram='') then
    begin
    // try predefined
    If Length(TryTerminals)>0 then
      TestTerminals(TryTerminals);
    // try session-specific terminal
    if (XTermProgram='') then
      begin
      D:=LowerCase(GetEnvironmentVariable('DESKTOP_SESSION'));
      If (Pos('kde',D)<>0) then
        begin
        TestTerminal('konsole');
        end
      else if (D='gnome') then
        begin
        TestTerminal('gnome-terminal');
        end
      else if (D='windowmaker') then
        begin
        If not TestTerminal('aterm') then
          TestTerminal('wterm');
        end
      else if (D='xfce') then
        TestTerminal('xfce4-terminal');
      end;
    if (XTermProgram='') then
      TestTerminals(DefaultTerminals)
    end;
  Result:=XTermProgram;
  If (Result='') then
    Raise EProcess.Create(SErrNoTerminalProgram);
end;

Function MakeCommand(P : TProcess) : PPchar;

{$ifdef darwin}
Const
  TerminalApp = 'open';
{$endif}
{$ifdef haiku}
Const
  TerminalApp = 'Terminal';
{$endif}

Var
  Cmd : String;
  S  : TStringList;
  G : String;

begin
  If (P.ApplicationName='') and (P.CommandLine='') and (P.Executable='') then
    Raise EProcess.Create(SNoCommandline);
  S:=TStringList.Create;
  try
    if (P.ApplicationName='') and (P.CommandLine='') then
      begin
      S.Assign(P.Parameters);
      S.Insert(0,P.Executable);
      end
    else
      begin
      If (P.CommandLine='') then
        Cmd:=P.ApplicationName
      else
        Cmd:=P.CommandLine;
      CommandToList(Cmd,S);
      end;
    if poNewConsole in P.Options then
      begin
      {$ifdef haiku}
      If (P.ApplicationName<>'') then
        begin
        S.Insert(0,P.ApplicationName);
        S.Insert(0,'--title');
        end;
      {$endif}
      {$if defined(darwin) or defined(haiku)}
      S.Insert(0,TerminalApp);
      {$else}
      S.Insert(0,'-e');
      If (P.ApplicationName<>'') then
        begin
        S.Insert(0,P.ApplicationName);
        S.Insert(0,'-title');
        end;
      if suoUseCountChars in P.StartupOptions then
        begin
        S.Insert(0,Format('%dx%d',[P.dwXCountChars,P.dwYCountChars]));
        S.Insert(0,'-geometry');
        end;
      If (P.XTermProgram<>'') then
        S.Insert(0,P.XTermProgram)
      else
        S.Insert(0,DetectXterm);
      {$endif}
      end;
    {$ifndef haiku}
    if (P.ApplicationName<>'') then
      begin
      S.Add(TitleOption);
      S.Add(P.ApplicationName);
      end;
    G:='';
    if (suoUseSize in P.StartupOptions) then
      g:=format('%dx%d',[P.dwXSize,P.dwYsize]);
    if (suoUsePosition in P.StartupOptions) then
      g:=g+Format('+%d+%d',[P.dwX,P.dwY]);
    if G<>'' then
      begin
      S.Add(GeometryOption);
      S.Add(g);
      end;
    {$endif}
    Result:=StringsToPcharList(S);
  Finally
    S.free;
  end;
end;

Function GetLastError : Integer;

begin
  Result:=-1;
end;

Type
  TPipeEnd = (peRead,peWrite);
  TPipePair = Array[TPipeEnd] of cint;

Procedure CreatePipes(Var HI,HO,HE : TPipePair; CE : Boolean);

  Procedure CreatePair(Var P : TPipePair);

   begin
    If not CreatePipeHandles(P[peRead],P[peWrite]) then
      Raise EProcess.Create(SErrCannotCreatePipes);
   end;

  Procedure ClosePair(Var P : TPipePair);

  begin
    if (P[peRead]<>-1) then
      FileClose(P[peRead]);
    if (P[peWrite]<>-1) then
      FileClose(P[peWrite]);
  end;

var
  ttyname : string;
  res     : integer;
  fSlave  : cint;
  fMaster : cint;
begin
  HO[peRead]:=-1;HO[peWrite]:=-1;
  HI[peRead]:=-1;HI[peWrite]:=-1;
  HE[peRead]:=-1;HE[peWrite]:=-1;
  Try

    SetLength(ttyname, 1024);
    res:=openpty(@fMaster, @fSlave, nil, nil, nil);
    //if res=0 then fFileName:=
    HO[peRead]:=fMaster; HO[peWrite]:=fSlave;
    HI[peRead]:=fSlave; HI[peWrite]:=fMaster;

    if CE then begin
      res:=openpty(@fMaster, @fSlave, nil, nil, nil);
      HE[peRead]:=fMaster; HE[peWrite]:=fSlave;
    end;

  except
    ClosePair(HO);
    ClosePair(HI);
    If CE then
      ClosePair(HE);
    Raise;
  end;
end;

Function safefpdup2(fildes, fildes2 : cInt): cInt;
begin
  repeat
    safefpdup2:=fpdup2(fildes,fildes2);
  until (safefpdup2<>-1) or (fpgeterrno<>ESysEINTR);
end;

Function  ioctl(Handle:cint;Ndx: TIOCtlRequest; Data: cint):cint; cdecl; external;

Procedure TProcess.Execute;

Var
  HI,HO,HE : TPipePair;
  PID      : Longint;
  FEnv     : PPChar;
  Argv     : PPChar;
  fd       : Integer;
  res      : cint;
  FoundName,
  PName    : String;
  wpres : integer;
begin
  If (poUsePipes in FProcessOptions) then begin
    CreatePipes(HI,HO,HE,Not (poStdErrToOutPut in FProcessOptions));
  end;
  Try
    if FEnvironment.Count<>0 then
      FEnv:=StringsToPcharList(FEnvironment)
    else
      FEnv:=Nil;
    Try
      Argv:=MakeCommand(Self);
      Try
        If (Argv<>Nil) and (ArgV[0]<>Nil) then
          PName:=StrPas(Argv[0])
        else
          begin
          // This should never happen, actually.
          PName:=ApplicationName;
          If (PName='') then
            PName:=CommandLine;
          end;

        if not FileExists(PName) then begin
          FoundName := ExeSearch(Pname,fpgetenv('PATH'));
          if FoundName<>'' then
            PName:=FoundName
          else
            raise EProcess.CreateFmt(SErrNoSuchProgram,[PName]);
        end;

{$if (defined(DARWIN) or defined(SUNOS))}
        { can't use vfork in case the child has to be
          suspended immediately, because with vfork the
          child borrows the execution thread of the parent
          unit it either exits or execs -> potential
          deadlock depending on how quickly the SIGSTOP
          signal is delivered

          We also can't use vfork in case we have to change the working
          directory, use pipes or not use a console since calling anything but
          exec* or _exit after vfork is unsupported. For the same reason, also
          don't use vfork in case there is a forkevent (since we don't know
          what that one will call) }
        if (([poRunSuspended,PoUsePipes,poNoConsole] * Options) = []) and
           (FCurrentDirectory='') and
           not assigned(FForkEvent) then
          Pid:=fpvfork
        else
          Pid:=fpfork;
{$else}
        Pid:=fpfork;
{$endif}
        if Pid<0 then
          Raise EProcess.Create(SErrCannotFork);
        if (PID>0) then
          begin
            // Parent process. Copy process information.
            FProcessHandle:=PID;
            FThreadHandle:=PID;
            FProcessId:=PID;
            //FThreadId:=PID;
          end
        else
          begin
            { We're in the child }
            if (FCurrentDirectory<>'') then
               begin
{$push}{$i-}
                 ChDir(FCurrentDirectory);
                 { exit if the requested working directory does not exist (can
                   use DirectoryExists, that would not be atomic; cannot change
                   before forking because that would also change the CWD of the
                   parent, which could influence other threads }
                 if ioresult<>0 then
                   fpexit(127);
{$pop}
               end;
            if PoUsePipes in Options then
              begin
                // disabling Constrolling Terminal, so getpass() (used by i.e. ftp)
                // wouldn't not be able to open it and use it
                //FpIOCtl(0, TIOCNOTTY, nil);
                //wpres:=ioctl(0, TIOCSCTTY, HO[peWrite]);

                //FileClose(HI[peWrite]);
                safefpdup2(HI[peRead],0);
                //FileClose(HO[peRead]);
                safefpdup2(HO[peWrite],1);
                if (poStdErrToOutPut in Options) then begin
                  safefpdup2(HO[peWrite],2)
                end
                else
                  begin
                    FileClose(HE[peRead]);
                    safefpdup2(HE[peWrite],2);
                  end;
                  FpSetsid();
                  wpres:=ioctl(1, TIOCSCTTY, 0);
              end
            else if poNoConsole in Options then
              begin
                fd:=FileOpen('/dev/null',fmOpenReadWrite or fmShareDenyNone);
                safefpdup2(fd,0);
                safefpdup2(fd,1);
                safefpdup2(fd,2);
              end;
            if Assigned(FForkEvent) then
              FForkEvent(Self);
            if (poRunSuspended in Options) then
              fpkill(fpgetpid,SIGSTOP); // sigraise(SIGSTOP);
            if FEnv<>Nil then
              fpexecve(PName,Argv,Fenv)
            else
              fpexecv(PName,argv);
            fpExit(127);
          end
      Finally
        FreePcharList(Argv);
      end;
    Finally
      If (FEnv<>Nil) then
        FreePCharList(FEnv);
    end;
  Finally
    if POUsePipes in FProcessOptions then
      begin
        //FileClose(HO[peWrite]);
        //FileClose(HI[peRead]);
        if Not (poStdErrToOutPut in FProcessOptions) then
          FileClose(HE[peWrite]);
        CreateStreams(HI[peWrite],HO[peRead],HE[peRead]);
      end;
  end;
  FRunning:=True;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in FProcessOptions) and
      not (poRunSuspended in FProcessOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Boolean;

Var
  R : Dword;

begin
  if FRunning then
    fexitcode:=waitprocess(handle);
  Result:=(fexitcode>=0);
  FRunning:=False;
end;

Function TProcess.Suspend : Longint;

begin
  If fpkill(Handle,SIGSTOP)<>0 then
    Result:=-1
  else
    Result:=1;
end;

Function TProcess.Resume : LongInt;

begin
  If fpKill(Handle,SIGCONT)<>0 then
    Result:=-1
  else
    Result:=0;
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  Result:=fpkill(Handle,SIGTERM)=0;
  If Result then
    begin
    If Running then
      Result:=fpkill(Handle,SIGKILL)=0;
    end;
  { the fact that the signal has been sent does not
    mean that the process has already handled the
    signal -> wait instead of calling getexitstatus }
  if Result then
    WaitOnExit;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);

begin
  FShowWindow:=Value;
end;

Procedure CommandToList(S : String; List : TStrings);

  Function GetNextWord : String;

  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : char;

  begin
    WStart:=1;
    While (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
      begin
      if S[Wend] in Literals then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;

     Result:=Copy(S,WStart,WEnd-WStart);

     if  (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
       Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);

  end;

Var
  W : String;

begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;

Constructor TProcess.Create (AOwner : TComponent);
begin
  Inherited;
  FProcessPriority:=ppNormal;
  FShowWindow:=swoNone;
  FInheritHandles:=True;
  {$ifdef UNIX}
  FForkEvent:=nil;
  {$endif UNIX}
  FPipeBufferSize := 1024;
  FEnvironment:=TStringList.Create;
  FParameters:=TStringList.Create;
end;

Destructor TProcess.Destroy;

begin
  FParameters.Free;
  FEnvironment.Free;
  FreeStreams;
  CloseProcessHandles;
  Inherited Destroy;
end;

Procedure TProcess.FreeStreams;
begin
  If FStderrStream<>FOutputStream then
    FreeStream(THandleStream(FStderrStream));
  FreeStream(THandleStream(FOutputStream));
  FreeStream(THandleStream(FInputStream));
end;


Function TProcess.GetExitStatus : Integer;

begin
  GetRunning;
  Result:=FExitCode;
end;

{$IFNDEF OS_HASEXITCODE}
Function TProcess.GetExitCode : Integer;

begin
  if Not Running then
    Result:=GetExitStatus
  else
    Result:=0
end;
{$ENDIF}

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    FRunning:=Not PeekExitStatus;
  Result:=FRunning;
end;


Procedure TProcess.CreateStreams(InHandle,OutHandle,ErrHandle : Longint);

begin
  FreeStreams;
  FInputStream:=TOutputPipeStream.Create (InHandle);
  FOutputStream:=TInputPipeStream.Create (OutHandle);
  if Not (poStderrToOutput in FProcessOptions) then
    FStderrStream:=TInputPipeStream.Create(ErrHandle);
end;

procedure TProcess.FreeStream(var AStream: THandleStream);
begin
  if AStream = nil then exit;
  FreeAndNil(AStream);
end;

procedure TProcess.Loaded;
begin
  inherited Loaded;
  If (csDesigning in ComponentState) and (FCommandLine<>'') then
    ConvertCommandLine;
end;

procedure TProcess.CloseInput;
begin
  FreeStream(THandleStream(FInputStream));
end;

procedure TProcess.CloseOutput;
begin
  FreeStream(THandleStream(FOutputStream));
end;

procedure TProcess.CloseStderr;
begin
  FreeStream(THandleStream(FStderrStream));
end;

Procedure TProcess.SetWindowColumns (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseCountChars);
  dwXCountChars:=Value;
end;


Procedure TProcess.SetWindowHeight (Value : Cardinal);

begin
  if Value<>0 then
    include(FStartupOptions,suoUsePosition);
  dwYSize:=Value;
end;

Procedure TProcess.SetWindowLeft (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseSize);
  dwx:=Value;
end;

Procedure TProcess.SetWindowTop (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUsePosition);
  dwy:=Value;
end;

Procedure TProcess.SetWindowWidth (Value : Cardinal);
begin
  If (Value<>0) then
    Include(FStartupOptions,suoUseSize);
  dwXSize:=Value;
end;

Function TProcess.GetWindowRect : TRect;
begin
  With Result do
    begin
    Left:=dwx;
    Right:=dwx+dwxSize;
    Top:=dwy;
    Bottom:=dwy+dwysize;
    end;
end;

procedure TProcess.SetCommandLine(const AValue: String);
begin
  if FCommandLine=AValue then exit;
  FCommandLine:=AValue;
  If Not (csLoading in ComponentState) then
    ConvertCommandLine;
end;

procedure TProcess.SetParameters(const AValue: TStrings);
begin
  FParameters.Assign(AValue);
end;

Procedure TProcess.SetWindowRect (Value : Trect);
begin
  Include(FStartupOptions,suoUseSize);
  Include(FStartupOptions,suoUsePosition);
  With Value do
    begin
    dwx:=Left;
    dwxSize:=Right-Left;
    dwy:=Top;
    dwySize:=Bottom-top;
    end;
end;


Procedure TProcess.SetWindowRows (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseCountChars);
  dwYCountChars:=Value;
end;

procedure TProcess.SetApplicationName(const Value: String);
begin
  FApplicationName := Value;
  If (csDesigning in ComponentState) and
     (FCommandLine='') then
    FCommandLine:=Value;
end;

procedure TProcess.SetProcessOptions(const Value: TProcessOptions);
begin
  FProcessOptions := Value;
  If poNewConsole in FProcessOptions then
    Exclude(FProcessOptions,poNoConsole);
  if poRunSuspended in FProcessOptions then
    Exclude(FProcessOptions,poWaitOnExit);
end;

procedure TProcess.SetActive(const Value: Boolean);
begin
  if (Value<>GetRunning) then
    If Value then
      Execute
    else
      Terminate(0);
end;

procedure TProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

procedure TProcess.ConvertCommandLine;
begin
  FParameters.Clear;
  CommandToList(FCommandLine,FParameters);
  If FParameters.Count>0 then
    begin
    Executable:=FParameters[0];
    FParameters.Delete(0);
    end;
end;

Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function internalRuncommand(p:TProcess;var outputstring:string;
                            var stderrstring:string; var exitstatus:integer):integer;
var
    numbytes,bytesread,available : integer;
    outputlength, stderrlength : integer;
    stderrnumbytes,stderrbytesread : integer;
begin
  result:=-1;
  try
    try
    p.Options :=  [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    p.Execute;
    while p.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available:=P.Output.NumBytesAvailable;
        if  available > 0 then
          begin
            if (BytesRead + available > outputlength) then
              begin
                outputlength:=BytesRead + READ_BYTES;
                Setlength(outputstring,outputlength);
              end;
            NumBytes := p.Output.Read(outputstring[1+bytesread], available);
            if NumBytes > 0 then
              Inc(BytesRead, NumBytes);
          end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
          begin
            available:=P.StdErr.NumBytesAvailable;
            if (StderrBytesRead + available > stderrlength) then
              begin
                stderrlength:=StderrBytesRead + READ_BYTES;
                Setlength(stderrstring,stderrlength);
              end;
            StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
            if StderrNumBytes > 0 then
              Inc(StderrBytesRead, StderrNumBytes);
          end
        else
          Sleep(100);
      end;
    // Get left output after end of execution
    available:=P.Output.NumBytesAvailable;
    while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
          begin
            outputlength:=BytesRead + READ_BYTES;
            Setlength(outputstring,outputlength);
          end;
        NumBytes := p.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available:=P.Output.NumBytesAvailable;
      end;
    setlength(outputstring,BytesRead);
    while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available:=P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength:=StderrBytesRead + READ_BYTES;
            Setlength(stderrstring,stderrlength);
          end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
    setlength(stderrstring,StderrBytesRead);
    exitstatus:=p.exitstatus;
    result:=0; // we came to here, document that.
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
         end;
     end;
  finally
    p.free;
    end;
end;

{ Functions without StderrString }

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer):integer;
Var
    p : TProcess;
    i : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,errorstring,exitstatus);
end;

function RunCommandInDir(const curdir,cmdline:string;var outputstring:string):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  p.setcommandline(cmdline);
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  result:=internalruncommand(p,outputstring,errorstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,errorstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const cmdline:string;var outputstring:string):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  p.setcommandline(cmdline);
  result:=internalruncommand(p,outputstring,errorstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const exename:string;const commands:array of string;var outputstring:string):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
    ErrorString : String;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,errorstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

end.
