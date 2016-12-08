unit consolelogic;

interface

{$mode delphi}{$H+}

uses 
  SysUtils, Classes;

type
  TOutputType = (otOutput, otError);
  TOutputTypes = set of TOutputType;

const
  otBoth = [otOutput, otError];
  otAny = otBoth;

type
  TPromptOption = record
    optCode  : String;
    cmdLine  : string;
  end;

  TConsoleLogicRule = class;

  TConsoleLogicWrite = class(TObject)
    rule         : TConsoleLogicRule;
    writeBuf     : string;
    isTerminate  : Boolean;
    isPrompt     : Boolean;
  end;

  { TConsoleLogicRule }

  TConsoleLogicRule = class(TObject)
    linefeed : string;
    display  : string;
    lookup   : string;
    isprompt : Boolean;
    source   : TOutputTypes;
    sendText : string;
    doTerminate: Boolean;
    constructor Create(const Alookup: string; const ASource: TOutputTypes=otAny);
    function Applies(const l: string): Boolean; virtual;
  end;

  TLineEvent = procedure (Sender: TObject; const line: string; at: TOutputType) of object;

  { TConsoleLogic }

  TConsoleLogic = class(TObject)
  private
    buf : array [TOutputType] of string;
    procedure CheckLines(lines: TStrings; ASource: TOutputType; var lastLineTrigerred: Boolean);
    function CheckLine(const l: string; ASource: TOutputType): Boolean;
    procedure ClearWrites;
  public
    Writes: TList;
    Rules : TList;
    AddTrailTextAsLine: Boolean;
    OnNoRuleLine : TLineEvent;
    constructor Create;
    destructor Destroy; override;

    function Consume(const atext: string; AOutput: TOutputType): Integer;
    procedure ClearTextBufs;

    procedure AddRule( ARule: TConsoleLogicRule ); overload;
    function AddRule(const atext, asendText: string): TConsoleLogicRule; overload;
    procedure Clear;
  end;

implementation

{ TConsoleLogicRule }

constructor TConsoleLogicRule.Create(const Alookup: string;
  const ASource: TOutputTypes);
begin
  inherited Create;
  lookup:=Alookup;
  source:=ASource;
end;

function TConsoleLogicRule.Applies(const l: string): Boolean;
begin
  Result:=Pos(lookup, l)>0;
end;

{ TConsoleLogic }

procedure TConsoleLogic.CheckLines(lines: TStrings; ASource: TOutputType; var lastLineTrigerred: Boolean);
var
  i : integer;
begin
  lastLineTrigerred:=false;
  for i:=0 to lines.Count-1 do
    if (CheckLine(lines[i], ASource)) and (i=lines.Count-1) then
        lastLineTrigerred:=true;
end;

function TConsoleLogic.CheckLine(const l: string; ASource: TOutputType): Boolean;
var
  i : integer;
  r : TConsoleLogicRule;
  w : TConsoleLogicWrite;
  any: Boolean;
begin
  any:=false;
  for i:=0 to Rules.Count-1 do begin
    r:=TConsoleLogicRule(Rules[i]);

    // very basic, right ;)
    if (ASource in r.source) and r.Applies(l) then begin
      w:=TConsoleLogicWrite.Create;
      w.rule:=r;
      w.writeBuf:=r.sendText;
      w.isTerminate:=r.doTerminate;
      w.isPrompt:=r.isprompt;
      Writes.Add(w);
      any:=true;
    end;
  end;
  if not any and Assigned(OnNoRuleLine) then OnNoRuleLine(Self, l, ASource);
  Result:=any;
end;

procedure TConsoleLogic.ClearWrites;
var
  i : integer;
begin
  for i:=0 to Writes.Count-1 do
    TConsoleLogicWrite(Writes[i]).Free;
  Writes.Clear;
end;

constructor TConsoleLogic.Create;
begin
  inherited Create;
  Writes:=TList.Create;
  Rules:=TList.Create;
end;

destructor TConsoleLogic.Destroy;
var
  i : integer;
begin
  for i:=0 to Writes.Count-1 do TObject(Writes[i]).Free;
  Writes.Free;
  for i:=0 to Rules.Count-1 do TObject(Rules[i]).Free;
  Rules.Free;
  inherited Destroy;
end;

function TConsoleLogic.Consume(const atext: string; AOutput: TOutputType): Integer;
var
  s: string;
  t: string;
  i: integer;
  j: integer;
  prs : TStringList;
  islast: Boolean;
begin
  ClearWrites;

  if atext='' then begin
    Result:=0;
    Exit;
  end;

  s:=buf[AOutput];
  i:=length(s);
  s:=s+atext;
  j:=length(s);

  // looking for a brand new line..
  while (j>i) and not (s[j] in [#13,#10]) do
    dec(j);
  if (j=i) and not AddTrailTextAsLine then begin
    buf[AOutput]:=s;
    // no new lines found
    Exit;
  end;

  t:=Copy(s, 1, j);
  s:=Copy(s, j+1, length(s));
  buf[AOutput]:=s;

  prs := TStringList.Create;
  try
    prs.Text:=t;
    if AddTrailTextAsLine then prs.add(s);
    islast:=false;
    CheckLines(prs, AOutput,islast);
    if islast and AddTrailTextAsLine then begin
      // the last line was recognized! to prevent "double" recognition
      // remove it from the parsing queue
      s:='';
      buf[Aoutput]:='';
    end;
  finally
    prs.Free;
  end;
  Result:=Writes.Count;
end;

procedure TConsoleLogic.ClearTextBufs;
begin
  buf[otError]:='';
  buf[otOutput]:='';
end;

procedure TConsoleLogic.AddRule(ARule: TConsoleLogicRule);
begin
  if Assigned(ARule) then
    Rules.Add(ARule);
end;

function TConsoleLogic.AddRule(const atext, asendText: string
  ): TConsoleLogicRule;
begin
  Result:=TConsoleLogicRule.Create(atext);
  AddRule(Result);
  Result.sendText:=LineEnding;
end;

procedure TConsoleLogic.Clear;
var
  i : integer;
begin
  for i:=0 to Rules.Count-1 do
    TObject(Rules[i]).Free;
  Rules.Clear;
end;

end.
