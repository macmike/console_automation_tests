unit ufrmShell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, RichMemo;

type

  { TfrmShell }

  TfrmShell = class(TForm)
    btnExec: TButton;
    btnSetWorkingDir: TButton;
    btnTest1: TButton;
    btnTest2: TButton;
    btnTest3: TButton;
    btnTest4: TButton;
    btnTest7: TButton;
    btnTest6: TButton;
    btnTest5: TButton;
    edtInput: TEdit;
    edtWorkingDir: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmLog: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RichMemo1: TRichMemo;
    RichMemo2: TRichMemo;
    RichMemo3: TRichMemo;
    RichMemo4: TRichMemo;
    RichMemo5: TRichMemo;
    RichMemo6: TRichMemo;
    RichMemo7: TRichMemo;
    ScrollBox1: TScrollBox;
    procedure btnExecClick(Sender: TObject);
    procedure btnTest1Click(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
    procedure btnTest3Click(Sender: TObject);
    procedure btnTest4Click(Sender: TObject);
    procedure btnTest5Click(Sender: TObject);
    procedure btnTest6Click(Sender: TObject);
    procedure btnTest7Click(Sender: TObject);
    procedure edtInputKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    { public declarations }
  end;

var
  frmShell: TfrmShell;

implementation

uses LCLType;

{$R *.lfm}

(*
Rules:

1. No external binaries or tools other than those directly invoked (bash, cmd, keygen etc.)
2. Try not to infect this file too much, I don't care about this app or gui,
   I want a class that does the interactions and raises events. This is just a test stub.
3. No modifications are allowed to the test commands (e.g. extra switches to redirect input/output
   without prior agreement)
4. No inclusion of units or libraries that have *any* license restrictions.
   The solution should be free to share, sell and exploit by anyone without restrictions or credits.
5. Although not all tests are aimed at all platforms, the same class should handle all requests so
   that client code does not need to add different units and use different classes for different
   platforms (although obviously command text will be different).

Notes:
  I'd expect some code to create a process executer class, send command details and respond to events. I don't require
  programmatic test passing and parsing. Seeing the expected output in the memo, and being able to interact via edtSendInput is
  all that's required.

*)


procedure TfrmShell.edtInputKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnExecClick(Sender);
end;

procedure TfrmShell.btnExecClick(Sender: TObject);
begin
  //proc.SendInput(edtInput.Text);
end;

procedure TfrmShell.btnTest1Click(Sender: TObject);
begin
  //proc.Command := 'sudo ls';
end;

procedure TfrmShell.btnTest2Click(Sender: TObject);
begin
  //proc.Command := 'test_sudo.sh';
end;

procedure TfrmShell.btnTest3Click(Sender: TObject);
begin
  //proc.Command := 'ssh-keygen -t rsa -C testy@example.com';
end;

procedure TfrmShell.btnTest4Click(Sender: TObject);
begin
  //proc.Command := 'sudo apt-get install leafpad';
end;

procedure TfrmShell.btnTest5Click(Sender: TObject);
begin
  //proc.Command := 'brew cask install sublime-text';
end;

procedure TfrmShell.btnTest6Click(Sender: TObject);
begin
  //proc.Command := 'ftp';
end;

procedure TfrmShell.btnTest7Click(Sender: TObject);
begin
  //proc.Command := 'nano';

end;







end.

