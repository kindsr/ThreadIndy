unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, System.SyncObjs, Vcl.StdCtrls, IdTCPClient,
  IdAntiFreezeBase, IdAntiFreeze;

type
  TSimple = Class
   public
     aContext : TIdContext;
     bRead : Boolean;
     data : String;
   end;

 TWriteThread = class(TThread)
  private
    FListBox: TListBox;
    FsData: TStringList;
    FIdClient : TIdTCPClient;
    FtCS: TCriticalSection;
    FSave_Context : Tlist;
    procedure SetListBox(const Value: TListBox);
    procedure SetsData(const Value: TStringList);
    procedure SetIndy(const Value: TIdTCPClient);
    procedure SettCS(const Value: TCriticalSection);
    procedure SetSave_Context(const Value: Tlist);
  protected

    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ListBox : TListBox read FListBox write SetListBox;
    property sData : TStringList read FsData write SetsData;
    property tCS : TCriticalSection read FtCS write SettCS;
    property Save_Context : TList Read FSave_Context Write SetSave_Context;
  end;

 TDisplayThread = class(TThread)
  private
    FsData: TStringList;
    FListBox: TListBox;
    FtCS: TCriticalSection;
    procedure SetListBox(const Value: TListBox);
    procedure SetsData(const Value: TStringList);
    procedure SettCS(const Value: TCriticalSection);
  protected

    procedure Display;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ListBox : TListBox read FListBox write SetListBox;
    property sData : TStringList read FsData write SetsData;
    property tCS : TCriticalSection read FtCS write SettCS;
  end;

type
  TForm3 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    IdAntiFreeze1: TIdAntiFreeze;
    ListBox1: TListBox;
    BTN_ServerStart: TButton;
    BTN_ServerStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BTN_ServerStopClick(Sender: TObject);
    procedure BTN_ServerStartClick(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
  private
    { Private declarations }
  public
    { Public declarations }
    Save_Context  : Tlist;
    Write_Thread : TWriteThread;
    Display_Thread : TDisplayThread;
    CTS_Lock: TCriticalSection;
    sData: TStringList;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

{ TWriteThread }

constructor TWriteThread.Create;
begin
  FreeOnTerminate := False;
  inherited Create( True );
end;

destructor TWriteThread.Destroy;
begin
  inherited;
end;

procedure TWriteThread.Execute;
var
  t : Cardinal;
  Simple : TSimple;
begin
  while not Terminated do
  Begin
    Try

      if Save_Context.Count > 0 then
      try
        tCS.Enter;
        Simple := TSimple( Save_Context[0] );  // 저장된 클라이언트의 접속정보.

        if ( Simple.aContext <> nil ) And
           ( Simple.aContext.Connection <> nil ) And
           ( Simple.aContext.Connection.IOHandler <> nil ) And
           ( Simple.aContext.Connection.IOHandler.connected ) then
        begin
          Simple.aContext.Connection.IOHandler.Write( Simple.data + #02#$d#$a );

          sData.Add('Write = ' + Simple.data );

          Simple.aContext := nil;
          Simple.data := '';
          Simple.Free;
        end;
      Finally
        Save_Context.Delete( 0 );  // 모든 처리가 끝났으니 바2바2
        tCS.Leave;
      End;

    finally
      WaitForSingleObject( Handle, 10 );
      Application.ProcessMessages;
    end;
  End;

end;

procedure TWriteThread.SetIndy(const Value: TIdTCPClient);
begin
 FIdClient := Value;
end;

procedure TWriteThread.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

procedure TWriteThread.SetsData(const Value: TStringList);
begin
  FsData := Value;
end;

procedure TWriteThread.SettCS(const Value: TCriticalSection);
begin
  FtCS := Value;
end;

procedure TWriteThread.SetSave_Context(const Value: Tlist);
begin
  FSave_Context := Value;
end;

{ TDisplayThread }

constructor TDisplayThread.Create;
begin
  inherited Create( True );
end;

destructor TDisplayThread.Destroy;
begin

  inherited;
end;

procedure TDisplayThread.Display;
var
  I: Integer;
begin
  if sData.Count = 0 then exit;

  With ListBox do
  begin
    Items.Add( 'sData Count = ' + IntToStr( sData.Count ) );
    for I := 0 to sData.Count - 1 do
    begin
      Items.Add(  sData[i] );
      ItemIndex := Count -1;
    end;

    sData.Clear;
  end;

end;

procedure TDisplayThread.Execute;
begin
  while not Terminated do
  begin
    tCS.Enter;
    try
      Synchronize(  Display );
    finally
      tCS.Leave;
    end;
    Application.ProcessMessages;
    WaitForSingleObject( Handle, 10 );
  end;

end;

procedure TDisplayThread.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

procedure TDisplayThread.SetsData(const Value: TStringList);
begin
  FsData := Value;
end;

procedure TDisplayThread.SettCS(const Value: TCriticalSection);
begin
  FtCS := Value;
end;

procedure TForm3.BTN_ServerStartClick(Sender: TObject);
begin
  IdTcpServer1.Active := True;
  BTN_ServerStart.Enabled := false;

  Write_Thread.Resume;
  Display_Thread.Resume;
end;

procedure TForm3.BTN_ServerStopClick(Sender: TObject);
var
  i: Integer;
  list : TList;
begin
  if not write_Thread.Suspended then
    write_Thread.Suspend;
  if not Display_Thread.Suspended then
    Display_Thread.Suspend;

  IdTcpServer1.Active := False;

  list := IdTcpServer1.Contexts.LockList;
  try
    Try
      for i := 0 to list.Count - 1 do
      begin
        TIdContext( list[i] ).Connection.Disconnect;
      end;
    except
    End;
  finally
    IdTcpServer1.Contexts.UnlockList;
  end;

  BTN_ServerStart.Enabled := True;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  BTN_ServerStop.Click;

  if write_Thread.Suspended then
    write_Thread.Resume;
  write_Thread.Terminate;

  if Display_Thread.Suspended then
    Display_Thread.Resume;
  Display_Thread.Terminate;

  write_Thread.WaitFor;
  write_Thread.Free;

  Display_Thread.WaitFor;
  Display_Thread.Free;

  IdTCPServer1.Active := False;

  CTS_Lock.Free;

  for i := 0 to Save_Context.Count - 1 do
    TSimple( Save_Context[i] ).Free;

  Save_Context.Free;

  sData.Free;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  CTS_Lock := TCriticalSection.Create;
  sData := TStringList.Create;
  Save_Context := TList.Create;


  Write_Thread          := TWriteThread.Create;
  Write_Thread.tCS      := CTS_Lock;
  Write_Thread.sData    := sData;
  Write_Thread.Save_Context := Save_Context;
  Write_Thread.ListBox  := ListBox1;

  Display_Thread          := TDisplayThread.Create;
  Display_Thread.tCS      := CTS_Lock;
  Display_Thread.sData    := sData;
  Display_Thread.ListBox  := ListBox1;
end;

procedure TForm3.IdTCPServer1Execute(AContext: TIdContext);
var
  CMD : String;
  UP_Ver, UP_FileName : String;
  simple : TSimple;
begin
  CMD := '';
  If AContext.Connection.IOHandler.Connected Then
    CMD := AContext.Connection.IOHandler.ReadLn( #02#$d#$a, 500 );

  if CMD = '' then Exit;

  try

    simple := TSimple.Create;
    simple.AContext := aContext;
    simple.bRead    := True;
    simple.data := CMD;

    CTS_Lock.Enter;
    Save_Context.add( simple );

    sData.Add( 'Read Data = ' + CMD );
    sData.Add( 'Read Data Size: ' + inttostr(length(CMD)) );

  Finally
    CTS_Lock.Leave;
  end;

  WaitForSingleObject( handle, 1);
end;

end.
