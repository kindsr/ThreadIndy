unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, System.SyncObjs,
  IdTCPConnection, IdTCPClient, Vcl.StdCtrls;


type
  TWriteThread = class(TThread)
  private
    FData: TStringList;
    FIdClient: TIdTCPClient;
    FCnt: Integer;
    FCS: TCriticalSection;
    procedure SetData(const Value: TStringList);
    procedure SetIdClient(const Value: TIdTCPClient);
    procedure SetCnt(const Value: Integer);
    procedure SetCS(const Value: TCriticalSection);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Data : TStringList read FData write SetData;
    property IdClient : TIdTCPClient read FIdClient write SetIdClient;
    property Cnt : Integer read FCnt write SetCnt;
    property CS : TCriticalSection read FCS write SetCS;
  end;

  TReadThread = class(TThread)
  private
    FData: TStringList;
    FIdClient : TIdTCPClient;
    FCS: TCriticalSection;
    procedure SetData(const Value: TStringList);
    procedure SetIndy(const Value: TIdTCPClient);
    procedure SetCS(const Value: TCriticalSection);
  protected

    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Data : TStringList read FData write SetData;
    property IdClient : TIdTCPClient read FIdClient write SetIndy;
    property CS : TCriticalSection read FCS write SetCS;
  end;

type
  TDisplayThread = class(TThread)
  private
    FData: TStringList;
    FListBox: TListBox;
    FCS: TCriticalSection;
    procedure SetListBox(const Value: TListBox);
    procedure SetData(const Value: TStringList);
    procedure SetCS(const Value: TCriticalSection);
  protected

    procedure Display;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ListBox : TListBox read FListBox write SetListBox;
    property Data : TStringList read FData write SetData;
    property CS : TCriticalSection read FCS write SetCS;
  end;

type
  TForm2 = class(TForm)
    IdTCPClient1: TIdTCPClient;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slData : TStringList;
    iCnt : Integer;
    cs : TCriticalSection;
    Read_Thread : TReadThread;
    write_Thread : TWriteThread;
    Display_Thread : TDisplayThread;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


{ TWriteThread }

constructor TWriteThread.Create;
begin
  FreeOnTerminate := False;
  Cnt := 0;
  inherited Create( true );
end;

destructor TWriteThread.Destroy;
begin
  inherited;
end;

procedure TWriteThread.Execute;
var
cmd : String;
begin
  if not IdClient.Connected Then  exit;

  while not Terminated do
  begin

    IdClient.IOHandler.CheckForDisconnect(True, True);

    inc( FCnt );

    IdClient.IOHandler.Write(Inttostr( Cnt )+#02#$d#$a );

    CS.Enter;
    try
      Data.Add( 'Write = ' + IntToStr( FCnt ) );
    finally
      CS.Leave;
    end;

    Application.ProcessMessages;
    WaitForSingleObject( Handle, 10 );
  end;

end;

procedure TWriteThread.SetCnt(const Value: Integer);
begin
  FCnt := Value;
end;

procedure TWriteThread.SetIdClient(const Value: TIdTCPClient);
begin
  FIdClient := Value;
end;


procedure TWriteThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TWriteThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
end;

{ TReadThread }

constructor TReadThread.Create;
begin
  FreeOnTerminate := False;
  inherited Create( True );
end;

destructor TReadThread.Destroy;
begin
  inherited;
end;

procedure TReadThread.Execute;
var
  t : Cardinal;
  procedure AddData( aStr : String );
  begin
    CS.Enter;
    try
      Data.Add( aStr );
    finally
      CS.Leave;
    end;
  end;
begin
  while not Terminated do
  try
    t := GetTickCount;

    repeat
      IdClient.IOHandler.CheckForDisconnect(True, True);
      IdClient.IOHandler.CheckForDataOnSource( 100 );
      if GetTickCount - t > 3000 then
      begin
        AddData( '>>>>>>>>>>>>>>>>>>> Read Time Out <<<<<<<<<<<<<<<<<<<<' );
        Break;
      end;

      WaitForSingleObject( Handle, 10 );
      Application.ProcessMessages;

    until IdClient.IOHandler.InputBuffer.Size > 0;

    if GetTickCount - t > 3000 then
      Continue;


    CS.Enter;
    try
      Data.Add('Read = ' + IdClient.IOHandler.ReadLn);
    finally
      CS.Leave;
    end;

  finally
    WaitForSingleObject( Handle, 10 );
    Application.ProcessMessages;
  end;

end;

procedure TReadThread.SetIndy(const Value: TIdTCPClient);
begin
  FIdClient := Value;
end;


procedure TReadThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TReadThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
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
  With ListBox do
  begin
    Items.Add( 'Data Count = ' + IntToStr( Data.Count ) );
    for I := 0 to Data.Count - 1 do
    begin
      Items.Add(  Data[i] );
      ItemIndex := Count -1;
    end;

    Data.Clear;
  end;

end;

procedure TDisplayThread.Execute;
begin
  while not Terminated do
  begin
    CS.Enter;
    try
      Synchronize(  Display );
    finally
      CS.Leave;
    end;
    Application.ProcessMessages;
    WaitForSingleObject( Handle, 10 );
  end;

end;

procedure TDisplayThread.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

procedure TDisplayThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TDisplayThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ListBox1.Clear;
  if not IdTCPClient1.Connected then
    IdTCPClient1.Connect;

  Read_Thread.CS       := cs;
  Read_Thread.IdClient  := IdTCPClient1;
  Read_Thread.Data     := slData;

  write_Thread.CS      := cs;
  write_Thread.IdClient := IdTCPClient1;
  write_Thread.Data     := slData;

  Display_Thread.ListBox  := ListBox1;
  Display_Thread.CS      := cs;
  Display_Thread.Data     := slData;

  write_Thread.Resume;
  Read_Thread.Resume;
  Display_Thread.Resume;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if not Read_Thread.Suspended then
    Read_Thread.Suspend;
  if not write_Thread.Suspended then
    write_Thread.Suspend;
  if not Display_Thread.Suspended then
    Display_Thread.Suspend;

//  IdTCPClient1.Disconnect;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  cs := TCriticalSection.Create;
  Read_Thread := TReadThread.Create;
  write_Thread := TWriteThread.Create;
  Display_Thread := TDisplayThread.Create;
  slData := TStringList.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if write_Thread.Suspended then
    write_Thread.Resume;
  write_Thread.Terminate;

  if Read_Thread.Suspended then
    Read_Thread.Resume;
  Read_Thread.Terminate;

  if Display_Thread.Suspended then
    Display_Thread.Resume;
  Display_Thread.Terminate;

  write_Thread.WaitFor;
  Read_Thread.WaitFor;
  Display_Thread.WaitFor;

  Read_Thread.Free;
  write_Thread.Free;
  Display_Thread.Free;
  cs.Free;
  slData.free;
end;

end.
