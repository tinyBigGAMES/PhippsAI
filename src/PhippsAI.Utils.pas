{===============================================================================
  ___  _     _                    _    ___ ™
 | _ \| |_  (_) _ __  _ __  ___  /_\  |_ _|
 |  _/| ' \ | || '_ \| '_ \(_-< / _ \  | |
 |_|  |_||_||_|| .__/| .__//__//_/ \_\|___|
               |_|   |_|
         Your Personal AI Butler

 Copyright © 2025-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/PhippsAI

 See LICENSE file for license information
===============================================================================}

unit PhippsAI.Utils;

{$I PhippsAI.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.NetConsts,
  System.RegularExpressions,
  System.Generics.Collections;

const
  LF   = #10;
  CR   = #13;
  CRLF = LF+CR;
  ESC  = #27;

  VK_ESC = 27;

  // Cursor Movement
  CSICursorPos = ESC + '[%d;%dH';         // Set cursor position
  CSICursorUp = ESC + '[%dA';             // Move cursor up
  CSICursorDown = ESC + '[%dB';           // Move cursor down
  CSICursorForward = ESC + '[%dC';        // Move cursor forward
  CSICursorBack = ESC + '[%dD';           // Move cursor backward
  CSISaveCursorPos = ESC + '[s';          // Save cursor position
  CSIRestoreCursorPos = ESC + '[u';       // Restore cursor position

  // Cursor Visibility
  CSIShowCursor = ESC + '[?25h';          // Show cursor
  CSIHideCursor = ESC + '[?25l';          // Hide cursor
  CSIBlinkCursor = ESC + '[?12h';         // Enable cursor blinking
  CSISteadyCursor = ESC + '[?12l';        // Disable cursor blinking

  // Screen Manipulation
  CSIClearScreen = ESC + '[2J';           // Clear screen
  CSIClearLine = ESC + '[2K';             // Clear line
  CSIScrollUp = ESC + '[%dS';             // Scroll up by n lines
  CSIScrollDown = ESC + '[%dT';           // Scroll down by n lines

  // Text Formatting
  CSIBold = ESC + '[1m';                  // Bold text
  CSINormal = ESC + '[22m';               // Normal text
  CSIUnderline = ESC + '[4m';             // Underline text
  CSIResetFormat = ESC + '[0m';           // Reset text formatting
  CSIResetBackground = #27'[49m';         // Reset background text formatting
  CSIResetForeground = #27'[39m';         // Reset forground text formatting
  CSIInvertColors = ESC + '[7m';          // Invert foreground/background
  CSINormalColors = ESC + '[27m';         // Normal colors

  CSIDim = ESC + '[2m';
  CSIItalic = ESC + '[3m';
  CSIBlink = ESC + '[5m';
  CSIFramed = ESC + '[51m';
  CSIEncircled = ESC + '[52m';

  // Text Modification
  CSIInsertChar = ESC + '[%d@';           // Insert n spaces at cursor position
  CSIDeleteChar = ESC + '[%dP';           // Delete n characters at cursor position
  CSIEraseChar = ESC + '[%dX';            // Erase n characters at cursor position

  // Colors (Foreground and Background)
  CSIFGBlack = ESC + '[30m';
  CSIFGRed = ESC + '[31m';
  CSIFGGreen = ESC + '[32m';
  CSIFGYellow = ESC + '[33m';
  CSIFGBlue = ESC + '[34m';
  CSIFGMagenta = ESC + '[35m';
  CSIFGCyan = ESC + '[36m';
  CSIFGWhite = ESC + '[37m';

  CSIBGBlack = ESC + '[40m';
  CSIBGRed = ESC + '[41m';
  CSIBGGreen = ESC + '[42m';
  ySIBGYellow = ESC + '[43m';
  CSIBGBlue = ESC + '[44m';
  CSIBGMagenta = ESC + '[45m';
  CSIBGCyan = ESC + '[46m';
  CSIBGWhite = ESC + '[47m';

  CSIFGBrightBlack = ESC + '[90m';
  CSIFGBrightRed = ESC + '[91m';
  CSIFGBrightGreen = ESC + '[92m';
  CSIFGBrightYellow = ESC + '[93m';
  CSIFGBrightBlue = ESC + '[94m';
  CSIFGBrightMagenta = ESC + '[95m';
  CSIFGBrightCyan = ESC + '[96m';
  CSIFGBrightWhite = ESC + '[97m';

  CSIBGBrightBlack = ESC + '[100m';
  CSIBGBrightRed = ESC + '[101m';
  CSIBGBrightGreen = ESC + '[102m';
  CSIBGBrightYellow = ESC + '[103m';
  CSIBGBrightBlue = ESC + '[104m';
  CSIBGBrightMagenta = ESC + '[105m';
  CSIBGBrightCyan = ESC + '[106m';
  CSIBGBrightWhite = ESC + '[107m';

  CSIFGRGB = ESC + '[38;2;%d;%d;%dm';        // Foreground RGB
  CSIBGRGB = ESC + '[48;2;%d;%d;%dm';        // Backg


function  GetCurrentDLLFilename(): string;
procedure GetConsoleSize(AWidth: PInteger; AHeight: PInteger);
function  AsUTF8(const AText: string): Pointer;
function  ContainsText(const AText, ASubText: string): Boolean;
function  CapitalizeFirstChar(const AText: string): string;
function  GetPhysicalProcessorCount(): DWORD;
function  SanitizeToJson(const aText: string): string;
function  SanitizeFromJson(const aText: string): string;
function  HasConsoleOutput(): Boolean;
function  EnableVirtualTerminalProcessing(): DWORD;
procedure Pause();

procedure Print(const AMsg: string); overload;
procedure PrintLn(const AMsg: string); overload;
procedure Print(const AMsg: string; const AArgs: array of const); overload;
procedure PrintLn(const AMsg: string; const AArgs: array of const); overload;
procedure Print(); overload;
procedure PrintLn(); overload;

procedure GetCursorPos(X, Y: PInteger);
procedure SetCursorPos(const X, Y: Integer);
procedure SetCursorVisible(const AVisible: Boolean);
procedure HideCursor();
procedure ShowCursor();
procedure SaveCursorPos();
procedure RestoreCursorPos();
procedure MoveCursorUp(const ALines: Integer);
procedure MoveCursorDown(const ALines: Integer);
procedure MoveCursorForward(const ACols: Integer);
procedure MoveCursorBack(const ACols: Integer);

procedure ClearScreen();
procedure ClearLine();
procedure ClearLineFromCursor(const AColor: string);

procedure SetBoldText(const ABold: Boolean);
procedure ResetTextFormat();
procedure SetForegroundColor(const AColor: string);
procedure SetBackgroundColor(const AColor: string);
procedure SetForegroundRGB(const ARed, AGreen, ABlue: Byte);
procedure SetBackgroundRGB(const ARed, AGreen, ABlue: Byte);

function  HasEnoughDiskSpace(const APath: string; ARequiredSpace: Int64): Boolean;
function  CreateJSONFromText(const JSONText: string): TJSONObject;
function  ExtractFunctionCallJSON(const InputText: string): string;
function  CleanAndConvertJSON(const InputJSON: string): string;
function  NormalizeJSONArguments(const JSONStr: string): string;
function  GetEnvVarValue(const AVarName: string): string;

type
  TFuncArgs = TDictionary<string, string>;

function IsFunctionCall(const JSONStr: string; out FuncName: string; out Args: TFuncArgs): Boolean;
function CreateFunctionResponseJSON(const FunctionName, Content: string): string;

function  TavilyWebSearch(const AAPIKey, AQuery: string): string;

type
  { TCallback<T> }
  TCallback<T> = record
    Handler: T;
    UserData: Pointer;
  end;

  { TTokenResponse }

  // AddToken return messages - for TResponse.AddToken
  //  paWait = No new (full) words, just wait for more incoming tokens
  //  Append = Append existing line with latest word
  //  NewLine = start new line then print the latest word
  TTokenPrintAction = (tpaWait, tpaAppend, tpaNewline);

  { TResponse
    Helper to handle incoming tokens during streaming
      Example uses:
      - Tabulate tokens into full words based on wordbreaks
      - Control wordwrap/linechanges for console or custom GUI without wordwrap functionality
        (Does change the print resolution from Token to logical words)
  }
  TTokenResponse = record
  private
    FRaw: string;                  // Full response as is
    FTokens: array of string;      // Actual tokens
    FMaxLineLength: Integer;       // Define confined space, in chars for fixed width font
    FWordBreaks: array of char;    // What is considered a logical word-break
    FLineBreaks: array of char;    // What is considered a logical line-break
    FWords: array of String;       // Response but as array of "words"
    FWord: string;                // Current word accumulating
    FLine: string;                // Current line accumulating
    FFinalized: Boolean;          // Know the finalization is done
    FRightMargin: Integer;
    function HandleLineBreaks(const AToken: string): Boolean;
    function SplitWord(const AWord: string; var APrefix, ASuffix: string): Boolean;
    function GetLineLengthMax(): Integer;
  public
    class operator Initialize (out ADest: TTokenResponse);
    property RightMargin: Integer read FRightMargin;
    property MaxLineLength: Integer read FMaxLineLength;
    procedure SetRightMargin(const AMargin: Integer);
    procedure SetMaxLineLength(const ALength: Integer);
    function AddToken(const aToken: string): TTokenPrintAction;
    function LastWord(const ATrimLeft: Boolean=False): string;
    function Finalize: Boolean;
  end;

  { TBaseObject }
  TBaseObject = class
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

implementation

var
  FMarshaller: TMarshaller;

function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: LPCSTR; var phModule: HMODULE): BOOL; stdcall; external 'kernel32.dll' name 'GetModuleHandleExA';

function GetCurrentDLLFilename(): string;
const
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS = $00000004;
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = $00000002;
var
  ModuleName: array[0..MAX_PATH] of Char;
  ModuleHandle: HMODULE;

begin
  ModuleHandle := 0;
  if GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, @GetCurrentDLLFilename, ModuleHandle) then
  begin
    if GetModuleFileName(ModuleHandle, ModuleName, SizeOf(ModuleName)) > 0 then
      Result := ModuleName
    else
      Result := '';
    FreeLibrary(ModuleHandle);  // Decrement the reference count
  end
  else
    Result := '';
end;

procedure GetConsoleSize(AWidth: PInteger; AHeight: PInteger);
var
  LConsoleInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), LConsoleInfo);
  if Assigned(AWidth) then
    AWidth^ := LConsoleInfo.dwSize.X;

  if Assigned(AHeight) then
  AHeight^ := LConsoleInfo.dwSize.Y;
end;

function AsUTF8(const AText: string): Pointer;
begin
  Result := FMarshaller.AsUtf8(AText).ToPointer;
end;

function ContainsText(const AText, ASubText: string): Boolean;
begin
  Result := Pos(UpperCase(ASubText), UpperCase(AText)) > 0;
end;

function CapitalizeFirstChar(const AText: string): string;
begin
  if AText = '' then
    Exit(AText); // Return an empty string if the input is empty
  Result := UpperCase(AText[1]) + Copy(AText, 2, Length(AText) - 1);
end;

function GetPhysicalProcessorCount(): DWORD;
var
  BufferSize: DWORD;
  Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  ProcessorInfo: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  Offset: DWORD;
begin
  Result := 0;
  BufferSize := 0;

  // Call GetLogicalProcessorInformation with buffer size set to 0 to get required buffer size
  if not GetLogicalProcessorInformation(nil, BufferSize) and (WinApi.Windows.GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
  begin
    // Allocate buffer
    GetMem(Buffer, BufferSize);
    try
      // Call GetLogicalProcessorInformation again with allocated buffer
      if GetLogicalProcessorInformation(Buffer, BufferSize) then
      begin
        ProcessorInfo := Buffer;
        Offset := 0;

        // Loop through processor information to count physical processors
        while Offset + SizeOf(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= BufferSize do
        begin
          if ProcessorInfo.Relationship = RelationProcessorCore then
            Inc(Result);

          Inc(ProcessorInfo);
          Inc(Offset, SizeOf(SYSTEM_LOGICAL_PROCESSOR_INFORMATION));
        end;
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function  SanitizeToJson(const aText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(aText) do
  begin
    case aText[i] of
      '\': Result := Result + '\\';
      '"': Result := Result + '\"';
      '/': Result := Result + '\/';
      #8:  Result := Result + '\b';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        Result := Result + aText[i];
    end;
  end;
  Result := Result;
end;

function  SanitizeFromJson(const aText: string): string;
var
  LText: string;
begin
  LText := aText;
  LText := LText.Replace('\n', #10);
  LText := LText.Replace('\r', #13);
  LText := LText.Replace('\b', #8);
  LText := LText.Replace('\t', #9);
  LText := LText.Replace('\f', #12);
  LText := LText.Replace('\/', '/');
  LText := LText.Replace('\"', '"');
  LText := LText.Replace('\\', '\');
  Result := LText;
end;

function  HasConsoleOutput(): Boolean;
var
  LStdOut: THandle;
  LMode: DWORD;
begin
  LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  Result := (LStdOut <> INVALID_HANDLE_VALUE) and GetConsoleMode(LStdOut, LMode);
end;

function EnableVirtualTerminalProcessing(): DWORD;
var
  HOut: THandle;
  LMode: DWORD;
begin
  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then
  begin
    Result := GetLastError;
    Exit;
  end;

  if not GetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  Result := 0;  // Success
end;

procedure Pause();
begin
  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLn;
end;

procedure Print(const AMsg: string);
begin
  if not HasConsoleOutput() then Exit;
  Write(AMsg+CSIResetFormat);
end;

procedure PrintLn(const AMsg: string);
begin
  if not HasConsoleOutput() then Exit;
  WriteLn(AMsg+CSIResetFormat);
end;

procedure Print(const AMsg: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(AMsg, AArgs)+CSIResetFormat);
end;

procedure PrintLn(const AMsg: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  WriteLn(Format(AMsg, AArgs)+CSIResetFormat);
end;

procedure Print();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIResetFormat);
end;

procedure PrintLn();
begin
  if not HasConsoleOutput() then Exit;
  WriteLn(CSIResetFormat);
end;

procedure GetCursorPos(X, Y: PInteger);
var
  hConsole: THandle;
  BufferInfo: TConsoleScreenBufferInfo;
begin
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE then
    Exit;

  if not GetConsoleScreenBufferInfo(hConsole, BufferInfo) then
    Exit;

  if Assigned(X) then
    X^ := BufferInfo.dwCursorPosition.X;
  if Assigned(Y) then
    Y^ := BufferInfo.dwCursorPosition.Y;
end;

procedure SetCursorPos(const X, Y: Integer);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSICursorPos, [X, Y]));
end;

procedure SetCursorVisible(const AVisible: Boolean);
var
  ConsoleInfo: TConsoleCursorInfo;
  ConsoleHandle: THandle;
begin
  ConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  ConsoleInfo.dwSize := 25; // You can adjust cursor size if needed
  ConsoleInfo.bVisible := AVisible;
  SetConsoleCursorInfo(ConsoleHandle, ConsoleInfo);
end;

procedure HideCursor();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIHideCursor);
end;

procedure ShowCursor();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIShowCursor);
end;

procedure SaveCursorPos();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSISaveCursorPos);
end;

procedure RestoreCursorPos();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIRestoreCursorPos);
end;

procedure MoveCursorUp(const ALines: Integer);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSICursorUp, [ALines]));
end;

procedure MoveCursorDown(const ALines: Integer);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSICursorDown, [ALines]));
end;

procedure MoveCursorForward(const ACols: Integer);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSICursorForward, [ACols]));
end;

procedure MoveCursorBack(const ACols: Integer);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSICursorBack, [ACols]));
end;

procedure ClearScreen();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIClearScreen);
  SetCursorPos(0, 0);
end;

procedure ClearLine();
begin
  if not HasConsoleOutput() then Exit;
  Write(CR);
  Write(CSIClearLine);
end;

procedure ClearLineFromCursor(const AColor: string);
var
  LConsoleOutput: THandle;
  LConsoleInfo: TConsoleScreenBufferInfo;
  LNumCharsWritten: DWORD;
  LCoord: TCoord;
begin
  LConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);

  if GetConsoleScreenBufferInfo(LConsoleOutput, LConsoleInfo) then
  begin
    LCoord.X := 0;
    LCoord.Y := LConsoleInfo.dwCursorPosition.Y;

    Print(AColor, []);
    FillConsoleOutputCharacter(LConsoleOutput, ' ', LConsoleInfo.dwSize.X
      - LConsoleInfo.dwCursorPosition.X, LCoord, LNumCharsWritten);
    SetConsoleCursorPosition(LConsoleOutput, LCoord);
  end;
end;

procedure SetBoldText(const ABold: Boolean);
begin
  if not HasConsoleOutput() then Exit;
  if ABold then
    Write(CSIBold)
  else
    Write(CSINormal);
end;

procedure ResetTextFormat();
begin
  if not HasConsoleOutput() then Exit;
  Write(CSIResetFormat);
end;

procedure SetForegroundColor(const AColor: string);
begin
  if not HasConsoleOutput() then Exit;
  Write(AColor);
end;

procedure SetBackgroundColor(const AColor: string);
begin
  if not HasConsoleOutput() then Exit;
  Write(AColor);
end;

procedure SetForegroundRGB(const ARed, AGreen, ABlue: Byte);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSIFGRGB, [ARed, AGreen, ABlue]));
end;

procedure SetBackgroundRGB(const ARed, AGreen, ABlue: Byte);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(CSIBGRGB, [ARed, AGreen, ABlue]));
end;

function HasEnoughDiskSpace(const APath: string; ARequiredSpace: Int64): Boolean;
var
  LFreeAvailable: Int64;
  LTotalSpace: Int64;
  LTotalFree: Int64;
begin
  Result := GetDiskFreeSpaceEx(PChar(APath), LFreeAvailable, LTotalSpace, @LTotalFree) and
            (LFreeAvailable >= ARequiredSpace);
end;

function ExtractFunctionCallJSON(const InputText: string): string;
var
  i, StartPos, EndPos, Counter: Integer;
  c: Char;
  Candidate: string;
begin
  Result := '';
  i := 1;
  while i <= Length(InputText) do
  begin
    if InputText[i] = '{' then
    begin
      StartPos := i;
      Counter := 0;
      for EndPos := StartPos to Length(InputText) do
      begin
        c := InputText[EndPos];
        if c = '{' then
          Inc(Counter)
        else if c = '}' then
        begin
          Dec(Counter);
          if Counter = 0 then
          begin
            Candidate := Copy(InputText, StartPos, EndPos - StartPos + 1);
            if Pos('"function_call":', Candidate) > 0 then
            begin
              Result := Candidate;
              Exit;
            end;
            i := EndPos;
            Break;
          end;
        end;
      end;
    end;
    Inc(i);
  end;
end;

function CleanAndConvertJSON(const InputJSON: string): string;
var
  Root, FunctionCall, ParsedArgs: TJSONObject;
  ArgumentsValue: TJSONValue;
  Temp: string;
begin
  Root := TJSONObject.ParseJSONValue(InputJSON) as TJSONObject;
  if Root = nil then
    Exit('');
  try
    if Root.TryGetValue('function_call', FunctionCall) then
    begin
      if FunctionCall.TryGetValue('arguments', ArgumentsValue) and (ArgumentsValue is TJSONString) then
      begin
        ParsedArgs := TJSONObject.ParseJSONValue(TJSONString(ArgumentsValue).Value) as TJSONObject;
        if Assigned(ParsedArgs) then
        begin
          FunctionCall.RemovePair('arguments').Free;
          FunctionCall.AddPair('arguments', ParsedArgs);
        end;
      end;
    end;
    Temp := Root.ToString;
    Temp := StringReplace(Temp, sLineBreak, '', [rfReplaceAll]);
    Temp := StringReplace(Temp, #13, '', [rfReplaceAll]);
    Temp := StringReplace(Temp, #10, '', [rfReplaceAll]);
    Result := Temp;
  finally
    Root.Free;
  end;
end;

function NormalizeJSONArguments(const JSONStr: string): string;
var
  JSONObject, FunctionCallObject: TJSONObject;
  ArgumentsValue: TJSONValue;
  ArgumentsString: string;
begin
  Result := JSONStr;
  JSONObject := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  if not Assigned(JSONObject) then
    Exit;

  try
    if JSONObject.TryGetValue<TJSONObject>('function_call', FunctionCallObject) then
    begin
      if FunctionCallObject.TryGetValue<TJSONValue>('arguments', ArgumentsValue) then
      begin
        // Check if arguments are already a string; if not, convert to string
        if not (ArgumentsValue is TJSONString) then
        begin
          ArgumentsString := ArgumentsValue.ToJSON; // Convert object to a JSON string
          FunctionCallObject.RemovePair('arguments').Free; // Remove old object
          FunctionCallObject.AddPair('arguments', TJSONString.Create(ArgumentsString)); // Store as string
          Result := JSONObject.ToJSON;
        end;
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

function IsFunctionCall(const JSONStr: string; out FuncName: string; out Args: TFuncArgs): Boolean;
var
  JSONObject, FunctionCallObject, ArgumentsObject: TJSONObject;
  ArgumentsString: string;
  FunctionNameValue: TJSONValue;
  Pair: TJSONPair;
  LJsonStr: string;
begin
  Result := False;
  FuncName := '';
  Args := TFuncArgs.Create;
  JSONObject := nil;
  ArgumentsObject := nil;

  LJsonStr := JSONStr;

  try
    JSONObject := TJSONObject.ParseJSONValue(LJsonStr) as TJSONObject;
    if Assigned(JSONObject) and JSONObject.TryGetValue<TJSONObject>('function_call', FunctionCallObject) then
    begin
      // Extract function name properly
      if FunctionCallObject.TryGetValue<TJSONValue>('name', FunctionNameValue) then
        FuncName := FunctionNameValue.Value;

      // Extract `arguments` as a STRING first
      if FunctionCallObject.TryGetValue<string>('arguments', ArgumentsString) then
      begin
        // Now parse `arguments` string into a new JSON object
        ArgumentsObject := TJSONObject.ParseJSONValue(ArgumentsString) as TJSONObject;
        if Assigned(ArgumentsObject) then
        begin
          for Pair in ArgumentsObject do
          begin
            Args.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
          end;
          Result := True;
        end;
      end;
    end;
  finally
    if Assigned(JSONObject) then
      JSONObject.Free;

    if Assigned(ArgumentsObject) then
      ArgumentsObject.Free;
  end;
end;

function CreateFunctionResponseJSON(const FunctionName, Content: string): string;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('role', 'function');
    //JSONObject.AddPair('role', 'tool');
    JSONObject.AddPair('name', FunctionName);
    JSONObject.AddPair('content', Content);

    Result := JSONObject.ToJSON;
  finally
    JSONObject.Free;
  end;
end;

function CreateJSONFromText(const JSONText: string): TJSONObject;
begin
  try
    // Parse the input JSON text and cast it to TJSONObject
    Result := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;

    // Check if parsing succeeded
    if not Assigned(Result) then
      raise Exception.Create('Invalid JSON format');
  except
    on E: Exception do
    begin
      //Writeln('Error creating JSON object: ', E.Message);
      Result := nil; // Return nil on error
    end;
  end;
end;

function GetEnvVarValue(const AVarName: string): string;
var
  LBufSize: Integer;
begin
  LBufSize := GetEnvironmentVariable(PChar(AVarName), nil, 0);
  if LBufSize > 0 then
    begin
      SetLength(Result, LBufSize - 1);
      GetEnvironmentVariable(PChar(AVarName), PChar(Result), LBufSize);
    end
  else
    Result := '';
end;


function TavilyWebSearch(const AAPIKey, AQuery: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JsonRequest, JsonResponse: TJSONObject;
  StringContent: TStringStream;
  Url: string;
begin
  Result := '';
  HttpClient := THTTPClient.Create;
  try
    // Set the API URL
    Url := 'https://api.tavily.com/search';

    // Create JSON request body
    JsonRequest := TJSONObject.Create;
    try
      JsonRequest.AddPair('api_key', AAPIKey);
      JsonRequest.AddPair('query', AQuery);
      JsonRequest.AddPair('include_answer', 'advanced'); // Include 'include_answer' parameter
      JsonRequest.AddPair('include_answer', True);
      JsonRequest.AddPair('include_images', False);
      JsonRequest.AddPair('include_image_descriptions', False);
      JsonRequest.AddPair('include_raw_content', False);
      JsonRequest.AddPair('max_results', 5);
      JsonRequest.AddPair('include_domains', TJSONArray.Create); // Empty array
      JsonRequest.AddPair('exclude_domains', TJSONArray.Create); // Empty array

      // Convert JSON to string stream
      StringContent := TStringStream.Create(JsonRequest.ToString, TEncoding.UTF8);
      try
        // Set content type to application/json
        HttpClient.ContentType := 'application/json';

        // Perform the POST request
        Response := HttpClient.Post(Url, StringContent);

        // Check if the response is successful
        if Response.StatusCode = 200 then
        begin
          // Parse the JSON response
          JsonResponse := TJSONObject.ParseJSONValue(Response.ContentAsString(TEncoding.UTF8)) as TJSONObject;
          try
            // Extract the 'answer' field from the response
            if JsonResponse.TryGetValue('answer', Result) then
            begin
              // 'Result' now contains the answer from the API
            end
            else
            begin
              raise Exception.Create('The "answer" field is missing in the API response.');
            end;
          finally
            JsonResponse.Free;
          end;
        end
        else
        begin
          raise Exception.CreateFmt('Error: %d - %s', [Response.StatusCode, Response.StatusText]);
        end;
      finally
        StringContent.Free;
      end;
    finally
      JsonRequest.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

{ TTokenResponse }
class operator TTokenResponse.Initialize (out ADest: TTokenResponse);
var
  LSize: Integer;
begin
  // Defaults
  ADest.FRaw := '';
  SetLength(ADest.FTokens, 0);
  SetLength(ADest.FWordBreaks, 0);
  SetLength(ADest.FLineBreaks, 0);
  SetLength(ADest.FWords, 0);
  ADest.FWord := '';
  ADest.FLine := '';
  ADest.FFinalized := False;
  ADest.FRightMargin := 10;

  // If stream output is sent to a destination without wordwrap,
  // the TTokenResponse will find wordbreaks and split into lines by full words

  // Stream is tabulated into full words based on these break characters
  // !Syntax requires at least one!
  SetLength(ADest.FWordBreaks, 4);
  ADest.FWordBreaks[0] := ' ';
  ADest.FWordBreaks[1] := '-';
  ADest.FWordBreaks[2] := ',';
  ADest.FWordBreaks[3] := '.';

  // Stream may contain forced line breaks
  // !Syntax requires at least one!
  SetLength(ADest.FLineBreaks, 2);
  ADest.FLineBreaks[0] := #13;
  ADest.FLineBreaks[1] := #10;


  ADest.SetRightMargin(10);

  GetConsoleSize(@LSize, nil);
  ADest.SetMaxLineLength(LSize);
end;

function TTokenResponse.AddToken(const aToken: string): TTokenPrintAction;
var
  LPrefix, LSuffix: string;
begin
  // Keep full original response
  FRaw := FRaw + aToken;                    // As continuous string
  Setlength(FTokens, Length(FTokens)+1);    // Make space
  FTokens[Length(FTokens)-1] := aToken;     // As an array

  // Accumulate "word"
  FWord := FWord + aToken;

  // If stream contains linebreaks, print token out without added linebreaks
  if HandleLineBreaks(aToken) then
    exit(TTokenPrintAction.tpaAppend)

  // Check if a natural break exists, also split if word is longer than the allowed space
  // and print out token with or without linechange as needed
  else if SplitWord(FWord, LPrefix, LSuffix) or FFinalized then
    begin
      // On last call when Finalized we want access to the line change logic only
      // Bad design (fix on top of a fix) Would be better to separate word slipt and line logic from eachother
      if not FFinalized then
        begin
          Setlength(FWords, Length(FWords)+1);        // Make space
          FWords[Length(FWords)-1] := LPrefix;        // Add new word to array
          FWord := LSuffix;                         // Keep the remainder of the split
        end;

      // Word was split, so there is something that can be printed

      // Need for a new line?
      if Length(FLine) + Length(LastWord) > GetLineLengthMax() then
        begin
          Result  := TTokenPrintAction.tpaNewline;
          FLine   := LastWord;                  // Reset Line (will be new line and then the word)
        end
      else
        begin
          Result  := TTokenPrintAction.tpaAppend;
          FLine   := FLine + LastWord;          // Append to the line
        end;
    end
  else
    begin
      Result := TTokenPrintAction.tpaWait;
    end;
end;

function TTokenResponse.HandleLineBreaks(const AToken: string): Boolean;
var
  LLetter, LLineBreak: Integer;
begin
  Result := false;

  for LLetter := Length(AToken) downto 1 do                   // We are interested in the last possible linebreak
  begin
    for LLineBReak := 0 to Length(Self.FLineBreaks)-1 do       // Iterate linebreaks
    begin
      if AToken[LLetter] = FLineBreaks[LLineBreak] then        // If linebreak was found
      begin
        // Split into a word by last found linechange (do note the stored word may have more linebreak)
        Setlength(FWords, Length(FWords)+1);                          // Make space
        FWords[Length(FWords)-1] := FWord + LeftStr(AToken, Length(AToken)-LLetter); // Add new word to array

        // In case aToken did not end after last LF
        // Word and new line will have whatever was after the last linebreak
        FWord := RightStr(AToken, Length(AToken)-LLetter);
        FLine := FWord;

        // No need to go further
        exit(true);
      end;
    end;
  end;
end;

function TTokenResponse.Finalize: Boolean;
begin
  // Buffer may contain something, if so make it into a word
  if FWord <> ''  then
    begin
      Setlength(FWords, Length(FWords)+1);      // Make space
      FWords[Length(FWords)-1] := FWord;        // Add new word to array
      Self.FFinalized := True;                // Remember Finalize was done (affects how last AddToken-call behaves)
      exit(true);
    end
  else
    Result := false;
end;

function TTokenResponse.LastWord(const ATrimLeft: Boolean): string;
begin
  Result := FWords[Length(FWords)-1];
  if ATrimLeft then
    Result := Result.TrimLeft;
end;

function TTokenResponse.SplitWord(const AWord: string; var APrefix, ASuffix: string): Boolean;
var
  LLetter, LSeparator: Integer;
begin
  Result := false;

  for LLetter := 1 to Length(AWord) do               // Iterate whole word
  begin
    for LSeparator := 0 to Length(FWordBreaks)-1 do   // Iterate all separating characters
    begin
      if AWord[LLetter] = FWordBreaks[LSeparator] then // check for natural break
      begin
        // Let the world know there's stuff that can be a reason for a line change
        Result := True;

        APrefix := LeftStr(AWord, LLetter);
        ASuffix := RightStr(AWord, Length(AWord)-LLetter);
      end;
    end;
  end;

  // Maybe the word is too long but there was no natural break, then cut it to LineLengthMax
  if Length(AWord) > GetLineLengthMax() then
  begin
    Result := True;
    APrefix := LeftStr(AWord, GetLineLengthMax());
    ASuffix := RightStr(AWord, Length(AWord)-GetLineLengthMax());
  end;
end;

(*

function TTokenResponse.GetLineLengthMax(): Integer;
begin
  GetConsoleSize(@Result, nil);
  Result := Result - FRightMargin;
end;

procedure TTokenResponse.SetRightMargin(const AMargin: Integer);
var
  LWidth: Integer;
begin
  GetConsoleSize(@LWidth, nil);
  FRightMargin := EnsureRange(AMargin, 1, LWidth);
end;
*)

function TTokenResponse.GetLineLengthMax(): Integer;
begin
  Result := FMaxLineLength - FRightMargin;
end;

procedure TTokenResponse.SetRightMargin(const AMargin: Integer);
begin
  FRightMargin := AMargin;
end;

procedure TTokenResponse.SetMaxLineLength(const ALength: Integer);
begin
  FMaxLineLength := ALength;
end;

{ TBaseObject }
constructor TBaseObject.Create();
begin
  inherited;
end;

destructor TBaseObject.Destroy();
begin
  inherited;
end;

end.
