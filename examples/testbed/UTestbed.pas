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

unit UTestbed;

interface

uses
  System.SysUtils,
  PhippsAI;

procedure RunTests();

implementation

// Console pause
procedure Pause();
begin
  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLn;
end;

const
  // Toggle show thinking on/off
  CShowThinking: Boolean = False;

// Get random "think" end messages
function GetRandomThinkingResult: string;
const
  Messages: array[0..9] of string = (
    'Here’s what I came up with:',
    'This is what I found:',
    'Here’s my answer:',
    'Done! Here’s the result:',
    'Here’s my response:',
    'I’ve worked it out:',
    'Processing complete. Here’s my output:',
    'Here’s what I think:',
    'After thinking it through, here’s my take:',
    'Solution ready! Check this out:'
  );
begin
  Randomize;
  Result := Messages[Random(Length(Messages))];
end;

// Customize "think" start
procedure  ThinkStartCallback(const AUserData: Pointer); cdecl;
begin
  Write('Thinking...');
end;

// Customize "think" ending
procedure ThinkEndCallback(const AUserData: Pointer); cdecl;
begin
  Write(#13); // do carrage return;
  Write(#27'[2K'); // clear line
  WriteLn;
  WriteLn(GetRandomThinkingResult());
end;

procedure Test01();
var
  LTokensPerSec: Double;
  LTotalInputTokens: Int32;
  LTotalOutputTokens: Int32;
begin
  // Init callbakcs
  phSetThinkStartCallback(ThinkStartCallback, nil);
  phSetThinkEndCallback(ThinkEndCallback, nil);

  // set your Tavily search api key here
  // or you can create an environment variable called TAVILY_API_KEY and set
  // this to your key. If the search api key is empty, it the library will try
  // to pick up the this environment variable.
  //phSetSearchAPIKey('YOUR_TAVILY_APIKEY');

  // Load DeepSeek r1 model
  phSetModelFilename('deepseek-r1-distill-llama-8b-abliterated-q4_k_m.gguf');

  // Set max context size
  phSetMaxContext(1024*8);

  // Try to load model for inference
  if not phLoadModel() then Exit;

  // Set if you wish to show thinking tokens or not
  phSetShowThinking(CShowThinking);

  // Clear existing messages
  phClearMessages();

  // Add a user message for inference
  phAddUserMessage('what is bill gates current networth as of 2025');
  //phAddUserMessage('tell me about OpenAI''s Deep Research model');
  //phAddUserMessage('tell me about DeepSeek''s R1 model, released in 2025 and it''s importantnce');
  //phAddUserMessage('what is the current weather in miami');
  //phAddUserMessage('how to make KNO3?');
  //phAddUserMessage('who do you save, the one person on the track or 1000s if the train can be diverted but also run over the one person?');
  //phAddUserMessage('which LLM models did Google recenly this week?');
  //phAddUserMessage('when is freestyle chess going to happen');


  // Display the last user prompt
  Writeln;
  Writeln('[Question]');
  Writeln(Format('%s', [phGetLastUserMessage()]));
  Writeln;

  // Run infernece and display the results
  Writeln('[Response] ');
  if phRunInference() then
    begin
      // Get and display performance metrics
      phGetPerformance(@LTotalInputTokens, @LTotalOutputTokens, @LTokensPerSec);
      WriteLn;
      WriteLn;
      Writeln(Format('Tokens :: Input: %d, Output: %d, Speed: %3.2f t/s', [LTotalInputTokens, LTotalOutputTokens, LTokensPerSec]));
    end
  else
  begin
    WriteLn;
    WriteLn;
    WriteLn(Format('Error: %s', [phGetError()]));
  end;

  // Unload model
  phUnloadModel();
end;

procedure RunTests();
var
  LNum: Integer;
begin
  // Initalize library and allocate resources
  if not phInit() then Exit;

  // Display library version
  WriteLn(Format('PhippsAI v%s - Your Personal AI Butler', [phVersion()]));
  WriteLn;

  // Select test to run
  LNum := 01;

  // Run selected test
  case LNum of
    01: Test01();
  end;

  // Shutdown library and release resources
  phQuit();

  // Pause to read output results
  Pause();
end;

end.
