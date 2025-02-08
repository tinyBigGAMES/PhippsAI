{===============================================================================
  ___  _     _                    _    ___ �
 | _ \| |_  (_) _ __  _ __  ___  /_\  |_ _|
 |  _/| ' \ | || '_ \| '_ \(_-< / _ \  | |
 |_|  |_||_||_|| .__/| .__//__//_/ \_\|___|
               |_|   |_|
         Your Personal AI Bulter

 Copyright � 2024-present tinyBigGAMES� LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/PhippsAI

 See LICENSE file for license information
===============================================================================}

library PhippsAI;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters.

  Important note about VCL usage: when this DLL will be implicitly
  loaded and this DLL uses TWicImage / TImageCollection created in
  any unit initialization section, then Vcl.WicImageInit must be
  included into your library's USES clause. }

uses
  System.SysUtils,
  System.Classes,
  MemoryDLL in 'MemoryDLL.pas',
  PhippsAI.API in 'PhippsAI.API.pas',
  PhippsAI.CLibs in 'PhippsAI.CLibs.pas',
  PhippsAI.Core in 'PhippsAI.Core.pas',
  PhippsAI.Utils in 'PhippsAI.Utils.pas';

{$R *.res}

begin
end.
