unit MealyFiniteStateMachine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type
  TFSMError = class(Exception);

  TEnterState = procedure( Sender : TComponent; AState : String ) of object;

  { TMealyFiniteStateMachine }

  TMealyFiniteStateMachine = class(TComponent)
  private
    fEnterState0: Boolean;
    fEvents: TStringList;
    fIgnoreInvalid: Boolean;
    fLog: TMemo;
    fOnEnterState: TEnterState;
    fState0: String;
    fStates: TStringList;
    fTransitions    : TStringList;
    vTMap : array of Integer;
    vPMap : array of Integer;
    vCurrentState : Integer;
    NStates      : Integer;
    NEvents      : Integer;
    IsLoaded     : Boolean;
    function GetCurrentState: String;
    procedure SetState0(AValue: String);
    procedure SetTransitions(AValue: TStringList);
    { Private declarations }
  protected
    { Protected declarations }
    type
      TTrans = array[0..3] of String;
    procedure Loaded; override;
    procedure BuildFSM;
    procedure DoEnterState( AState, APostEvent : Integer );
    function  ParseTransition( const theTrans : String; out Trans : TTrans ) : Boolean;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent );  override;
    destructor  Destroy; override;

    procedure Event( theEvent : String );
    procedure ForceState(theState : String);

    function  CurrentStateIs( aState : String ) : Boolean;

    property  CurrentState : String read GetCurrentState;
    procedure LogCurrentState;
  published
    { Published declarations }
    property Events        : TStringList read fEvents;
    property States        : TStringList read fStates;
    property Log           : TMemo       read fLog write fLog;
    property Transitions   : TStringList read fTransitions  write SetTransitions;
    property State0        : String      read fState0       write SetState0;
    property EnterState0   : Boolean     read fEnterState0  write fEnterState0;
    property IgnoreInvalidTransitions : Boolean read fIgnoreInvalid write fIgnoreInvalid;

    property OnEnterState  : TEnterState read fOnEnterState write fOnEnterState;
  end;

procedure Register;

implementation

uses
  Stringsubs, RegExpr, LazLogger;

procedure Register;
begin
  {$I mealyfinitestatemachine_icon.lrs}
  RegisterComponents('Magic',[TMealyFiniteStateMachine]);
end;

{ TMealyFiniteStateMachine }

constructor TMealyFiniteStateMachine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEvents := TStringList.Create;
  fEvents.Sorted := True;
  fEvents.Duplicates := dupIgnore;
  fEvents.CaseSensitive := False;
  fStates := TStringList.Create;
  fStates := TStringList.Create;
  fStates.Sorted := True;
  fStates.Duplicates := dupIgnore;
  fEvents.CaseSensitive := False;
  fTransitions    := TStringList.Create;
  NStates := 0;
  NEvents := 0;
  vCurrentState := -1; // No State to start.
  IsLoaded := False;
  fIgnoreInvalid := True;
end;

destructor TMealyFiniteStateMachine.Destroy;
begin
  inherited Destroy;
  fEvents.Free;
  fStates.Free;
  fTransitions.Free;
  SetLength( vTMap, 0 );
  SetLength( vPMap, 0 );
end;

procedure TMealyFiniteStateMachine.DoEnterState(AState, APostEvent: Integer);
var
  PostEvent : Integer;
begin
  if Assigned( @fOnEnterState ) then
  begin
    PostEvent := vPMap[ vCurrentState * NEvents + APostEvent ];
    vCurrentState := AState;
    DebugLn('Entering State ' + fStates.Strings[AState] );
    fOnEnterState( Self, fStates.Strings[AState] );
    if PostEvent >= 0 then
      begin
        DebugLn('Post Event:  ' + fEvents.Strings[PostEvent] );
        Event( fEvents.Strings[PostEvent] );
      end;
  end;
end;

function TMealyFiniteStateMachine.CurrentStateIs(aState: String): Boolean;
begin
  Result := UpperCase( aState ) = CurrentState;
end;

function TMealyFiniteStateMachine.ParseTransition(const theTrans: String;
  out Trans : TTrans ) : Boolean;
var
  I, P : Integer;
  S : String;
begin
  S := UpperCase( Trim( theTrans ));
  // Get rid of any comments
  I := Pos('#',S);
  if I > 0 then
    S := Copy(S,1,I-1);
  if Empty( S ) then
    begin
      Result := False;
      Exit;
    end;
  S := Trim(S);
  I := 0;
  Trans[3] := '';  // Allowed to leave fourth entry blank.
  while not Empty( S ) do
    begin
      if I > 3 then
        raise TFSMError.Create('More than 4 entries in Transition' );
      P := Pos(' ',S);
      if P > 0 then
        Trans[I] := Trim(Copy(S,1,P-1))
      else
        Trans[I] := Trim( S );
      Inc(I);
      if P > 0 then
        S := Trim( Copy( S,P,255 ) )
      else
        S := '';
    end;
  if I < 3 then
    raise TFSMError.Create('Less than 3 entries in Transition');
  Result := True;
end;

procedure TMealyFiniteStateMachine.BuildFSM;
var
  I : Integer;
  Trans : TTrans;
  StateIdx : Integer;
  EventIdx : Integer;
  NewStateIdx : Integer;
  PostEventIdx : Integer;
  //NStates : Integer;
  //NEvents : Integer;
  NMap    : Integer;
begin
  // Get all of the states and events, automatically sorted by TStringList
  fStates.Clear;
  fEvents.Clear;
  for I := 0 to pred( fTransitions.Count ) do
    if ParseTransition( fTransitions.Strings[I], Trans ) then
      begin
        fStates.Add( Trans[0] );
        fStates.Add( Trans[2] );
        fEvents.Add( Trans[1] );
        if not Empty( Trans[3] ) then
          fEvents.Add( Trans[3] );
      end;
  // Build the state[event] => newstate map;
  NStates := fStates.Count;
  NEvents := fEvents.Count;
  NMap    := NStates*NEvents;
  //writeln('Building ',fStates.Count,' x ',fEvents.Count,' map');
  SetLength( vTMap, NMap );
  SetLength( vpMap, NMap );
  for I := 0 to pred( NMap ) do
    begin
      vTMap[I] := -1;  // Not all transitions are specified.
      vPMap[I] := -1;
    end;
  for I := 0 to pred( fTransitions.Count ) do
    if ParseTransition( fTransitions.Strings[I], Trans ) then
      begin
        StateIdx := fStates.IndexOf( Trans[0] );
        EventIdx := fEvents.IndexOf( Trans[1] );
        NewStateIdx := fStates.IndexOf( Trans[2] );
        vTMap[ StateIdx * NEvents + EventIdx ] := NewStateIdx;
        if not Empty( Trans[3] ) then
          begin
            PostEventIdx := fEvents.IndexOf( Trans[3] );
            vPMap[ StateIdx * NEvents + EventIdx ] := PostEventIdx;
          end;
      end;
end;

procedure TMealyFiniteStateMachine.Event(theEvent: String);
var
  EventIdx : Integer;
  NewState : Integer;
  LogCurState,
  LogEvent,
  LogNewState : String;
  //procedure DoEnterState( AState, APostEvent : Integer );
  //var
  //  PostEvent : Integer;
  //begin
  //  if Assigned( @fOnEnterState ) then
  //  begin
  //    PostEvent := vPMap[ vCurrentState * NEvents + APostEvent ];
  //    vCurrentState := AState;
  //    fOnEnterState( Self, fStates.Strings[AState] );
  //    if PostEvent >= 0 then
  //      begin
  //        Event( fEvents.Strings[PostEvent] );
  //      end;
  //  end;
  //end;

begin
  if vCurrentState < 0 then
    begin
      if Empty( fState0 ) then
        raise TFSMError.Create('State0 is EMPTY');
      vCurrentState := fStates.IndexOf( fState0 );
      if vCurrentState < 0 then
        raise TFSMError('Unknown State0:  [' + fState0 + ']' );
      if fEnterState0 then
        DoEnterState( vCurrentState, -1 );  // NO Post Event from State0
    end;
  DebugLn('Event:  ' + theEvent );
  theEvent := UpperCase( theEvent );
  EventIdx := fEvents.IndexOf( theEvent );
  if EventIdx < 0 then
    raise TFSMError.Create('Unknown Event: ' + theEvent + ' from state ' + fStates.Strings[vCurrentState] );
  NewState := vTMap[ vCurrentState * NEvents + EventIdx ];
  LogCurState := CurrentState;
  LogEvent    := theEvent;
  if NewState >= 0 then
    begin
      LogNewState := fStates.Strings[NewState];
      if Assigned( fLog ) then
        fLog.Lines.Add(LogCurState + ' : ' + LogEvent + ' -> ' + LogNewState );
      DoEnterState( NewState, EventIdx );
    end
  else
    begin
      if Assigned( fLog ) then
        fLog.Lines.Add(LogCurState + ' : ' + LogEvent + ' -> <None>' );
      if not fIgnoreInvalid then
        raise TFSMError.Create('Invalid Transition from ' + CurrentState + ' by ' + theEvent);
    end;
end;

procedure TMealyFiniteStateMachine.ForceState(theState: String);
var
  S : String;
  I : Integer;
begin
  S := UpperCase(theState);
  I := fStates.IndexOf( S );
  if I < 0 then
    raise TFSMError.Create('Invalid ForceState ' + theState );
  vCurrentState := I;
end;

procedure TMealyFiniteStateMachine.Loaded;
begin
  inherited Loaded;
  BuildFSM;
  IsLoaded := True;
end;

procedure TMealyFiniteStateMachine.LogCurrentState;
begin
  if Assigned(fLog) then
    fLog.Lines.Add('Current State:  ' + CurrentState );
end;

procedure TMealyFiniteStateMachine.SetTransitions(AValue: TStringList);
begin
  //writeln('start SetTRansitions');
  if AValue <> nil then
    begin
      fTransitions.Assign(AValue);
    end
  else
    begin
      if Assigned( fTransitions ) then
        fTransitions.Free;
      fTransitions := TStringList.Create;
    end;

  BuildFSM;

  //writeln('end SetTRansitions');

end;

function TMealyFiniteStateMachine.GetCurrentState: String;
begin
  if vCurrentState >= 0 then
    Result := fStates.Strings[vCurrentState]
  else
    Result := '';
end;

procedure TMealyFiniteStateMachine.SetState0(AValue: String);
begin
  AValue := UpperCase( AValue );
  if fState0=AValue then Exit;
  fState0:=AValue;
  if not IsLoaded then exit;
  if fStates.IndexOf( fState0 ) < 0 then
    raise TFSMError.Create('Unknown State0:  [' + fState0 + ']' );
end;

end.
