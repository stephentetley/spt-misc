% sensors.pl

:- module(classification,
            [ has_negative_setpoints/2
            , has_positive_setpoints/2

            , is_pump_control_relay/1
            , is_positive_alarm_relay/1
            , is_negative_alarm_relay/1
            , is_bounds_alarm_relay/1
            , is_alarm_relay/1
            , is_active_relay/1
            , is_fixed_relay/1
            , is_relay_not_in_use/1
            , is_time_of_day_relay/1
            , is_undefined_relay/1
            ]).

:- use_module(facts/fact_db).


% TODO should this take a relay struct, e.g.:
%   active_relay('UNDESIGNATED LEVEL ALARM',  0.66, 0.6).
%
has_negative_setpoints(OnSetPoint, OffSetPoint) :- 
    OffSetPoint > OnSetPoint.

has_positive_setpoints(OnSetPoint, OffSetPoint) :- 
    OnSetPoint > OffSetPoint.


% is_pump_control_relay
is_pump_control_relay('ALTERNATE DUTY ASSIST').
is_pump_control_relay('ALTERNATE DUTY BACKUP').
is_pump_control_relay('ALTERNATE DUTY STANDBY').
is_pump_control_relay('FIXED DUTY').
is_pump_control_relay('FIXED DUTY ASSIST').
is_pump_control_relay('FIXED DUTY BACKUP').
is_pump_control_relay('FIXED DUTY STANDBY').
is_pump_control_relay('SERVICE RATIO DUTY ASSIST').
is_pump_control_relay('SERVICE RATIO DUTY BACKUP').

% is_negative_alarm
is_negative_alarm_relay('LOW LEVEL ALARM').
is_negative_alarm_relay('LOW LOW LEVEL ALARM').

% is_positive_alarm_relay
is_positive_alarm_relay('HIGH HIGH LEVEL ALARM').
is_positive_alarm_relay('HIGH LEVEL ALARM').
is_positive_alarm_relay('UNDESIGNATED LEVEL ALARM').

% is_bounds_alarm_relay
is_bounds_alarm_relay('IN BOUNDS ALARM').
is_bounds_alarm_relay('OUT OF BOUNDS ALARM').

% is_alarm_relay
is_alarm_relay(Relay) :- is_negative_alarm_relay(Relay).
is_alarm_relay(Relay) :- is_positive_alarm_relay(Relay).
is_alarm_relay(Relay) :- is_bounds_alarm_relay(Relay).

% is_active_relay
is_active_relay(Relay) :- is_alarm_relay(Relay).
is_active_relay(Relay) :- is_pump_control_relay(Relay).

% is_fixed_relay - fixed relay, no setpoints.
is_fixed_relay('TRANSDUCER CABLE FAULT ALARM').
is_fixed_relay('LOSS OF ECHO').
is_fixed_relay('RELAY NOT IN USE').

% is_relay_not_in_use
is_relay_not_in_use('RELAY NOT IN USE').


% is_time_of_day_relay
% Note - (HyPlus) 
% This should carry a single value (HH:MM) rather than On/Off setpoints 
% the database is not set up to record time values. 
is_time_of_day_relay('TIME OF DAY').

% is_known_relay_
is_known_relay_(Relay) :- is_fixed_relay(Relay).
is_known_relay_(Relay) :- is_active_relay(Relay).
is_known_relay_(Relay) :- is_time_of_day_relay(Relay).

is_undefined_relay(Relay) :- not(is_known_relay_(Relay)).
is_undefined_relay(Relay) :- not(is_relay_not_in_use(Relay)).


