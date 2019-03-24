/* 
    database.pl
*/

hydros_db([
    hydro('DA045', 4.6, 4.3, 
            [ measure_relay(1, level_alarm, 4.0, 3.8)
            , relay(5, loss_of_echo)
            ]),
    hydro('SZ0205', 2.3, 2.0, 
            [ measure_relay(1, fixed_duty, 1.5, 1.4)
            , measure_relay(2, fixed_duty, 1.7, 1.4)
            , relay(5, loss_of_echo)
            ])
    ]).            

