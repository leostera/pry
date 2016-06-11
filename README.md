# Pry [![Travis-CI](https://api.travis-ci.org/ostera/pry.svg)](https://travis-ci.org/ostera/pry)
> Watch your application spawn an unfold right in front of you 🔭

As of right now, `pry` is just a very simple application that will trace and store all of
`erlang:spawn` and `erlang:spawn_link` calls being made for any non-standard Erlang module.

Have a look at `pry_blacklist` to see exactly what's being ignored.

## Tutorial

```erlang
Eshell V7.3  (abort with ^G)
1> application:start(pry).
ok
2> pry:add_publisher(self()).
ok
3> application:start(schrodinger).
ok
4> flush().
Shell got #{info => [{registered_name,schrodinger_sup},
             {current_function,{gen_server,loop,6}},
             {initial_call,{proc_lib,init_p,5}},
             {status,waiting},
             {message_queue_len,0},
             {messages,[]},
             {links,[<0.86.0>,<0.88.0>]},
             {dictionary,[{'$ancestors',[<0.86.0>]},
                          {'$initial_call',{supervisor,schrodinger_sup,1}}]},
             {trap_exit,true},
             {error_handler,error_handler},
             {priority,normal},
             {group_leader,<0.85.0>},
             {total_heap_size,233},
             {heap_size,233},
             {stack_size,9},
             {reductions,104},
             {garbage_collection,[{min_bin_vheap_size,46422},
                                  {min_heap_size,233},
                                  {fullsweep_after,65535},
                                  {minor_gcs,0}]},
             {suspending,[]}],
            mfa => {schrodinger_sup,start_link,0},
            parent => <0.86.0>,
            self => <0.87.0>,
            timestamp => {1465,643365,584223}}
Shell got #{info => [{registered_name,schrodinger_lab},
             {current_function,{gen_server,loop,6}},
             {initial_call,{proc_lib,init_p,5}},
             {status,waiting},
             {message_queue_len,0},
             {messages,[]},
             {links,[<0.87.0>]},
             {dictionary,[{'$ancestors',[schrodinger_sup,<0.86.0>]},
                          {'$initial_call',{schrodinger_lab,init,1}}]},
             {trap_exit,false},
             {error_handler,error_handler},
             {priority,normal},
             {group_leader,<0.85.0>},
             {total_heap_size,233},
             {heap_size,233},
             {stack_size,9},
             {reductions,20},
             {garbage_collection,[{min_bin_vheap_size,46422},
                                  {min_heap_size,233},
                                  {fullsweep_after,65535},
                                  {minor_gcs,0}]},
             {suspending,[]}],
            mfa => {schrodinger_lab,init,1},
            parent => <0.87.0>,
            self => <0.88.0>,
            timestamp => {1465,643365,584277}}
ok
```

## Event format


```erlang
#{
  % Raw Process information
  info => [{current_function,{pry,dummy,1}}, ..],

  % Quick access to the MFA this process was spawned with
  mfa => {pry,dummy,1},

  % Pid of the parent
  parent => <0.77.0>,

  % Pid of this process
  self => <0.79.0>,

  % Timestamp when the tracer sent this process for tracking
  timestamp => {1465,43729,784546}
}
```

## Next Steps

See the [issues page](https://github.com/ostera/pry/issues?q=is%3Aopen+is%3Aissue+label%3Aenhancement) for a list of planned enhancements and features.
