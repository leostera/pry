# Pry [![Travis-CI](https://api.travis-ci.org/ostera/pry.svg)](https://travis-ci.org/ostera/pry)
> Watch your application spawn an unfold right in front of you 🔭

As of right now, `pry` is just a very simple application that will trace and store all of
`erlang:spawn` and `erlang:spawn_link` calls being made until it's stopped.

It lets you dump the data to disk for later analysis.

## Tutorial

```erlang
Eshell V7.3  (abort with ^G)
1> application:start(pry).
ok
2> pry:test(2).
ok
3> pry:dump().
%% ...
[{{1465,43729,784538},
  #{info => [{current_function,{pry,dummy,1}},
     {initial_call,{pry,dummy,1}},
     {status,runnable},
     {message_queue_len,0},
     {messages,[]},
     {links,[]},
     {dictionary,[]},
     {trap_exit,false},
     {error_handler,error_handler},
     {priority,normal},
     {group_leader,<0.63.0>},
     {total_heap_size,233},
     {heap_size,233},
     {stack_size,0},
     {reductions,0},
     {garbage_collection,[{min_bin_vheap_size,46422},
                          {min_heap_size,233},
                          {fullsweep_after,65535},
                          {minor_gcs,0}]},
     {suspending,[]}],
    mfa => {pry,dummy,1},
    parent => <0.76.0>,
    self => <0.78.0>,
    timestamp => {1465,43729,784538}}},
 {{1465,43729,784546},
  #{info => [{current_function,{pry,dummy,1}},
     {initial_call,{pry,dummy,1}},
     {status,runnable},
     {message_queue_len,0},
     {messages,[]},
     {links,[]},
     {dictionary,[]},
     {trap_exit,false},
     {error_handler,error_handler},
     {priority,normal},
     {group_leader,<0.63.0>},
     {total_heap_size,233},
     {heap_size,233},
     {stack_size,0},
     {reductions,0},
     {garbage_collection,[{min_bin_vheap_size,46422},
                          {min_heap_size,233},
                          {fullsweep_after,65535},
                          {minor_gcs,0}]},
     {suspending,[]}],
    mfa => {pry,dummy,1},
    parent => <0.77.0>,
    self => <0.79.0>,
    timestamp => {1465,43729,784546}}}]
%% ...
4> pry:dump_to_file("./pry.1").
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
