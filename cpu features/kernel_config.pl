% Complete the initialization for a desktop system
init_desktop_example :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    assert(fact(cpu_cores(4))),
    assert(fact(total_ram_mb(8192))),
    assert(fact(disk_type(ssd))),
    assert(fact(cpu_feature(aes))),
    assert(fact(ac_powered)),
    apply_profile(desktop),
    print_configuration.

% Example initialization for a server system
init_server_example :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    assert(fact(cpu_cores(16))),
    assert(fact(total_ram_mb(32768))),
    assert(fact(disk_type(ssd))),
    assert(fact(cpu_feature(tsc))),
    assert(fact(ac_powered)),
    apply_profile(server),
    print_configuration.

% Example initialization for a laptop system
init_laptop_example :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    assert(fact(cpu_cores(2))),
    assert(fact(total_ram_mb(4096))),
    assert(fact(disk_type(ssd))),
    assert(fact(prefer_power_saving)),
    apply_profile(laptop),
    print_configuration.

% Example initialization for an embedded system
init_embedded_example :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    assert(fact(cpu_cores(1))),
    assert(fact(total_ram_mb(512))),
    assert(fact(disk_type(flash))),
    apply_profile(embedded),
    print_configuration.

% Example initialization for a virtualized system
init_virtualized_example :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    assert(fact(cpu_cores(8))),
    assert(fact(total_ram_mb(16384))),
    assert(fact(disk_type(ssd))),
    assert(fact(is_virtual(true))),
    apply_profile(server),
    print_configuration.

% Main entry point to initialize and demonstrate different system configurations
demo :-
    format('~n=== Desktop System Configuration ===~n'),
    init_desktop_example,
    format('~n=== Server System Configuration ===~n'),
    init_server_example,
    format('~n=== Laptop System Configuration ===~n'),
    init_laptop_example,
    format('~n=== Embedded System Configuration ===~n'),
    init_embedded_example,
    format('~n=== Virtualized System Configuration ===~n'),
    init_virtualized_example.

% Utility to reset the system
reset :-
    retractall(fact(_)),
    retractall(setting(_, _)),
    format('System reset. All facts and settings cleared.~n').

% Example query to check if a specific configuration is applied
is_configured(Setting, Value) :-
    setting(Setting, Value),
    format('Configuration ~w = ~w is applied.~n', [Setting, Value]).

% Example usage instructions
usage :-
    format('~nCoreA OS Kernel Configuration Expert System Usage:~n'),
    format('1. Initialize a system profile:~n'),
    format('   ?- init_desktop_example.~n'),
    format('   ?- init_server_example.~n'),
    format('   ?- init_laptop_example.~n'),
    format('   ?- init_embedded_example.~n'),
    format('   ?- init_virtualized_example.~n'),
    format('2. Run a full demonstration:~n'),
    format('   ?- demo.~n'),
    format('3. Check specific configuration:~n'),
    format('   ?- is_configured(scheduler, cfs).~n'),
    format('4. Reset the system:~n'),
    format('   ?- reset.~n'),
    format('5. Manually add facts and configure:~n'),
    format('   ?- assert(fact(cpu_cores(4))), assert(fact(total_ram_mb(8192))), configure_all, print_configuration.~n').

% Initialize with a welcome message
:- initialization((
    format('Welcome to CoreA OS Kernel Configuration Expert System~n'),
    format('Type "usage." for instructions on how to use the system.~n')
)).
