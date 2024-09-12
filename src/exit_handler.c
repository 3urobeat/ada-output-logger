/*
 * File: exit_handler.c
 * Project: ada-output-logger
 * Created Date: 2024-09-12 18:40:24
 * Author: 3urobeat
 *
 * Last Modified: 2024-09-12 18:42:35
 * Modified By: 3urobeat
 *
 * Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
 * You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.
 */


#include "exit_handler.h"

extern void ada_log_exit();


// Calls the actual exit interceptor function and reraises the signal using the default handler
void handle_signal(int sig)
{
    ada_log_exit(); // Let Ada handle the printing
    signal(sig, SIG_DFL);
    raise(sig);
}

// Called from Ada to setup signal handlers
void c_exit_handler()
{
    // Create a new sigaction struct and set our signal handler
    struct sigaction act;
    act.sa_handler = handle_signal;

    // Catch all relevant signals, including a normal exit using atexit
    atexit(ada_log_exit);
    sigaction(SIGINT, &act, NULL);
    sigaction(SIGTERM, &act, NULL);
    sigaction(SIGQUIT, &act, NULL);
    /*signal(SIGBREAK, logExit);*/
}
