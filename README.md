# ada-output-logger
An Ada logging library offspring of my [JS output-logger library](https://github.com/3urobeat/output-logger).  

It allows you to painlessly log values of different data types with different log levels to stdout without having to join strings together manually.  
It simultaneously provides color-highlighting based on log level, attaches a timestamp to your message, logs to an output file and much more.

&nbsp;

## Features
- Colored log levels: Info, Warn, Error & Debug
- Keep track of every log message with a output file
- Display file name to keep track of log origins even in large projects
- Overwrite a log message with the next one by marking it as *remove*
- Animations: 6 are shipped by default

</br>

The fundamental idea of the library is to easily construct log messages by chaining calls to public functions of `Logger_Type`.

It facilitates this concept by creating an instance `Logger` of the *tagged record* `Logger_Dummy` which every function takes as the first parameter, making them accessible through `Logger` by using the *dot notation*.  
Every function then returns the `Logger` instance again so that you can easily chain another function call.  

This creates only one minor problem because Ada *forces* you to handle function return values - you have to end your chain with a call to the procedure `EoL` (End Of Line).  
This procedure only serves to consume the `Logger` parameter, does nothing and returns nothing.

For examples, see the [logger_test.adb](./logger_test.adb) file.

<br>

> [!IMPORTANT]
> Make sure to \*only\* use this library for logging while it is active.  
> Printing yourself while the logger is active will lead to weird behavior like messages clipping into another,  
> as the library does clever cursor management which `Ada.Text_IO` does not do.

&nbsp;

## Include
Create a `lib/` directory in your project and clone this repository into it.  
Add `-I../lib/ada-output-logger/src` to your `gnatmake` command, assuming your run the command from a build folder inside your project root.

You can then include the logger in your project using `with Logger_Type;` and `use Logger_Type;`.

&nbsp;

## Functions
For examples, see the [logger_test.adb](./logger_test.adb) file.
