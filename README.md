# ada-output-logger
An Ada logging library offspring of my [JS output-logger library](https://github.com/3urobeat/output-logger).  

It drastically simplifies logging messages and interacting with stdout/-in while also providing a bunch of eyecandy for a better user experience.  
See the list of features below!

&nbsp;

## Features
- Colored log levels: Info, Warn, Error & Debug
- Keep track of every log message with an output file
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

Add an import at the top of your main body:  
`with Logger_Type; use Logger_Type;`

At the top of your project's gpr file, add a with statement:  
`with "lib/ada-output-logger/ada_output_logger.gpr";`

You can then simply compile your project using gprbuild and the library will be included in the build process.    
> Compiling using gnatmake is currently not supported because C sources are used as well.

&nbsp;

## Functions
Start constructing your message by calling `Logger`.  
You can then chain any other function behind it, following a few rules:
- `Rm`, indicating that this message should be removed by the next one, must always be the **first** function in the chain
- `Animation` must always be the first, or in case of `Rm` existing the second, function in the chain 
- `Nl`, indicating that a newline should be printed after this message, must always be the second to last function in the chain
- Every call chain must end with `EoL;` to consume the return value of the previous function in the chain
- You cannot add `Nl` to a message marked as `Rm` as it would obviously break the removal of it

For basically any possible example, please see the [logger_test.adb](./tests/logger_test.adb) file.
