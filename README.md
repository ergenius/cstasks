# cstasks

Solution for CraftingSoftware

[![Erlang CI](https://github.com/ergenius/cstasks/actions/workflows/erlang.yml/badge.svg)](https://github.com/ergenius/cstasks/actions/workflows/erlang.yml)

## Build

The solution is using rebar3. A Makefile is also provided.
Development took place using OTP 26 release on Ubuntu 22.04.
Unofficially, you may be able to use this project with older Erlang versions. No guarantee included.

Only one command, compile, is required to fetch dependencies and compile (use either make or rebar3):
```
make compile 
rebar3 compile
```

Create a release:
```
make release
rebar3 release
```

A release is already included into the repo into the /release directory.

## Running the solution

Change directory to the /release/cstaks/bin directory and run
```
$ ./cstasks console
```

To see all the vailable comands run ./cstasks script without parameters
```
$ ./cstasks
Usage: cstasks [COMMAND] [ARGS]
Commands:
  foreground              Start release with output to stdout
  remote_console          Connect remote shell to running node
  rpc [Mod [Fun [Args]]]] Run apply(Mod, Fun, Args) on running node
  eval [Exprs]            Run expressions on running node
  stop                    Stop the running node
  restart                 Restart the applications but not the VM
  reboot                  Reboot the entire VM
  pid                     Print the PID of the OS process
  ping                    Print pong if the node is alive
  console                 Start the release with an interactive shell
  console_clean           Start an interactive shell without the release's applications
  console_boot [File]     Start an interactive shell for boot script [File]
  daemon                  Start release in the background with run_erl (named pipes)
  daemon_boot [File]      Start boot script [File] in the background with run_erl (named pipes)
  daemon_attach           Connect to node started as daemon with to_erl (named pipes)
  upgrade [Version]       Upgrade the running release to a new version
  downgrade [Version]     Downgrade the running release to a new version
  install [Version]       Install a release
  uninstall [Version]     Uninstall a release
  unpack [Version]        Unpack a release tarball
  versions                Print versions of the release available
  escript                 Run an escript in the same environment as the release
  status                  Verify node is running and then run status hook scripts
  undefined
```

## Testing the solution using the provided web frontend application

The csapplication application starts a webserver listening to localhost on port 1986 (the year Erlang was released).
Open http://localhost:1986/ in your browser and you will be presented with a frontend application that you can use to test the API. The web application is self explanatory. 

On the left side of the screen there is a textarea where you can edit the API input parameters. You can test the default input parameters mentioned into the problem or you can paste your own JSON. To test the default simply click on the button

### Web frontend test applications screenshots

Frontend application screenshot calling the JSON API. Notice the input body is in the left textarea and the response body from the API is displayed in the right textarea.
![Frontend application screenshot calling the JSON API](https://raw.githubusercontent.com/ergenius/cstasks/refs/heads/main/screenshots/calling-json-api-2024-10-07%2021-43-27.png)

Frontend application screenshot calling the "Bash" API. You will be presented with a save/dowload windows. Notice the input body is in the left textarea and the response body from the API is displayed in the right textarea. 
![Frontend application screenshot calling the Bash API](https://raw.githubusercontent.com/ergenius/cstasks/refs/heads/main/screenshots/calling-the-bash-api-2024-10-07%2021-46-12.png)

## Manually testing the API 

The following API routes are available (it is important to use the proper method POST otherwise you will receive
a 400 Bad Request for the existing API routes).

### Sort JSON

Request:
- URL: http://localhost:1986/api/v1/sort/json
- Method: POST
- No special headers are required
- Request body example (JSON):
```
{
	"tasks": [
		{
			"name": "task-1",
			"command": "touch /tmp/file1"
		},
		{
			"name": "task-2",
			"command": "cat /tmp/file1",
			"requires": [
				"task-3"
			]
		},
		{
			"name": "task-3",
			"command": "echo 'Hello World!' > /tmp/file1",
			"requires": [
				"task-1"
			]
		},
		{
			"name": "task-4",
			"command": "rm /tmp/file1",
			"requires": [
				"task-2",
				"task-3"
			]
		}
	]
}
```

Response example:
```
{
  "tasks" : [
    {
      "name" : "task-1",
      "command" : "touch /tmp/file1"
    },
    {
      "name" : "task-3",
      "command" : "echo 'Hello World!' > /tmp/file1"
    },
    {
      "name" : "task-2",
      "command" : "cat /tmp/file1"
    },
    {
      "name" : "task-4",
      "command" : "rm /tmp/file1"
    }
  ],
  "sort_time_milliseconds" : 416
}
```

### Sort BASH

- URL: http://localhost:1986/api/v1/sort/bash
- Method: POST
- No special headers are required
- Request body example (JSON):
```
{
	"tasks": [
		{
			"name": "task-1",
			"command": "touch /tmp/file1"
		},
		{
			"name": "task-2",
			"command": "cat /tmp/file1",
			"requires": [
				"task-3"
			]
		},
		{
			"name": "task-3",
			"command": "echo 'Hello World!' > /tmp/file1",
			"requires": [
				"task-1"
			]
		},
		{
			"name": "task-4",
			"command": "rm /tmp/file1",
			"requires": [
				"task-2",
				"task-3"
			]
		}
	]
}
```

Response example:
```
{
  "bash" : "#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1\n"
}
```

## Testing with eunit

Various eunit tests are provided in /tests/cstasks_tests.erl

Run
```
make test
```
or
```
./rebar3 eunit
```

## Directory structure

- .github - contains Github workflow
- config - contain system configuration source file: sys.config.src and vm.args.src. Most settings are self explanatory and they should work out of the box without changes. However you may want to change the cowboy http listner port for example.
- doc - contains documentation automatically generated from the source code
- include - contains csstask.hrl - included in all project modules. Nothing fancy there, a record, and a few error log macros.
- screenshots - various screnshots of the frontend web application you can use for testing
- src
- src/cs_http_handler_api.erl - Cowboy sorting API handler
- src/cs_http_handler_ww.erl - A custom Cowboy Static file HTTP handler. This handler serve files inside www directory - the frontend Single Page Application written in JavaScript and used to test our solution API. I know cowboy comes with a ready to use handler for serving static files, but again it was fun creating a static file handler that is able to cache the static content into an ETS, serve any IMT known to man, pardon me apache/nginx (the cs_http_imt was build studing Apache/Nginx IMT config files) and a paranoic validation of the request using recursive functions and binaries. Because I must say it's a hobby of mine NOT to use regex.
- src/cs_http_imt.erl This file contain IMT (Internet Media Type) related functions. It is used for detecting proper IMT from files extensions into the cs_http_handler_www module.
- src/cs_json.erl - This is nothing else but a wrapper around jiffy. I added it to the solution to be able to easily change jiffy options
- src/cs_sort_behaiviour.erl -  I created this module to demonstrate how to create a custom behaiviour. To be sincere I love defining new behaiviours despite I'm not much in love with OOP.
- src/cs_sort_kahn.erl - This is the sorting solution based on a modified Kahn algorithm.
- src/cs_sup.erl - main application supervisor
- src/cs_tasks_job.erl - Contains varios helper functions for dealing with a tasks job record
- src/cs_utils.erl - a few utils functions
- src/cs_uuid.erl - This is an exercise to create a uuid. The uuid is based on: local time in microseconds starting from "CraftingSoftware epoch", some random bytes and a namespace (phash2 of the erlang node atom). When I wanted to insert the tasks job in database I said to myself let's use an uuid instead of that sqlite integer auto incremented primary key and let's have fun and create our own uuid instead of just using a 3rd party uuid erlang library.
- src/cs_validate.erl - Some paranoic validation done on various binaries comming from http requests. "Features": No conversion to lists is done.
- src/cstasks_app.erl
- src/cstasks_app.src 
- test - a few eunit tests you can run with make test
- www - contains the one page web application (html+Javascript+jQuery+Bulma - mobile friendly)

## How did I implemented the sort solution?

- It was obviously from the begining this is a graph sorting problem.
- The input data is be a Directed Acyclic Graph (a graph with no cycles) because otherwise we will not be able to solve it and we will end up with circular dependencies and infinite loops.
- The first algorithm that I could think of was the famous Depth First Search
- Searching more information on this subject I came across Kahn algorithm and I found it more interesting to exercise it.
- When chosing in between DFS & Kahn I also took into consideration both the time & space complexity and they are both the same - O(V+E). So from this point of view I thought both could do the job.
- When studying various Kahn examples something "contrived" struck me to use the word from the coding challenge: many implementations examples are based on data that keep track of the CHILDREN dependencies not the FATHER nodes/vertices (our require). This was for me one of the most tricky aspect when solving this problem.
- After writing the basic algorithm I start thinking to some corner case scenarios and implemented them as options or errors.
- In a normal relation with a client I would ask the client about those corner case scenarios but because I had to finish the test in weekend I decided not to bother Vivien from CS with extra questions and simply implement all those corner case scenarios as API options.

It is also worth mentioning I favored eficiency. Because of this I prefered to mix and implement most validation inside the sort itself in order to avoid extra iterations of the tasks list outside the sort algorithm.

The corner case scenarios implemented as options are:
- should we allow or not more than 1 source vertice?
- should we allow or not disconected vertices?

The error (validation) case scenarios are:
- invalid or malformed tasks with missing or invalid names, commands and requires
- we must detect cycles (tasks that are linked to each other in a loop) and we must obviously consider such an input invalid by returning a specific error

## Dependencies

- [m_unicode](https://github.com/ergenius/m_unicode)
- [cowboy](https://github.com/ninenines/cowboy)
- [rebar3](https://s3.amazonaws.com/rebar3/rebar3)

## Authors

- Madalin Grigore-Enescu (ergenius) - [Github](https://github.com/ergenius) [ergenius.com](<https://ergenius.com>)

## License

cstasks is available under the MIT license (see `LICENSE`).
