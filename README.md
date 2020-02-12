# coreutils

Unix core utilities implemented in Haskell.

Goals are:
* idiomatic implementations
* fast, lazy IO
* feature parity with GNU utils
* fully tested
* platform independent

| Utility | Idiomatic | Complete | Fast | Lazy | Tests |
|---------|-----------|----------|------|------|-------|
| cat     | ✓         | ✓        | ✓    | ✓    |       |
| cut     | ✓         | 90       | 75   | ✓    | 90    |
| echo    | ✓         | ✓        | -    | -    | -     |
| head    |           | 75       | 50   | 50   | ✓     |
| nologin | ✓         | ✓        | -    | -    | -     |
| pwd     | ✓         | ✓        | -    | -    | -     |
| rev     | ✓         | ✓        | ✓    | ✓    | ✓     |
| seq     | ✓         | ✓        | ✓    | ✓    | ✓     |
| sleep   | ✓         | ✓        | -    | -    | ✓     |
| tr      |           | 70       | 40   | 40   | 30    |
| uniq    |           | 80       | 90   | ✓    |       |
| wc      | ✓         | ✓        | ✓    | ✓    |       |
| which   |           | ✓        | -    | -    |       |
| yes     | ✓         | ✓        | ✓    | ✓    | -     |

| Symbol | Meaning            |
|--------|--------------------|
| ✓      | Fully complete     |
| -      | Not applicable     |
| n%     | % feature complete |
