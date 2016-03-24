# Weatherballon
Based off the ambiata interview challenge thing at https://github.com/ambiata/interview/blob/master/weather.md

# Capabilities
- Generate valid and invalid data (or a mix!)
- Produces all the statistics (Min T, Max T, Mean T, N obs, total D) although outputs are in SI only
- Outputs a sorted version of the input file in desired units

# Usage
weatherballoon generate number onlyvalid(T/F) -t OUTPUT-FILE

weatherballoon analyse output-dist output-temp mintemp maxtemp meantemp distance nobs
weatherballoon analyse (km|mi|m) (C|f|K) T/F T/F T/F T/F T/F -t TARGET-FILE

Has help options too!

# Tests
stack test
- see tests/WBTests.hs for details

# Known Issues
- The rounding for US converstions is off by up to 1 due to using Ints as storage values. Consiquently the test fails. Given the question specifying Z+ as measurements values and the generally unknown behaviour of the balloon (e.g. direction of rounding) I was uncomfortable inventing accuracy.
