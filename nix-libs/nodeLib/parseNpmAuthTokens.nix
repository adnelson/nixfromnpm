# Parses the `NPM_AUTH_TOKENS` environment variable to discover
# namespace-token associations and turn them into an attribute set
# which we can use as an input to the fetchPrivateNpm function.
{pkgs, joinSets}:

let
  inherit (pkgs.lib) flip length elemAt;
  npmAuthTokens = builtins.getEnv "NPM_AUTH_TOKENS";
in

# Split the variable on ':', then turn each k=v element in
# the list into an attribute set and join all of those sets.
joinSets (
  flip map (split ":" npmAuthTokens) (kvPair:
    if length (split "=" kvPair) != 2 then {}
    else {"${elemAt kvPair 0}" = elemAt kvPair 1;}))
