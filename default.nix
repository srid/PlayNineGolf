{ system ? builtins.currentSystem }: # TODO: Get rid of this system cruft
with import ./.obelisk/impl { inherit system; };
project ./. ({ ... }: {
  android.applicationId = "ca.srid.playninegolf";
  android.displayName = "Play Nine Golf";
  ios.bundleIdentifier = "ca.srid.playninegolf";
  ios.bundleName = "Play Nine Golf";
})
