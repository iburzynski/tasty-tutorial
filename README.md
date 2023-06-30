# **Introduction to Haskell Testing with `tasty` 🍽️😋**

This repository contains sample code for exploring the [`tasty`](https://hackage.haskell.org/package/tasty) test framework. We'll build a test suite for a simple IP filtering application and explore two different testing methodologies:
1. Unit testing with [`hspec`](https://hackage.haskell.org/package/hspec)
2. Property-based testing with [`hedgehog`](https://hackage.haskell.org/package/hedgehog)

The sample code is adapted from Chapter 8 of [*Haskell in Depth*](https://www.manning.com/books/haskell-in-depth) by Vitaly Bragilevsky. The original source code can be found [here](https://github.com/bravit/hid-examples). Significant portions of the code have been modified to increase simplicity and readability.

## **Setup**
The application can be built and run using either Nix or Cabal.

### **Nix (recommended)**
If you use Nix, you can build the application from the `flake.nix` file, which will install compatible versions of the Haskell toolchain as well as a preconfigured VS Codium editor you can use to explore the code.

You must have the following `experimental-features` enabled in your Nix configuration (`/etc/nix/nix.conf`):

```
# /etc/nix/nix.conf

experimental-features = flakes nix-command
```

**🚨 IMPORTANT!** You must restart the `nix-daemon` after modifying `nix.conf` to apply the changes

**Linux:**

  ```sh
  sudo systemctl restart nix-daemon
  ```

**MacOS:**

  ```sh
  sudo launchctl stop org.nixos.nix-daemon
  sudo launchctl start org.nixos.nix-daemon
  ```

After cloning the repository, enter the `tasty-tutorial` directory and run `nix develop` to enter the Nix environment.

Once the dependencies finish building, you can run `codium .` to open a preconfigured VS Codium instance with IDE support. Run `cabal build` in the integrated terminal to build the project.

### **Cabal**
Running the application with Cabal only (without Nix) requires a version of GHC `>= 9.2.5` and `< 9.4`, Cabal `>= 3.0`, and a compatible version of Haskell Language Server (HLS).

Use [GHCup](https://www.haskell.org/ghcup/) to install the required tooling and `ghcup tui` to adjust versions as needed.

You can then build the project via `cabal build`.

## **Running the App**
You can try the app using the following command:

```
cabal run tasty-tutorial -- data/ipranges.txt 192.168.1.3
```

## **Testing the App**
You can run the test suite with the following command:

```
cabal test tasty-tutorial.cabal
```

## **Application Overview**
The application is a command-line utility that checks whether a specified IP address is contained within any ranges present in a given text file.

The `Main` module in `app/Main.hs` makes use of the [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) library to provide a CLI and associated help text, accepting a filepath and IP address string as options. The code in this module may be difficult to understand without prior familiarity with `optparse-applicative`. It isn't critical to understand this code in detail: the important piece is the `run` function, which parses the IP ranges in the file and the provided IP address, then either prints the result or throws exceptions if the data is invalid or malformed. 

Our test suite will focus on testing the business logic of the application, which consists of:
  1. parsing the IP ranges in the input file and the provided IP address
  2. checking if the address is contained in any of the IP ranges

This functionality is contained in `src/ParseIP.hs` and `src/LookupIP.hs`, respectively. The custom types used in the application are contained in `src/IPTypes.hs`.

The **`ParseIP`** module contains the following functions:
* **`parseIP`**: parses an IP address string (i.e. `"192.168.3.15"`) to a `Maybe IP` value (where `IP` is a `newtype` wrapping a `Word32` value)
* **`parseIPRange`**: parses a string expressing an IP range (i.e. `"192.168.0.1,192.168.3.100"`) into a `Maybe IPRange` value (where `IPRange` is a product type consisting of two `IP` values).
* **`parseIPRanges`**: parses a string containing multiple IP range strings (separated by newlines), returning an `Either ParseError IPRangeDB` value. 
  * An `IPRangeDB` value is a `newtype` wrapping list of `IPRange` values. 
  * A `ParseError` is a custom `Exception` type containing an `Int` value representing the line number where the error occurred.

The **`LookupIP`** module contains the following functions:
* **`lookupIP`**: a predicate function that takes an `IPRangeDB` value and an `IP` value and indicates whether the `IP` is covered by one of the `IPRange` values in the `IPRangeDB`.
* **`reportIP`**: performs `lookupIP` and outputs a formatted string containing the IP address and a YES/NO result indicating whether it satisfies one of the ranges in the `IPRangeDB`.