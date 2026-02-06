<a id="readme-top"></a>

<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
<h3 align="center">Class-Dictionary Specialization of Rank-2 Polymorphic Functions</h3>

  <p align="center">
    <a href="https://github.com/plilab/class-spec-rank2/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    ·
    <a href="https://github.com/plilab/class-spec-rank2/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about">About</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installing-and-building">Installing and Building</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a>
      <ul>
        <li><a href="#basic-usage">Basic Usage</a></li>
        <li><a href="#exposing-unfoldings">Exposing Unfoldings</a></li>
        <li><a href="#plugin-options">Plugin Options</a></li>
      </ul>
    </li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About

This is a GHC plugin for optimizing Haskell programs that use rank-2 polymorphic functions with (ad-hoc) polymorphic arguments. A typical example is the [Scrap Your Boilerplate (SYB)](https://wiki.haskell.org/Scrap_your_boilerplate) library.

This plugin has been tested on GHC version 9.8.4. Support for more GHC versions is in progress.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites
- [GHC](https://www.haskell.org/ghc/) v9.8.4 
- [Cabal](https://www.haskell.org/cabal/) v3.12
- git

### Installing and Building
* Clone this repository
  ```sh
  git clone https://github.com/plilab/class-spec-rank2.git
  cd class-spec-rank2/
  ```
* Building
  ```sh
  cabal build
  ```
You're all set!

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage
### Basic Usage
This plugin can be used when compiling any source file. For example:
```haskell
import Data.Company -- A 'Company' datatype that derives Data
import Data.Generics ( everywhere, mkT ) -- SYB functions

incS :: Float -> Salary -> Salary
incS k (S s) = S $ s * (1 + k)

increase :: Data a => Float -> a -> a
increase k = everywhere $ mkT (incS k)
```
To compile with this plugin, specify the `-O2` optimization and the `ClassSpecRank2` plugin, like so:
```haskell
{-# OPTIONS_GHC -O2 -fplugin ClassSpecRank2 #-}
import Data.Company -- A 'Company' datatype that derives Data
import Data.Generics ( everywhere, mkT ) -- SYB functions

incS :: Float -> Salary -> Salary
incS k (S s) = S $ s * (1 + k)

increase :: Data a => Float -> a -> a
increase k = everywhere $ mkT (incS k)
```
However, this plugin will not do anything to this program because no information on the type to specialize `increase` over is provided. Therefore, either specify the exact type that `increase` operates over, such as
```haskell
-- ...
increase :: Float -> Company -> Company -- Specific type for increase
increase k = everywhere $ mkT (incS k)
```
Or write a `SPECIALIZE` pragma:
```haskell
-- ...
increase :: Data a => Float -> a -> a
increase k = everywhere $ mkT (incS k)
{-# SPECIALIZE increase :: Float -> Company -> Company #-}
```

Include the plugin project directory in your `cabal.project` file:
```cabal
packages: .
          -- ...
          /path/to/class-spec-rank2
```
Include `class-spec-rank2` in your build dependencies in your `my-project.cabal` file:
```cabal
-- ...
executable MyProject
  -- ...
  build-depends:   -- ...
                 , class-spec-rank2
                   -- ... 
```
Now you can build your project with `cabal build` and this plugin will optimize your traversals!

### Exposing Unfoldings
An important caveat when using this plugin is that all unfoldings of any used `Data` instances must be included. That is, in the file containing the instance declarations, provide an `-fexpose-all-unfoldings` GHC option:
```haskell
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

import Data.Generics

data Company = C [Dept] deriving (Show, Data)
data Dept = D Name Manager [SubUnit] deriving (Show, Data)
-- ...
```
Otherwise, the plugin may not be able to inline occurrences of recursive methods/dictionaries,k like `gmapT`, `gmapQ` etc. from `Data`, especially when your datatypes are recursive (GHC does not expose these unfoldings when that is the case):
```haskell
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Data) -- unfoldings for Data (Tree a) are not exposed without
                                                                     -- -fexpose-all-unfoldings option
```
_Note: this requirement affects any data type where `[]` occurs, because GHC does not expose the unfoldings for `Data [a]`. In our testing, re-compiling GHC with the `-fexpose-all-unfoldings` option included in the `Data.Data` source file removes this issue._

### Plugin Options
1. Debug (`--debug`): show some information
2. Iteration limit (`--iter:100`): Set the iteration limit for the partial evaluator to be 100
3. No type constant folding (`--no-type-fold`): Disables type constant folding, only performing partial evaluation.
4. Running the pipeline once (`--pipe-once`): Only runs the optimization pipeline once. This is usually the default, but we added this option for benchmarking in our paper.

```haskell
{-# OPTIONS_GHC -O2 -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

import Data.Company -- A 'Company' datatype that derives Data
import Data.Generics ( everywhere, mkT ) -- SYB functions

incS :: Float -> Salary -> Salary
incS k (S s) = S $ s * (1 + k)

increase :: Float -> Company -> Company
increase k = everywhere $ mkT (incS k)
```

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/yonggqiii/optimizing-syb/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Top contributors:

<a href="https://github.com/plilab/class-spec-rank2/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=plilab/class-spec-rank2" alt="contrib.rocks image" />
</a>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Foo Yong Qi - yongqi@nus.edu.sg

Project Link: [https://github.com/plilab/class-spec-rank2](https://github.com/plilab/class-spec-rank2)

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/plilab/class-spec-rank2.svg?style=for-the-badge
[contributors-url]: https://github.com/plilab/class-spec-rank2/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/plilab/class-spec-rank2.svg?style=for-the-badge
[forks-url]: https://github.com/plilab/class-spec-rank2/network/members
[stars-shield]: https://img.shields.io/github/stars/plilab/class-spec-rank2.svg?style=for-the-badge
[stars-url]: https://github.com/plilab/class-spec-rank2/stargazers
[issues-shield]: https://img.shields.io/github/issues/plilab/class-spec-rank2.svg?style=for-the-badge
[issues-url]: https://github.com/plilab/class-spec-rank2/issues
[license-shield]: https://img.shields.io/github/license/plilab/class-spec-rank2.svg?style=for-the-badge
[license-url]: https://github.com/plilab/class-spec-rank2/blob/master/LICENSE.txt
