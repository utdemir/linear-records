{ compiler ? "ghcHEAD" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import ../../nixos/nixpkgs { config.allowBroken = true; };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "linear-records" =
        let orig = hself.callCabal2nix
              "linear-records"
              (gitignore ./.) {};
         in pkgs.haskell.lib.overrideCabal orig (_: {
           doBenchmark = true;
         });

      "linear-base" =
        hself.callCabal2nix
          "linear-base"
          ../../tweag/linear-base2
          {};

      # Overrides for GHC 9
      exceptions = null;

      primitive = pkgs.haskell.lib.doJailbreak hsuper.primitive;
      tar = pkgs.haskell.lib.doJailbreak hsuper.tar;
      dec = pkgs.haskell.lib.doJailbreak hsuper.dec;
      type-equality = pkgs.haskell.lib.doJailbreak hsuper.type-equality;
      regex-posix = pkgs.haskell.lib.doJailbreak hsuper.regex-posix;
      parallel = pkgs.haskell.lib.doJailbreak hsuper.parallel;
      test-framework = pkgs.haskell.lib.doJailbreak hsuper.test-framework;
      test-framework-quickcheck2 = pkgs.haskell.lib.doJailbreak hsuper.test-framework-quickcheck2;
      async = pkgs.haskell.lib.doJailbreak hsuper.async;
      unliftio-core = pkgs.haskell.lib.doJailbreak hsuper.unliftio-core;
      lifted-async = pkgs.haskell.lib.doJailbreak hsuper.lifted-async;
      ed25519 = pkgs.haskell.lib.doJailbreak hsuper.ed25519;
      hashable-time = pkgs.haskell.lib.doJailbreak hsuper.hashable-time;
      vector = pkgs.haskell.lib.doJailbreak hsuper.vector;
      time-compat = pkgs.haskell.lib.doJailbreak hsuper.time-compat;
      split = pkgs.haskell.lib.doJailbreak hsuper.split;
      vector-th-unbox = pkgs.haskell.lib.doJailbreak hsuper.vector-th-unbox;
      vector-binary-instances = pkgs.haskell.lib.doJailbreak hsuper.vector-binary-instances;
      generic-deriving = pkgs.haskell.lib.doJailbreak hsuper.generic-deriving;
      binary-orphans = pkgs.haskell.lib.doJailbreak hsuper.binary-orphans;
      zlib = pkgs.haskell.lib.doJailbreak hsuper.zlib;
      resolv = pkgs.haskell.lib.doJailbreak hsuper.resolv;
      tasty-hedgehog = pkgs.haskell.lib.doJailbreak hsuper.tasty-hedgehog;

      # hspec-core tests have hardcoded strings for QuickCheck failures,
      # which are not stable across QuickCheck updates.
      hspec-core = pkgs.haskell.lib.dontCheck hsuper.hspec-core;

      # These packages refer to each other in tests in weird ways after
      # recent changes to 'random', so we just disable tests for now.
      mwc-random = hself.mwc-random_0_15_0_1;
      splitmix = pkgs.haskell.lib.dontCheck hself.splitmix_0_1_0_1;
      random = pkgs.haskell.lib.dontCheck hself.random_1_2_0;

      # PR's for GHC 9

      logict = pkgs.haskell.lib.appendPatches hsuper.logict [
        ( pkgs.fetchpatch {
          url = "https://github.com/Bodigrim/logict/commit/"
            + "bf7f2153dac824219640e640e29b2e40392fee0f.patch";
          sha256 = "08a26dvyvqspbwfmqgzxmrfammsg6sbd9p4sq3jlnqf617pjwp81";
        })];
      hedgehog = pkgs.haskell.lib.overrideCabal hsuper.hedgehog (_: {
        jailbreak = true;
        # https://github.com/hedgehogqa/haskell-hedgehog/pull/392
        src =
          let src = pkgs.fetchFromGitHub {
            owner = "utdemir"; repo = "haskell-hedgehog";
            rev = "c98aa9e33bf6871098d6f4ac94eeaac10383d696";
            sha256 = "1018zldx562g5wskzaiq7ifaqnnjxa3dm2vy13j0k5zjvaj966aj";
          }; in "${src}/hedgehog";
      });
      hashable = pkgs.haskell.lib.overrideCabal hsuper.hashable (_: {
        revision = null;
        editedCabalFile = null;
        # https://github.com/tibbe/hashable/pull/189
        src = pkgs.fetchFromGitHub {
          owner = "utdemir"; repo = "hashable";
          rev = "d97d595fff2108da89f5d9f2d46ca1b2f504e888";
          sha256 = "1z27my11nbkzcfn18wrvp5gp5461c4rw4d2h4bgkbjlw8kydazmy";

        };
      });
      doctest = pkgs.haskell.lib.overrideCabal hsuper.doctest (_: {
        revision = null;
        editedCabalFile = null;
        doCheck = false;
        # https://github.com/sol/doctest/pull/272
        src = pkgs.fetchFromGitHub {
          owner = "utdemir"; repo = "doctest";
          rev = "604ff035c9f8ccbe9ccd2f7316797c763bfb2db3";
          sha256 = "1ysc2gsj9pxb7lcpap9mz7j0nvhgz88cnx1mrngdvfs01gccrza2";
        };
      });
      syb = pkgs.haskell.lib.appendPatches hsuper.syb [
        # https://github.com/dreixel/syb/pull/25
        (pkgs.fetchpatch {
          url = "https://github.com/dreixel/syb/commit/"
            + "a072e20d7a5806779486646418fa20c5ffd99a4f.patch";
          sha256 = "08xr33990gx7ic867d0wyjx6k6f4raxyimvarcfk927l4krfp1vj";
        })
      ];
      th-abstraction = pkgs.haskell.lib.appendPatches hsuper.th-abstraction [
        # https://github.com/glguy/th-abstraction/pull/83
        (pkgs.fetchpatch {
          url = "https://github.com/glguy/th-abstraction/commit/"
            + "82ffd01498c5b43478d2607911ed308686ace218.patch";
          sha256 = "1i2fsxskmqn345zfvgp158dkkx8gy6c3n9xdxyqkqa7rx6l75pxa";
        })
      ];
      memory = pkgs.haskell.lib.appendPatches hsuper.memory [
        # https://github.com/vincenthz/hs-memory/pull/81
        (pkgs.fetchpatch {
          url = "https://github.com/vincenthz/hs-memory/commit/"
            + "683b402965e68800337af8f9b237d28be273c0d8.patch";
          sha256 = "1jqqbby9rwkgfs65i4fkxzh93ck2sp0w16nqvnz3yc3gx08hb9ll";
        })
      ];
      tagged = pkgs.haskell.lib.overrideCabal hsuper.tagged ({ patchPhase ? "", ... }: {
        # https://github.com/ekmett/tagged/pull/50
        patchPhase = ''
          ${patchPhase}
          sed -i -E 's/(template-haskell.*)&& < 2.15/\1/g' tagged.cabal
        '';
      });
      integer-logarithms =
        pkgs.haskell.lib.overrideCabal hsuper.integer-logarithms (su: {
          jailbreak = true;
          configureFlags = (su.configureFlags or [ ]) ++ [ "-fghc-bignum" ];
          editedCabalFile = null;
          revision = null;
          src = pkgs.fetchFromGitHub {
            owner = "Bodigrim"; repo = "integer-logarithms";
            rev = "f5813d8cd0b6fd871b3c76027802af2a46915cd4";
            sha256 = "1xa52394l4myfavz68pwk9gq4gmvjfvf15hcksvapc777jlx9qys";
          };
        });
      # https://github.com/haskell-hvr/regex-base/pull/3
      regex-base = pkgs.haskell.lib.overrideCabal hsuper.regex-base (_: {
        jailbreak = true;
        src = pkgs.fetchFromGitHub {
          owner = "bgamari"; repo = "regex-base";
          rev = "67c427f4907a544759b9fbe8fb4a28cb65a73e34";
          sha256 = "0zc4819a896fvy7nsddh738irqxgh2x1ahqgvidmsj6qp536vg88";
        };
      });
      # https://github.com/sjoerdvisscher/fmlist/pull/6
      fmlist = pkgs.haskell.lib.overrideCabal hsuper.fmlist (_: {
        src = pkgs.fetchFromGitHub {
          owner = "utdemir"; repo = "fmlist";
          rev = "a1b72120fd6e0b421e3eed32eb398cf1133e1440";
          sha256 = "0q1hx9pb62r2hpdqszri4qkkgbgba7ld0c4xxirn7q1lawnwhk04";
        };
      });
      # https://github.com/nick8325/quickcheck/pull/311
      QuickCheck = pkgs.haskell.lib.overrideCabal hsuper.QuickCheck_2_14_1 (_: {
        src = pkgs.fetchFromGitHub {
          owner = "phadej"; repo = "quickcheck";
          rev = "d659a5e44b3304e7599321d506bf48d2e88389d0";
          sha256 = "0ifp5wj2dsj3r2syb7z2486c88w1rl08lxsm5m8k5yn57vn96ykb";};});
    } //
    (
      let foundationSrc = pkgs.applyPatches {
        src = pkgs.fetchFromGitHub {
          owner = "haskell-foundation";
          repo = "foundation";
          rev = "a4f20de16eaee74ae4fbe279a27929ee27c86f9e";
          sha256 = "0a0fi8n4wi3a46yl8ph0rzczvplhdqjcffqx7msardbkdvks6x7x";
        };
        patches = [
          # https://github.com/haskell-foundation/foundation/pull/538
          (pkgs.fetchpatch {
            url = "https://github.com/haskell-foundation/foundation/commit/"
              + "1a39cd21c00642f8a52555713c0f5bed2d687e72.patch";
            sha256 = "09104agjqysjv0qcph6r53c5waa17g00c5glqhc153xmf7s9rkzb";
          })

          # https://github.com/haskell-foundation/foundation/pull/537
          (pkgs.fetchpatch {
            url = "https://github.com/haskell-foundation/foundation/commit/"
              + "790884c9bede38460ba37c8e8e599416cb040a31.patch";
            sha256 = "0hp72fahc03c0lwm9jkv8hp5jxrlkimb8nnsf0c7mnaqg4kzgcgi";
          })

          # https://github.com/haskell-foundation/foundation/pull/540
          (pkgs.fetchpatch {
            url = "https://github.com/haskell-foundation/foundation/commit/"
              + "70af0854f591961edd40cb244e405fc3bc75d395.patch";
            sha256 = "1zykabmw0sw7qpmjmdkla4fjrjxyrjxhx036ff9mzm0mpg70ik7g";
          })
        ];
      };
      in
      {
        basement =
          pkgs.haskell.lib.overrideCabal hsuper.basement (_: {
            src = "${foundationSrc}/basement";
          });
        foundation =
          pkgs.haskell.lib.overrideCabal hsuper.foundation (_: {
            src = "${foundationSrc}/foundation";
          });
      });
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."linear-records"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };
in
{
  inherit shell;
  haskellPackages = myHaskellPackages;
  "linear-records" = myHaskellPackages."linear-records";
}
