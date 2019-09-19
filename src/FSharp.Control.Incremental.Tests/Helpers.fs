namespace FsCheck

[<AutoOpen>]
module Helpers =

    let refequal (expected : 'a) =
        { new NHamcrest.Core.IsEqualMatcher<obj>(expected) with
            override x.Matches o =
                System.Object.ReferenceEquals(o, expected)
        }
