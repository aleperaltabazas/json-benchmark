JSON benchmarks
===

Some dumb JSON serialization/deserialization benchmarks. I came up with this idea based off of Aeson's `FromJSON` typeclass, and thought that maybe we could apply something similar on the JVM, generating code at compile time for static deserializers (I believe Circe does this in Scala, but don't quote me on that).

I will be using [this file](json/test.json) for all tests. It's a compact JSON with 1000 entries of the default object generated by [json-generator](https://www.json-generator.com/).

## Haskell

### Aeson
Using aeson with a derived Generic and default `FromJSON` instance gave the following result, in average:
```
6.220000000000001e-5ms
6.19e-5ms
6.15e-5ms
5.8e-5ms
```

So, in average, 0.00006 ms.

### Parsec

As for using a hand-made parser with attoparsec, I got this times in average:

```
5.77e-5ms
7.33e-5ms
5.9800000000000003e-5ms
```

Although _I did_ get some outliers with e-4 instead of e-5, but overall, they seem to perform similarly.

So, in average, 0.00006 ms.

## Conclusions

I strongly suspect I'm fucking up somewhere and the parsing isn't actually being executed due to Haskell's lazy evaluation. I will look into [criterion](https://hackage.haskell.org/package/criterion-0.5.0.0/docs/Criterion-Main.html) to make some better conclusions.

As for reference, I run some tests prior which _suggested_ the average times were ~60ms for the Aeson one and ~100ms for attoparsec, but I'm not quite sure since it may have taken into account the parsing of the file (again, lazy evaluation, which I'm not well versed when mixed with IO).

## Kotlin

The thing with Kotlin (well, the JVM in general) is that we do not have generic types in runtime. While Haskell binds all of its polymorphic functions and runtime, Java does not, it simply [erases the types](https://en.wikipedia.org/wiki/Generics_in_Java#Problems_with_type_erasure). Thus, whenever Java needs to deal with a generic at runtime, it needs to perform a cast, which has a [big overhead](https://stackoverflow.com/questions/2170872/does-java-casting-introduce-overhead-why).

Enter Jackson's (or Gson, if you so want it) ObjectMapper to convert back and forth between JSON and Java objects. Taking a look at this signature:

```java
public <T> T readValue(String source, TypeReference<T> ref)
```

We know now that this will lead to a cast at runtime, which introduces overhead into our program.

So, can we learn something from Haskell with compile time deserialization to improve times? Or is Jackson competitive as it is? Let's take a look!

### Jackson

My first tests confirmed my suspicions: Jackson is slow (like, 10x slower)
```hs
731ms
726ms
756ms
```

However, I decided to run the test multiple times (because that's what the pros do, man). And I was quite shocked (shocked, I tell ya!) when the time it took to deserialize the file drastically fell from ~700ms to ~20ms. Turns out, [Jackson caches its reflection since version 2.8](https://github.com/FasterXML/jackson-module-kotlin/issues/44).

I also tried reusing a type reference instead of recreating one, but I did not get much better speed (although that last one may help with memory usage, which I'm not taking into account right now).

One configuration I have not been able to successfully try is to recreate the object mapper in each iteration. It seems the compiler notices that it's recreating the same object in a loop and decide to re-use it, instead (although I may be wrong, since my JVM-bytecode-foo is not good and Jackon is actually _that_ good).

Anyways, here are some results:
```
=== Jackson: Default behaviour (reflection field caching) over 1000 iterations ===

First read: 703
Last read: 7
Fastest read: 6 (at position 33)
Slowest read: 703 (at position 1)
Average read: 7.996

=== Jackson: Default behaviour reusing type reference (over 1000 iterations) ===

First read: 7
Last read: 7
Fastest read: 6 (at position 3)
Slowest read: 9 (at position 18)
Average read: 6.847

```

We have a pretty tall mountain to climb, eh?

### Intermediate solution

In Aeson, we have `Value` as the tree representing the JSON, while with Jackson, we have `JSONNode`, which also representes the tree. Jackson deserializes the JSON to this structure, and then performs the conversion to our objects. We will deserialize every JSON to this structure, and then use this structure to convert to our objects (just like we would do with custom `FromJSON` instances).

```
=== Intermediate over 1000 iterations ===

First read: 33
Last read: 5
Fastest read: 4 (at position 46)
Slowest read: 33 (at position 1)
Average read: 4.736
```

We see that this solution offers consistently low times too, and, in average, beats both Jackson's implementations by 2ms.

### Conclusions

Honestly, with Jackson's caching I haven't really been able to find much reason to not use it (or any other JSON-object mapper, for that matter). Maybe if you're hard pressed for memory or you have _really_ high volume if might make sense, but in those cases, maybe the reasonable thing to do would be to migrate from the JVM to something quicker.

One thing I did not is how times tend to decrease with every subsequent  iteration, and I believe that is because Java does some caching of sorts on its types (or I might just be talking shit and it just really fluctuates). I did some tests on that, and I found that if I added a delay between iterations, the average went somewhat higher (note: I did far fewer iterations with this scheme because I didn't want to sit here for an enternity).

```
=== Jackson: Default behaviour with delay over 10 iterations ===

First read: 7
Last read: 32
Fastest read: 7 (at position 1)
Slowest read: 38 (at position 4)
Average read: 28.3

=== Intermediate with delay over 10 iterations ===

First read: 6
Last read: 7
Fastest read: 6 (at position 1)
Slowest read: 24 (at position 2)
Average read: 14.9
```

In this case, it seems that Jackson does suffer quite a bit, with an average of almost double of the intermediate solution.