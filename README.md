## CounterpointerReport

This library reports on a 1st species counterpoint and tells you which rules are violated.
Its based on a [1:1 Counterpoint lecture by Jacob Gran](https://www.youtube.com/watch?v=b5PoTBOj7Xc&list=PL6Towqbh0pdpxUL5NlGTOW2hwVVo1IhdQ)

The cantus firmus is by Fancois-Josepf Fetis (1784-1871)
The counterpoint is by Jacob Gran

![counterpoint](https://github.com/user-attachments/assets/f7c75a2f-8785-42af-b30d-67cc2765a883)

```
CounterpointReport {
  isValidFirstSpeciesR = False,
  noDissonantIntervalsR = True,
  noLeapsInSameDirectionR = True,
  avoidParallelFourthsR = True,
  avoidParallelFifthsOrOctavesR = False,
  unisonOnlyBeginOrEndR = True,
  approachPerfectionR = True,
  beginAndEndPerfectlyR = True,
  mostlyContraryR = True,
  limitIntervalR = True,
  lessThan3RepeatedIntervalsR = True,
  mostlyStepsCantusFirmusR = True,
  mostlyStepsCounterpointR = True,
  onlyConsonantLeapsR = True
}
```

This library uses Euterpea to render the MIDI output, and modular-arithmetic to calculate the intervals.
