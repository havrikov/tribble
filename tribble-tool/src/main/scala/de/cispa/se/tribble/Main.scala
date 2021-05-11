package de.cispa.se.tribble

import dk.brics.automaton.Automaton
import org.backuity.clist.Cli

/* Notes on command line parsing:

- Scopt won't work because the entire parser statically works with only one type, which doesn't play nice with modules
- Picocli doesn't seem to work with Scala classes
- Scallop loses type information on subcommand retrieval
- Clist seems to work for now
*/

object Main {
  Automaton.setAllowMutate(false) // treat all automata as immutable
  Automaton.setMinimizeAlways(true) // always work with minimal automata


  def main(args: Array[String]): Unit = {
    Cli.parse(args)
      .withProgramName("tribble")
      .withCommands(
        new GenerateTask,
        new GenerateForestationTask,
        new InlineGrammarTask,
        new ExtractAlternationsTask,
        new CacheGrammarTask,
      ).foreach(_.execute())
  }

}
