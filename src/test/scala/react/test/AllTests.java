package scala.react.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  TopoQueueTests.class,
  ReactiveTests.class,
  CombinatorTests.class,
  FlowTests.class,
  RouterTests.class,
  ReactorTests.class
  })
public final class AllTests {
}
