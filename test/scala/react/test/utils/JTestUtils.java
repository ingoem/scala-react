package scala.react.test.utils;

import org.junit.Rule;
import org.junit.rules.TestName;

public class JTestUtils {
  // This needs to be a public member, so needs to be in a Java class.
  @Rule public TestName testName = new TestName();
}