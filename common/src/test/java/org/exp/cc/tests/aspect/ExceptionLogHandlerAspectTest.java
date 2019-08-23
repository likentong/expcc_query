package org.exp.cc.tests.aspect;

import com.google.common.collect.ImmutableSet;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.Signature;
import org.exp.cc.steps.aspect.ExceptionLogHandlerAspectStep;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import uk.org.lidalia.slf4jext.ConventionalLevelHierarchy;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static uk.org.lidalia.slf4jtest.LoggingEvent.debug;
import static uk.org.lidalia.slf4jtest.LoggingEvent.error;

/**
 * Exception handler aspect test.
 */
// Unit test using BDD method and assertion in steps.
@SuppressWarnings("squid:S2699")
public class ExceptionLogHandlerAspectTest {
    private static final String DEBUG_LOGGING = "Arguments for method {}: {}";
    private static final String ERROR_LOGGING = "Exception from {}.{}: {}";
    private static final String[] METHOD_ARGS = new String[]{"arg1", "arg2", "arg3"};

    private final ExceptionLogHandlerAspectStep step;
    private JoinPoint joinPoint;
    private Signature signature;

    public ExceptionLogHandlerAspectTest() {
        this.step = new ExceptionLogHandlerAspectStep();
    }

    @BeforeAll
    public static void beforeAll() {
        TestLoggerFactory.clearAll();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        joinPoint = mock(JoinPoint.class);
        signature = mock(Signature.class);

        when(joinPoint.getTarget()).thenReturn(new Object());
        when(joinPoint.getSignature()).thenReturn(signature);
        when(joinPoint.getArgs()).thenReturn(METHOD_ARGS);

        final String testMethodName = "testMethod(..)";

        when(signature.toString()).thenReturn(testMethodName);

        this.step.givenISetupJoinPoint(joinPoint);
    }

    @AfterEach
    public void cleanUp() {
        TestLoggerFactory.clear();
    }

    /**
     * Test to log any throwable with error log.
     * @param ex throwable
     */
    @ParameterizedTest
    @MethodSource("exceptions")
    public void logException_AnyThrowable_LogWithCorrectMessage(final Throwable ex) {
        this.step.givenISetLoggingLevel(ConventionalLevelHierarchy.ERROR_LEVELS)
                .whenILogException(ex)
                .thenLoggingShouldBe(newArrayList(error(ex, ERROR_LOGGING, Object.class.getName(), signature, ex.getMessage())));
    }

    /**
     * Test on logging levels except debug with error log.
     * @param enabledLevels enabled logging levels
     */
    @ParameterizedTest
    @MethodSource("loggingLevels")
    public void logException_LoggingLevel_LogWithCorrectMessage(final ImmutableSet<Level> enabledLevels) {
        final RuntimeException exception = new RuntimeException();

        this.step.givenISetLoggingLevel(ConventionalLevelHierarchy.ERROR_LEVELS)
                .whenILogException(exception)
                .thenLoggingShouldBe(newArrayList(error(exception, ERROR_LOGGING, Object.class.getName(), signature, exception.getMessage())));
    }

    /**
     * Test debug logging level with debug log.
     * @param ex throwable
     */
    @ParameterizedTest
    @MethodSource("exceptions")
    public void logException_DebugLoggingLevel_LogWithCorrectMessage(final Throwable ex) {
        this.step.givenISetLoggingLevel(ConventionalLevelHierarchy.DEBUG_LEVELS)
                .whenILogException(ex)
                .thenLoggingShouldBe(newArrayList(
                        debug(DEBUG_LOGGING, signature, Arrays.toString(METHOD_ARGS)),
                        error(ex, ERROR_LOGGING, Object.class.getName(), signature, ex.getMessage()))
                );
    }

    private static Stream<Throwable> exceptions() {
        return Stream.of(
                new NullPointerException("null pointer exception"),
                new RuntimeException(),
                new FileNotFoundException()
        );
    }

    private static Stream<ImmutableSet<Level>> loggingLevels() {
        return Stream.of(
                ConventionalLevelHierarchy.ERROR_LEVELS,
                ConventionalLevelHierarchy.INFO_LEVELS,
                ConventionalLevelHierarchy.TRACE_LEVELS,
                ConventionalLevelHierarchy.WARN_LEVELS
        );
    }

}
