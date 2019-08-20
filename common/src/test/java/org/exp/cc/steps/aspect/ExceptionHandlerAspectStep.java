package org.exp.cc.steps.aspect;

import com.google.common.collect.ImmutableSet;
import org.aspectj.lang.JoinPoint;
import org.exp.cc.aspect.ExceptionHandlerAspect;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

/**
 * Exception handler aspect step.
 */
public class ExceptionHandlerAspectStep {
    private final TestLogger logger = TestLoggerFactory.getTestLogger(ExceptionHandlerAspect.class);
    private final ExceptionHandlerAspect exceptionHandlerAspect;
    private JoinPoint joinPoint;

    public ExceptionHandlerAspectStep() {
        this.exceptionHandlerAspect = new ExceptionHandlerAspect();
    }

    /**
     * Step to setup join point.
     * @param joinPoint join point
     * @return this instance
     */
    public ExceptionHandlerAspectStep givenISetupJoinPoint(final JoinPoint joinPoint) {
        this.joinPoint = joinPoint;
        return this;
    }

    /**
     * Step to setup logging level.
     * @param enabledLevels enabled logging level
     * @return this instance
     */
    public ExceptionHandlerAspectStep givenISetLoggingLevel(final ImmutableSet<Level> enabledLevels) {
        this.logger.setEnabledLevels(enabledLevels);
        return this;
    }

    /**
     * Step to call logException from Exception handler aspect.
     * @param ex throwable
     * @return this instance
     */
    public ExceptionHandlerAspectStep whenILogException(final Throwable ex) {
        this.exceptionHandlerAspect.logException(this.joinPoint, ex);
        return this;
    }

    /**
     * Step to assert expected logging.
     * @param loggingEvent expected logging event
     * @return this instance
     */
    public ExceptionHandlerAspectStep thenLoggingShouldBe(final List<LoggingEvent> loggingEvent) {
        assertThat(this.logger.getLoggingEvents()).isEqualTo(loggingEvent);
        return this;
    }
}
