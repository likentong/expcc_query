package org.exp.cc.steps.aspect;

import com.google.common.collect.ImmutableSet;
import org.aspectj.lang.JoinPoint;
import org.exp.cc.aspect.ExceptionLogHandlerAspect;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

/**
 * Exception handler aspect step.
 */
public class ExceptionLogHandlerAspectStep {
    private final TestLogger logger = TestLoggerFactory.getTestLogger(ExceptionLogHandlerAspect.class);
    private final ExceptionLogHandlerAspect exceptionHandlerAspect;
    private JoinPoint joinPoint;

    public ExceptionLogHandlerAspectStep() {
        this.exceptionHandlerAspect = new ExceptionLogHandlerAspect();
    }

    /**
     * Step to setup join point.
     * @param joinPoint join point
     * @return this instance
     */
    public ExceptionLogHandlerAspectStep givenISetupJoinPoint(final JoinPoint joinPoint) {
        this.joinPoint = joinPoint;
        return this;
    }

    /**
     * Step to setup logging level.
     * @param enabledLevels enabled logging level
     * @return this instance
     */
    public ExceptionLogHandlerAspectStep givenISetLoggingLevel(final ImmutableSet<Level> enabledLevels) {
        this.logger.setEnabledLevels(enabledLevels);
        return this;
    }

    /**
     * Step to call logException from Exception handler aspect.
     * @param ex throwable
     * @return this instance
     */
    public ExceptionLogHandlerAspectStep whenILogException(final Throwable ex) {
        this.exceptionHandlerAspect.logException(this.joinPoint, ex);
        return this;
    }

    /**
     * Step to assert expected logging.
     * @param loggingEvent expected logging event
     * @return this instance
     */
    public ExceptionLogHandlerAspectStep thenLoggingEventShouldBe(final List<LoggingEvent> loggingEvent) {
        assertThat(this.logger.getLoggingEvents()).isEqualTo(loggingEvent);
        return this;
    }
}
