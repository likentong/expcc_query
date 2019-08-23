package org.exp.cc.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Arrays;

/**
 * Exception handler aspect.
 */
@Aspect
@Component
public class ExceptionLogHandlerAspect {
    private static final Logger logger = LoggerFactory.getLogger(ExceptionLogHandlerAspect.class);

    @AfterThrowing(
            value = "@annotation(org.exp.cc.annotation.ExceptionLogHandler)",
            throwing = "ex"
    )
    public void logException(final JoinPoint joinPoint, final Throwable ex) {
        if (logger.isDebugEnabled()) {
            final String arguments = Arrays.toString(joinPoint.getArgs());
            logger.debug("Arguments for method {}: {}", joinPoint.getSignature(), arguments);
        }

        logger.error("Exception from {}.{}: {}",
                joinPoint.getTarget().getClass().getName(),
                joinPoint.getSignature(),
                ex.getMessage(),
                ex);
    }
}
