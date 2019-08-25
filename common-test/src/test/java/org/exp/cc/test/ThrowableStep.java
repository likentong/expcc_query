package org.exp.cc.test;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.ThrowableAssert;

/**
 * Throwable step.
 */
public class ThrowableStep<T extends ThrowableStep> {
    private Throwable thrown;

    /**
     * Step to setup throwing callable.
     * @param throwingCallable throwing callable
     * @return this instance
     */
    public T givenISetupAThrowingCallable(final ThrowableAssert.ThrowingCallable throwingCallable) {
        this.thrown = ThrowableAssert.catchThrowable(throwingCallable);
        return (T) this;
    }

    /**
     * Step to assert exception class type.
     * @param exceptionClass exception class
     * @return this instance
     */
    public T thenExceptionMatchCorrectType(final Class<? extends Throwable> exceptionClass) {
        Assertions.assertThat(this.thrown).isExactlyInstanceOf(exceptionClass);
        return (T) this;
    }

    /**
     * Step to assert exception message.
     * @param exceptionMessage exception message
     * @return this instance
     */
    public T thenExceptionWithCorrectMessage(final String exceptionMessage) {
        Assertions.assertThat(this.thrown).hasMessage(exceptionMessage);
        return (T) this;
    }

}
