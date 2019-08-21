package org.exp.cc.exception;

/**
 * Application run time exception.
 */
public class ApplicationRuntimeException extends RuntimeException {
    public ApplicationRuntimeException(final String message) {
        super(message);
    }
}
