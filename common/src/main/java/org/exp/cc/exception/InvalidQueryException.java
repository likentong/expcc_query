package org.exp.cc.exception;

/**
 * Invalid query exception.
 */
public class InvalidQueryException extends ApplicationRuntimeException {
    public InvalidQueryException(final String message) {
        super(message);
    }
}
