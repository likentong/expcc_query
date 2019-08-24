package org.exp.cc.controller;

import org.exp.cc.exception.InvalidQueryException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Custom exception handler.
 */
@RestControllerAdvice
public class RestExceptionHandler {

    @ExceptionHandler(InvalidQueryException.class)
    public void handleInvalidQuery(final HttpServletResponse response) throws IOException {
        response.sendError(HttpStatus.BAD_REQUEST.value());
    }
}
