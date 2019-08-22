package org.exp.cc.model.service;

import org.exp.cc.model.controller.Response;

import java.util.List;
import java.util.Map;

/**
 * Result model.
 */
public class Result extends Response {

    public Result(final List<Map<String, Object>> result, final Map<String, Object> summary) {
        super(result, summary);
    }
}
