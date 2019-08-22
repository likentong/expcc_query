package org.exp.cc.model.demographic.service;

import org.exp.cc.model.demographic.controller.DemographicResponse;

import java.util.List;
import java.util.Map;

/**
 * Demographic result model.
 */
public class DemographicResult extends DemographicResponse {

    public DemographicResult(final List<Map<String, Object>> result, final Map<String, Object> summary) {
        super(result, summary);
    }
}
