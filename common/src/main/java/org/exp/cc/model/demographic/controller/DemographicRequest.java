package org.exp.cc.model.demographic.controller;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.exp.cc.model.persistence.FilterCriteria;
import org.exp.cc.model.persistence.QueryFields;

import java.util.List;
import java.util.Map;

/**
 * Demographic request model.
 */
public class DemographicRequest extends FilterCriteria {

    @JsonCreator
    public DemographicRequest(@JsonProperty("filter") final List<Map<String, QueryFields>> filter) {
        super(filter);
    }
}
