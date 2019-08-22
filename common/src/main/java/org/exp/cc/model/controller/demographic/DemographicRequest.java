package org.exp.cc.model.controller.demographic;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;

import java.util.List;
import java.util.Map;

/**
 * Demographic request model.
 */
public class DemographicRequest extends QueryCriteria {

    @JsonCreator
    public DemographicRequest(@JsonProperty("query") final List<Map<String, QueryFields>> filter) {
        super(filter);
    }
}
