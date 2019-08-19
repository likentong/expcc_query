package org.exp.cc.model.demographic.controller;

import org.exp.cc.model.persistence.FilterCriteria;
import org.exp.cc.model.persistence.QueryOperator;

import java.util.List;

/**
 * Demographic request model.
 */
public class DemographicRequest extends FilterCriteria {

    public DemographicRequest(final List<QueryOperator> filter) {
        super(filter);
    }
}
