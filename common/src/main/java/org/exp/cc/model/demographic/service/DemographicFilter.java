package org.exp.cc.model.demographic.service;

import org.exp.cc.model.persistence.FilterCriteria;
import org.exp.cc.model.persistence.QueryOperator;

import java.util.List;

/**
 * Demographic filter model.
 */
public class DemographicFilter extends FilterCriteria {

    public DemographicFilter(final List<QueryOperator> filter) {
        super(filter);
    }
}
