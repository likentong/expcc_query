package org.exp.cc.model.demographic.service;

import org.exp.cc.model.persistence.FilterCriteria;
import org.exp.cc.model.persistence.QueryFields;

import java.util.List;
import java.util.Map;

/**
 * Demographic filter model.
 */
public class DemographicFilter extends FilterCriteria {

    public DemographicFilter(final List<Map<String, QueryFields>> filter) {
        super(filter);
    }
}
