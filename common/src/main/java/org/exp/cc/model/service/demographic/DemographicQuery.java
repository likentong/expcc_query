package org.exp.cc.model.service.demographic;

import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;

import java.util.List;
import java.util.Map;

/**
 * Demographic query model.
 */
public class DemographicQuery extends QueryCriteria {

    public DemographicQuery(final List<Map<String, QueryFields>> query) {
        super(query);
    }
}
