package org.exp.cc.service;

import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.model.service.Result;

/**
 * Demographic service.
 */
public interface DemographicService {
    /**
     * Query data based on given filter.
     * @param query query
     * @return result
     */
    Result queryData(DemographicQuery query);
}
