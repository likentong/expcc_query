package org.exp.cc.service;

import org.exp.cc.model.demographic.service.DemographicQuery;
import org.exp.cc.model.demographic.service.DemographicResult;

/**
 * Demographic service.
 */
public interface DemographicService {
    /**
     * Query data based on given filter.
     * @param query query
     * @return demographic result
     */
    DemographicResult queryData(DemographicQuery query);
}
