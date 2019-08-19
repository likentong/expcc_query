package org.exp.cc.service;

import org.exp.cc.model.demographic.service.DemographicFilter;
import org.exp.cc.model.demographic.service.DemographicResult;

/**
 * Demographic service.
 */
public interface DemographicService {
    /**
     * Query data based on given filter.
     * @param filter filter
     * @return demographic result
     */
    DemographicResult queryData(DemographicFilter filter);
}
