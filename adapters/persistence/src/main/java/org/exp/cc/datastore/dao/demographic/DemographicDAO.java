package org.exp.cc.datastore.dao.demographic;

import org.exp.cc.model.persistence.FilterCriteria;

import java.util.List;

/**
 * Demographic DAO.
 */
public interface DemographicDAO {
    /**
     * Get all demographic id based on filter.
     * @param filter filter
     * @return list of object
     */
    List<Object> getIds(FilterCriteria filter);
}
