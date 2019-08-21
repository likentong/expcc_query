package org.exp.cc.datastore.dao.demographic;

import org.exp.cc.model.persistence.QueryCriteria;

import java.util.List;

/**
 * Demographic DAO.
 */
public interface DemographicDAO {
    /**
     * Get all demographic id based on filter.
     * @param queryCriteria queryCriteria
     * @return list of object
     */
    List<Object> getIds(QueryCriteria queryCriteria);
}
