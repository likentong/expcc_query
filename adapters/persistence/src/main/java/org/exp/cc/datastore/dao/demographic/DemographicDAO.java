package org.exp.cc.datastore.dao.demographic;

import org.exp.cc.model.persistence.QueryCriteria;

import java.util.List;
import java.util.Optional;

/**
 * Demographic DAO.
 */
public interface DemographicDAO {
    /**
     * Get all demographic id based on filter.
     * @param filter filter
     * @return list of object
     */
    Optional<List<Object>> getIds(QueryCriteria filter);
}
