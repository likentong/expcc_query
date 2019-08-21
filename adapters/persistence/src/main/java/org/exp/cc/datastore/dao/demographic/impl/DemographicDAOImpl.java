package org.exp.cc.datastore.dao.demographic.impl;

import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.model.persistence.QueryCriteria;

import java.util.List;
import java.util.Optional;

/**
 * Demographic DAO implementation.
 */
public class DemographicDAOImpl implements DemographicDAO {
    @Override
    public Optional<List<Object>> getIds(final QueryCriteria filter) {
        throw new UnsupportedOperationException();
    }
}
