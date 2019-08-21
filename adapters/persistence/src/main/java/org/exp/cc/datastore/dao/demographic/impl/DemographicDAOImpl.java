package org.exp.cc.datastore.dao.demographic.impl;

import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.model.persistence.QueryCriteria;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Demographic DAO implementation.
 */
@Repository
public class DemographicDAOImpl implements DemographicDAO {
    private final DataStoreDAO dataStoreDAO;

    public DemographicDAOImpl(final DataStoreDAO dataStoreDAO) {
        this.dataStoreDAO = dataStoreDAO;
    }

    @Override
    public List<Object> getIds(final QueryCriteria queryCriteria) {
        final List<Map<String, Object>> results = dataStoreDAO.queryData("demographic", queryCriteria, Collections.singletonList("ID"));

        return results.stream()
                .map(entry -> entry.get("ID"))
                .collect(Collectors.toList());
    }

}
