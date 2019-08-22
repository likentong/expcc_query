package org.exp.cc.datastore.dao.demographic.impl;

import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.model.persistence.QueryCriteria;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;

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
        checkArgument(queryCriteria != null, "queryCriteria cannot be null.");

        final List<Map<String, Object>> results = dataStoreDAO.queryData(PersistenceConstant.Entity.DEMOGRAPHIC, queryCriteria, Collections.singletonList(PersistenceConstant.Demographic.ID));

        return results.stream()
                .map(entry -> entry.get(PersistenceConstant.Demographic.ID))
                .collect(Collectors.toList());
    }

}
