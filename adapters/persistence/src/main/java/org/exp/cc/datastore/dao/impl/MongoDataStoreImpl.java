package org.exp.cc.datastore.dao.impl;

import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.model.persistence.FilterCriteria;

import java.util.List;
import java.util.Map;

/**
 * Mongodb data store implementation.
 */
public class MongoDataStoreImpl implements DataStoreDAO {
    @Override
    public Map<String, Object> queryData(final String entity, final FilterCriteria filter, final List<String> fieldsToRetrive) {
        throw new UnsupportedOperationException();
    }
}
