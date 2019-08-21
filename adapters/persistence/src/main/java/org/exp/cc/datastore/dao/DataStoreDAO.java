package org.exp.cc.datastore.dao;

import org.exp.cc.model.persistence.QueryCriteria;

import java.util.List;
import java.util.Map;

/**
 * Data store data access object.
 */
public interface DataStoreDAO {
    /**
     * Query data based on given entity, filter and fields to retrive.
     * @param entity entity
     * @param queryCriteria queryCriteria
     * @param fieldsToRetrive field to retrive
     * @return result from data store
     */
    List<Map<String, Object>> queryData(String entity, QueryCriteria queryCriteria, List<String> fieldsToRetrive);

    /**
     * Query data based on given entity, query string and fields to retrieve.
     * @param entity entity
     * @param query query
     * @param fieldsToRetrive field to retrive
     * @return result from data store
     */
    List<Map<String, Object>> queryData(String entity, String query, List<String> fieldsToRetrive);
}
