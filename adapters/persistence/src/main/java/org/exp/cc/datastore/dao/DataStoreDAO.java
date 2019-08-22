package org.exp.cc.datastore.dao;

import org.exp.cc.model.AggregationCriteria;
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
     * @param queryCriteria query criteria
     * @param fieldsToRetrive fields to retrive
     * @return result from data store
     */
    List<Map<String, Object>> queryData(String entity, QueryCriteria queryCriteria, List<String> fieldsToRetrive);

    /**
     * Query data based on given entity, query string and fields to retrieve.
     * @param entity entity
     * @param query query
     * @param fieldsToRetrive fields to retrive
     * @return result from data store
     */
    List<Map<String, Object>> queryData(String entity, String query, List<String> fieldsToRetrive);

    /**
     * Aggregate data based on given entity and aggregation criteria.
     * @param entity entity
     * @param aggregationCriteria aggregation criteria
     * @return result from data store
     */
    List<Map<String, Object>> aggregateData(String entity, AggregationCriteria aggregationCriteria);
}
