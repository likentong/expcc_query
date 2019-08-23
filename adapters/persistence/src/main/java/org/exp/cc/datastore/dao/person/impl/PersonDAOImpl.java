package org.exp.cc.datastore.dao.person.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.model.persistence.QueryOperator;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Person DAO implementation.
 */
@Repository
public class PersonDAOImpl implements PersonDAO {
    private final DataStoreDAO dataStoreDAO;

    public PersonDAOImpl(final DataStoreDAO dataStoreDAO) {
        this.dataStoreDAO = dataStoreDAO;
    }

    @Override
    public List<Map<String, Object>> getPersonCountByDemographicId(final Set<Object> id) {
        final QueryOperator queryOperator = new QueryOperator(ImmutableMap.of(ComparisonOperator.IN.getOperator(), id));
        final QueryFields queryFields = new QueryFields(ImmutableMap.of(PersistenceConstant.Demographic.ID, queryOperator));
        final Map<String, AggregationOperator> fieldsToAggregate = ImmutableMap.of(PersistenceConstant.Person.ID_PERSON, AggregationOperator.SUM);

        final AggregationCriteria aggregationCriteria = new AggregationCriteria(
                queryFields,
                Collections.singletonList(PersistenceConstant.Demographic.ID),
                fieldsToAggregate);

        return this.dataStoreDAO.aggregateData(PersistenceConstant.Entity.PERSON, aggregationCriteria);
    }

    @Override
    public List<Map<String, Object>> getPersonByDemographicId(final Set<Object> id) {
        return this.dataStoreDAO.queryData(
                PersistenceConstant.Entity.PERSON,
                String.format("{ %s : { $in : [%s] }}", PersistenceConstant.Demographic.ID,
                        id.stream()
                                .map(e -> (String) e)
                                .collect(Collectors.joining(","))
                ), Collections.emptyList());
    }
}
