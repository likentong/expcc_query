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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Person DAO implementation.
 */
@Repository
public class PersonDAOImpl implements PersonDAO {
    private static final String ID_NULL_EXCEPTION_MESSAGE = "id cannot be null.";
    private static final String ID_EMPTY_EXCEPTION_MESSAGE = "id cannot be empty.";

    private final DataStoreDAO dataStoreDAO;

    public PersonDAOImpl(final DataStoreDAO dataStoreDAO) {
        this.dataStoreDAO = dataStoreDAO;
    }

    @Override
    public List<Map<String, Object>> getPersonCountByDemographicId(final Set<Object> id) {
        checkArgument(id != null, ID_NULL_EXCEPTION_MESSAGE);
        checkArgument(!id.isEmpty(), ID_EMPTY_EXCEPTION_MESSAGE);

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
        checkArgument(id != null, ID_NULL_EXCEPTION_MESSAGE);
        checkArgument(!id.isEmpty(), ID_EMPTY_EXCEPTION_MESSAGE);

        return this.dataStoreDAO.queryData(
                PersistenceConstant.Entity.PERSON,
                String.format("{ %s : { $in : [%s] }}", PersistenceConstant.Demographic.ID,
                        id.stream()
                                .map(String::valueOf)
                                .collect(Collectors.joining(","))
                ), Collections.emptyList());
    }
}
