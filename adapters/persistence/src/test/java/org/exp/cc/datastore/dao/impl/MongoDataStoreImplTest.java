package org.exp.cc.datastore.dao.impl;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.bson.Document;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.impl.steps.MongoDataStoreImplStep;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.processor.mongo.AggregationOperatorProcessor;
import org.exp.cc.processor.mongo.LogicalOperatorProcessor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.query.BasicQuery;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Mongo data store implementation test.
 */
public class MongoDataStoreImplTest {
    private static final String BLANK_ENTITY_ERROR = "entity cannot be null or blank.";
    private static final String NULL_FIELDS_ERROR = "fieldsToRetrive cannot be null.";
    private static final String NULL_QUERY_CRITERIA_ERROR = "queryCriteria cannot be null.";
    private static final String BLANK_QUERY_STRING_ERROR = "query string cannot be null or blank.";
    private static final String NULL_AGGREGATION_CRITERIA_ERROR = "aggregationCriteria cannot be null.";

    private static final String QUERY_VALUE = "{ 'field1': { $lt: 3} }";
    private static final String ENTITY_VALUE = "entity";

    private MongoDataStoreImplStep step;
    private AggregationCriteria aggregationCriteria;
    private QueryCriteria queryCriteria;
    private Criteria criteria;

    public MongoDataStoreImplTest() {
        this.step = new MongoDataStoreImplStep();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        final LogicalOperatorProcessor logicalOperatorProcessor = mock(LogicalOperatorProcessor.class);
        final AggregationOperatorProcessor aggregationOperatorProcessor = mock(AggregationOperatorProcessor.class);
        final Document document = mock(Document.class);
        final Aggregation aggregation = mock(Aggregation.class);

        this.criteria = mock(Criteria.class);
        this.queryCriteria = mock(QueryCriteria.class);
        this.aggregationCriteria = mock(AggregationCriteria.class);

        when(this.criteria.getCriteriaObject()).thenReturn(document);
        when(logicalOperatorProcessor.generateCriteria(this.queryCriteria)).thenReturn(this.criteria);
        when(aggregationOperatorProcessor.generateAggregation(this.aggregationCriteria)).thenReturn(aggregation);

        this.step.givenISetupMongoDataStoreImpl(logicalOperatorProcessor, aggregationOperatorProcessor);
    }

    /**
     * queryData with query criteria and blank entity, should throws {@link IllegalArgumentException}.
     * @param entity entity
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    public void queryData_WithQueryCriteria_WithBlankEntity_ThrowsIllegalArgumentException(final String entity) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(entity, this.queryCriteria, Collections.emptyList()))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(BLANK_ENTITY_ERROR);
    }

    /**
     * queryDaya with null queryCriteria, should throws {@link IllegalArgumentException}.
     * @param queryCriteria query criteria
     */
    @ParameterizedTest
    @NullSource
    public void queryData_WithNullQueryCriteria_ThrowsIllegalArgumentException(final QueryCriteria queryCriteria) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(ENTITY_VALUE, queryCriteria, Collections.emptyList()))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(NULL_QUERY_CRITERIA_ERROR);
    }

    /**
     * queryData with query criteria with null fields to retrive, should throws {@link IllegalArgumentException}.
     * @param fieldsToRetrive fields to retrieve
     */
    @ParameterizedTest
    @NullSource
    public void queryData_WithQueryCriteria_WithNullFieldsToRetrieve_ThrowsIllegalArgumentException(final List<String> fieldsToRetrive) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(ENTITY_VALUE, this.queryCriteria, fieldsToRetrive))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(NULL_FIELDS_ERROR);
    }

    /**
     * queryData with query criteria with valid arguments, should return results.
     */
    @Test
    public void queryData_WithQueryCriteria_WithValidArguments_ShouldReturnResults() {
        final ImmutablePair<List<Document>, List<Map<String, Object>>> results = getPairOfMongoResultsAndDataStoreResults();
        final Query expectedQuery = new Query(this.criteria);
        expectedQuery.fields().exclude(PersistenceConstant.MongoDB.MONGO_OBJECT_ID).include("field1").include("field2");

        this.step.givenIMockMongoTemplateFind(ENTITY_VALUE, results.getLeft())
                .whenIQueryData(ENTITY_VALUE, this.queryCriteria, Arrays.asList("field1", "field2"))
                .thenResultsShouldBe(results.getRight())
                .thenQueryShouldBe(expectedQuery);
    }

    /**
     * queryData with query string with blank entity, should throws {@link IllegalArgumentException}.
     * @param entity entity
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    public void queryData_WithQueryString_WithBlankEntity_ThrowsIllegalArgumentException(final String entity) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(entity, QUERY_VALUE, Collections.emptyList()))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(BLANK_ENTITY_ERROR);
    }

    /**
     * queryData with query string with blank query string, should throws {@link IllegalArgumentException}.
     * @param queryString query string
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    public void queryData_WithQueryString_WithBlankQueryString_ThrowsIllegalArgumentException(final String queryString) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(ENTITY_VALUE, queryString, Collections.emptyList()))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(BLANK_QUERY_STRING_ERROR);
    }

    /**
     * queryData with query string with null fields to retrive, should throws {@link IllegalArgumentException}.
     * @param fieldsToRetrive fields to retrieve
     */
    @ParameterizedTest
    @NullSource
    public void queryData_WithQueryString_WithNullFieldsToRetrieve_ThrowsIllegalArgumentException(final List<String> fieldsToRetrive) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIQueryData(ENTITY_VALUE, QUERY_VALUE, fieldsToRetrive))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(NULL_FIELDS_ERROR);
    }

    /**
     * queryData with query string with valid arguments, should return correct results.
     */
    @Test
    public void queryData_WithQueryString_WithValidArguments_ShouldReturnResults() {
        final ImmutablePair<List<Document>, List<Map<String, Object>>> results = getPairOfMongoResultsAndDataStoreResults();
        final BasicQuery expectedBasicQuery = new BasicQuery(QUERY_VALUE, "{ _id: 0, field1: 1, field2: 1}");

        this.step.givenIMockMongoTemplateFind(ENTITY_VALUE, results.getLeft())
                .whenIQueryData(ENTITY_VALUE, QUERY_VALUE, Arrays.asList("field1", "field2"))
                .thenResultsShouldBe(results.getRight())
                .thenBasicQueryShouldBe(expectedBasicQuery);
    }

    /**
     * aggregateData with blank entity, should throws {@link IllegalArgumentException}.
     * @param entity entity
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    public void aggregateData_WithBlankEntity_ThrowsIllegalArgumentException(final String entity) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIAggregateData(entity, this.aggregationCriteria))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(BLANK_ENTITY_ERROR);
    }

    /**
     * aggregateData with null aggregation criteria, should throws {@link IllegalArgumentException}.
     * @param aggregationCriteria aggregation criteria
     */
    @ParameterizedTest
    @NullSource
    public void aggregateData_WithNullAggregationCriteria_ThrowsIllegalArgumentException(final AggregationCriteria aggregationCriteria) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIAggregateData(ENTITY_VALUE, aggregationCriteria))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(NULL_AGGREGATION_CRITERIA_ERROR);
    }

    /**
     * aggregateData with valid arguments, should return results.
     */
    @Test
    public void aggregateData_WithValidArguments_ReturnsExpectedResults() {
        final Map<String, Object> record1 = ImmutableMap.of("_id", 1, "sum", 3);
        final Map<String, Object> record2 = ImmutableMap.of("_id", 2, "sum", 15);

        final List<Document> mongoResults = newArrayList(new Document(record1), new Document(record2));

        final AggregationResults<Document> aggregationResults = new AggregationResults<>(mongoResults, new Document());

        final List<Map<String, Object>> results = newArrayList(record1, record2);

        this.step.givenIMockMongoTemplateAggregate(ENTITY_VALUE, aggregationResults)
                .whenIAggregateData(ENTITY_VALUE, this.aggregationCriteria)
                .thenResultsShouldBe(results);
    }

    private ImmutablePair<List<Document>, List<Map<String, Object>>> getPairOfMongoResultsAndDataStoreResults() {
        final Map<String, Object> record1 = ImmutableMap.of("_id", 1, "field1", 1, "field2", "1");
        final Map<String, Object> record2 = ImmutableMap.of("_id", 2, "field1", 2, "field2", "2");

        final List<Document> mongoResults = newArrayList(new Document(record1), new Document(record2));
        final List<Map<String, Object>> results = newArrayList(record1, record2);

        return new ImmutablePair<>(mongoResults, results);
    }

}
