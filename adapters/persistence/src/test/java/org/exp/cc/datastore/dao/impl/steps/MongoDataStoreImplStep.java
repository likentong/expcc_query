package org.exp.cc.datastore.dao.impl.steps;

import org.bson.Document;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.impl.MongoDataStoreImpl;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.processor.mongo.AggregationOperatorProcessor;
import org.exp.cc.processor.mongo.LogicalOperatorProcessor;
import org.exp.cc.test.ThrowableStep;
import org.mockito.ArgumentCaptor;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.query.BasicQuery;
import org.springframework.data.mongodb.core.query.Query;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * Mongo data store implementation step.
 */
public class MongoDataStoreImplStep extends ThrowableStep<MongoDataStoreImplStep> {
    private List<Map<String, Object>> results;
    private MongoTemplate mongoTemplate;
    private String entity;
    private DataStoreDAO dataStoreDAO;
    private ArgumentCaptor<BasicQuery> basicQueryArgumentCaptor;
    private ArgumentCaptor<Query> queryArgumentCaptor;

    public MongoDataStoreImplStep() {
        this.basicQueryArgumentCaptor = ArgumentCaptor.forClass(BasicQuery.class);
        this.queryArgumentCaptor = ArgumentCaptor.forClass(Query.class);
        this.mongoTemplate = mock(MongoTemplate.class);
    }

    /**
     * Step with no setup.
     * @return return this
     */
    public MongoDataStoreImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to setup MongoDataStoreImpl.
     * @param logicalOperatorProcessor logical operator processor
     * @param aggregationOperatorProcessor aggregation operator processor
     * @return this instance
     */
    public MongoDataStoreImplStep givenISetupMongoDataStoreImpl(final LogicalOperatorProcessor logicalOperatorProcessor, final AggregationOperatorProcessor aggregationOperatorProcessor) {
        this.dataStoreDAO = new MongoDataStoreImpl(this.mongoTemplate, logicalOperatorProcessor, aggregationOperatorProcessor);
        return this;
    }

    /**
     * Step to mock MongoTemplate find.
     * @param entity entity
     * @param mongoResults mongo results
     * @return this instance
     */
    public MongoDataStoreImplStep givenIMockMongoTemplateFind(final String entity, final List<Document> mongoResults) {
        when(this.mongoTemplate.find(any(Query.class), eq(Document.class), eq(entity))).thenReturn(mongoResults);
        return this;
    }

    /**
     * Step to mock MongoTemplate aggregate.
     * @param entity entity
     * @param mongoResults mongo results
     * @return this instance
     */
    public MongoDataStoreImplStep givenIMockMongoTemplateAggregate(final String entity, final AggregationResults<Document> mongoResults) {
        when(this.mongoTemplate.aggregate(any(Aggregation.class), eq(entity), eq(Document.class))).thenReturn(mongoResults);
        return this;
    }

    /**
     * Step to call DataStoreDAO queryData.
     * @param entity entity
     * @param queryCriteria query criteria
     * @param fieldsToRetrive fields to retrieve
     * @return this instance
     */
    public MongoDataStoreImplStep whenIQueryData(final String entity, final QueryCriteria queryCriteria, final List<String> fieldsToRetrive) {
        this.entity = entity;
        this.results = this.dataStoreDAO.queryData(entity, queryCriteria, fieldsToRetrive);
        return this;
    }

    /**
     * Step to call DataStoreDAO queryData.
     * @param entity entity
     * @param query query
     * @param fieldsToRetrive fields to retrieve
     * @return this instance
     */
    public MongoDataStoreImplStep whenIQueryData(final String entity, final String query, final List<String> fieldsToRetrive) {
        this.entity = entity;
        this.results = this.dataStoreDAO.queryData(entity, query, fieldsToRetrive);
        return this;
    }

    /**
     * Step to call DataStoreDAO aggregateData.
     * @param entity entity
     * @param aggregationCriteria aggregation criteria
     * @return this instance
     */
    public MongoDataStoreImplStep whenIAggregateData(final String entity, final AggregationCriteria aggregationCriteria) {
        this.entity = entity;
        this.results = this.dataStoreDAO.aggregateData(entity, aggregationCriteria);
        return this;
    }

    /**
     * Step to assert expected results.
     * @param results expected results
     * @return this instance
     */
    public MongoDataStoreImplStep thenResultsShouldBe(final List<Map<String, Object>> results) {
        assertThat(this.results).isEqualTo(results);
        return this;
    }

    /**
     * Step to assert basic query sent by MongoTemplate.
     * @param basicQuery expected basic query
     * @return this instance
     */
    public MongoDataStoreImplStep thenBasicQueryShouldBe(final BasicQuery basicQuery) {
        verify(this.mongoTemplate).find(this.basicQueryArgumentCaptor.capture(), eq(Document.class), eq(this.entity));
        assertThat(this.basicQueryArgumentCaptor.getValue()).isEqualTo(basicQuery);
        return this;
    }

    /**
     * Step to assert query sent by MongoTemplate.
     * @param query query
     * @return this instance
     */
    public MongoDataStoreImplStep thenQueryShouldBe(final Query query) {
        verify(this.mongoTemplate).find(this.queryArgumentCaptor.capture(), eq(Document.class), eq(this.entity));
        assertThat(this.queryArgumentCaptor.getValue()).isEqualTo(query);
        return this;
    }

}
