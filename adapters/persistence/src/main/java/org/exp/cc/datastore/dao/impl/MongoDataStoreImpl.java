package org.exp.cc.datastore.dao.impl;

import org.apache.commons.lang3.StringUtils;
import org.bson.Document;
import org.exp.cc.annotation.ExceptionHandler;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.processor.mongo.AggregationOperatorProcessor;
import org.exp.cc.processor.mongo.LogicalOperatorProcessor;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.query.BasicQuery;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;
import static org.exp.cc.constant.PersistenceConstant.MongoDB.MONGO_OBJECT_ID;

/**
 * Mongodb data store implementation.
 */
@Repository
public class MongoDataStoreImpl implements DataStoreDAO {
    private static final String BLANK_ENTITY_ERROR = "entity cannot be null or blank.";

    private final MongoTemplate mongoTemplate;
    private final LogicalOperatorProcessor logicalOperatorProcessor;
    private final AggregationOperatorProcessor aggregationOperatorProcessor;

    public MongoDataStoreImpl(final MongoTemplate mongoTemplate,
                              final LogicalOperatorProcessor logicalOperatorProcessor,
                              final AggregationOperatorProcessor aggregationOperatorProcessor) {
        this.mongoTemplate = mongoTemplate;
        this.logicalOperatorProcessor = logicalOperatorProcessor;
        this.aggregationOperatorProcessor = aggregationOperatorProcessor;
    }

    @ExceptionHandler
    @Override
    public List<Map<String, Object>> queryData(final String entity, final QueryCriteria queryCriteria, final List<String> fieldsToRetrive) {
        checkArgument(StringUtils.isNotBlank(entity), BLANK_ENTITY_ERROR);
        checkArgument(queryCriteria != null, "query cannot be null.");
        checkArgument(fieldsToRetrive != null, "fieldsToRetrive cannot be null.");

        final Query query = new Query(this.logicalOperatorProcessor.generateCriteria(queryCriteria));
        fieldsToRetrive.forEach(query.fields()::include);
        query.fields().exclude(MONGO_OBJECT_ID);

        final List<Document> mongoResults = this.mongoTemplate.find(query, Document.class, entity);

        return convertDocumentToMap(mongoResults);
    }

    @ExceptionHandler
    @Override
    public List<Map<String, Object>> queryData(final String entity, final String query, final List<String> fieldsToRetrive) {
        checkArgument(StringUtils.isNotBlank(entity), BLANK_ENTITY_ERROR);
        checkArgument(StringUtils.isNotBlank(query), "query cannot be null or blank.");
        checkArgument(fieldsToRetrive != null, "fieldsToRetrive cannot be null.");

        final List<String> projection = fieldsToRetrive.stream()
                .map(field -> "'" + field + "'" + ": 1")
                .collect(Collectors.toList());

        //remove default _id field
        projection.add(MONGO_OBJECT_ID + ": 0");

        final String projectionJson = projection.stream()
                .collect(Collectors.joining(", ", "{", "}"));

        final BasicQuery basicQuery = new BasicQuery(query, projectionJson);

        final List<Document> mongoResults = this.mongoTemplate.find(basicQuery, Document.class, entity);

        return convertDocumentToMap(mongoResults);
    }

    @Override
    public List<Map<String, Object>> aggregateData(final String entity, final AggregationCriteria aggregationCriteria) {
        checkArgument(StringUtils.isNotBlank(entity), BLANK_ENTITY_ERROR);
        checkArgument(aggregationCriteria != null, "aggregationCriteria cannot be null.");
        checkArgument(aggregationCriteria.getQueryFields() != null, "aggregationCriteria queryField cannot be null.");
        checkArgument(aggregationCriteria.getFieldsToAggregate() != null, "aggregationCriteria fieldsToAggregate cannot be null.");
        checkArgument(aggregationCriteria.getFieldsToGroup() != null, "aggregationCriteria fieldsToGroup cannot be null.");

        final AggregationResults<Document> mongoResults = this.mongoTemplate.aggregate(
                this.aggregationOperatorProcessor.generateAggregation(aggregationCriteria),
                entity,
                Document.class);

        return convertDocumentToMap(mongoResults.getMappedResults());
    }

    private List<Map<String, Object>> convertDocumentToMap(final List<Document> mongoDocuments) {
        return mongoDocuments.stream()
                .map(doc -> {
                    Map<String, Object> fields = new HashMap<>();

                    for (Map.Entry entry : doc.entrySet()) {
                        fields.put(entry.getKey().toString(), entry.getValue());
                    }

                    return fields;
                })
                .collect(Collectors.toList());
    }

}
