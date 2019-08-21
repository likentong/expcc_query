package org.exp.cc.datastore.dao.impl;

import org.apache.commons.lang3.StringUtils;
import org.bson.Document;
import org.exp.cc.annotation.ExceptionHandler;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.processor.mongo.LogicalOperatorProcessor;
import org.exp.cc.processor.mongo.impl.MongoLogicalOperatorProcessorImpl;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.BasicQuery;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Mongodb data store implementation.
 */
@Repository
public class MongoDataStoreImpl implements DataStoreDAO {
    private final MongoTemplate mongoTemplate;
    private final LogicalOperatorProcessor logicalOperatorProcessor;

    public MongoDataStoreImpl(final MongoTemplate mongoTemplate,
                              final MongoLogicalOperatorProcessorImpl logicalOperatorProcessor) {
        this.mongoTemplate = mongoTemplate;
        this.logicalOperatorProcessor = logicalOperatorProcessor;
    }

    @ExceptionHandler
    @Override
    public List<Map<String, Object>> queryData(final String entity, final QueryCriteria queryCriteria, final List<String> fieldsToRetrive) {
        checkArgument(StringUtils.isNotBlank(entity), "entity cannot be null or blank.");
        checkArgument(queryCriteria != null, "query cannot be null.");
        checkArgument(fieldsToRetrive != null, "fieldsToRetrive cannot be null.");

        final Query query = new Query(this.logicalOperatorProcessor.generateCriteria(queryCriteria));
        final List<Document> mongoResults = this.mongoTemplate.find(query, Document.class, entity);

        return convertDocumentToMap(mongoResults);
    }

    @ExceptionHandler
    @Override
    public List<Map<String, Object>> queryData(final String entity, final String query, final List<String> fieldsToRetrive) {
        checkArgument(StringUtils.isNotBlank(entity), "entity cannot be null or blank.");
        checkArgument(StringUtils.isNotBlank(query), "query cannot be null or blank.");
        checkArgument(fieldsToRetrive != null, "fieldsToRetrive cannot be null.");

        final String projection = fieldsToRetrive.stream()
                .map(field -> "'" + field + "'" + ": 1")
                .collect(Collectors.joining(", ", "{", "}"));

        final BasicQuery basicQuery = new BasicQuery(query, projection);

        final List<Document> mongoResults = this.mongoTemplate.find(basicQuery, Document.class, entity);

        return convertDocumentToMap(mongoResults);
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
