package org.exp.cc.processor.mongo;

import org.exp.cc.model.persistence.QueryFields;
import org.springframework.data.mongodb.core.query.Criteria;

/**
 * Comparison operator processor.
 */
public interface ComparisonOperatorProcessor {
    /**
     * Generate mongodb criteria based on query fields.
     * @param queryFields query fields
     * @return Criteria array
     */
    Criteria[] generateCriteria(QueryFields queryFields);
}
