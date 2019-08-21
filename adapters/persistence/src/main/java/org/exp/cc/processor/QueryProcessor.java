package org.exp.cc.processor;

import org.exp.cc.model.persistence.QueryCriteria;
import org.springframework.data.mongodb.core.query.Criteria;

/**
 * Query processor.
 */
public interface QueryProcessor {
    /**
     * Generate mongodb criteria based on query.
     * @param queryCriteria query criteria
     * @return mongodb criteria
     */
    Criteria generateCriteria(QueryCriteria queryCriteria);
}
