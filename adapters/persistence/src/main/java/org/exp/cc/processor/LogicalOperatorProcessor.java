package org.exp.cc.processor;

import org.exp.cc.model.persistence.QueryCriteria;
import org.springframework.data.mongodb.core.query.Criteria;

/**
 * Logical operator processor.
 */
public interface LogicalOperatorProcessor {
    /**
     * Generate mongodb criteria based on query.
     * @param queryCriteria query criteria
     * @return mongodb criteria
     */
    Criteria generateCriteria(QueryCriteria queryCriteria);
}
