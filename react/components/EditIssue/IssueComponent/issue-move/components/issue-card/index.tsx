import React, {
  useEffect, useState, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Row, Col,
} from 'choerodon-ui';
import { Tooltip } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import {
  Issue, IField, User, IIssueType,
} from '@/common/types';
import {
  includes, map, uniq, compact, flatten, find, keyBy,
} from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import TypeTag from '@/components/TypeTag';
import styles from './index.less';
import Field from './Field';
import transformValue from './transformValue';
import store, { FieldWithValue, MoveTarget } from '../../store';

export interface IssueCardProps {
  // record: Record
  sourceIssue: Issue
  sourceFields: FieldWithValue[]
}
const IssueCard: React.FC<IssueCardProps> = ({ sourceIssue, sourceFields }) => {
  const { target } = store.issueMap.get(sourceIssue.issueId)!;
  const { fields, issue: targetIssue } = target;
  return (
    <div className={styles.issueItem}>
      <div className={styles.issueItemHeader}>
        <TypeTag data={targetIssue.issueTypeVO} />
        <span className={styles.issueNum}>{sourceIssue.issueNum}</span>
        <span className={styles.summary}>{sourceIssue.summary}</span>
      </div>
      <div className={styles.issueItemFields}>
        <Row className={styles.fieldHeaderRow}>
          <Col span={7}>
            <span className={styles.fieldHeader}>字段</span>
          </Col>
          <Col span={8}>
            <span className={styles.fieldHeader}>原始值</span>
          </Col>
          <Col span={9}>
            <span className={styles.fieldHeader}>更新值</span>
          </Col>
        </Row>
        {
          fields.map((field) => {
            const { fieldCode, fieldName, required } = field;
            const sourceValue = transformValue({ issue: sourceIssue, field, fieldsWithValue: sourceFields });
            return (
              <Row key={fieldCode} className={styles.fieldRow}>
                <Col span={7}>
                  <span className={`${styles.fieldReadOnly} ${styles.fieldNameCol}`}>
                    {fieldName}
                    {required && (<span className={styles.required}>*</span>)}
                  </span>
                </Col>
                <Col span={8}>
                  <Tooltip title={sourceValue}>
                    <span className={styles.fieldReadOnly}>{sourceValue}</span>
                  </Tooltip>
                </Col>
                <Col span={9}>
                  <Field field={field} target={target} />
                </Col>
              </Row>
            );
          })
        }
      </div>
    </div>
  );
};

export default observer(IssueCard);
