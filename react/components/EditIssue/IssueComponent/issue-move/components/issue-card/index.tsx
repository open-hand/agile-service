import React from 'react';
import { observer } from 'mobx-react-lite';
import {
  Row, Col,
} from 'choerodon-ui';
import { Tooltip } from 'choerodon-ui/pro';
import { Issue } from '@/common/types';
import TypeTag from '@/components/TypeTag';
import styles from './index.less';
import Field from './Field';
import transformValue from './transformValue';
import store, { FieldWithValue } from '../../store';

export interface IssueCardProps {
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
              <Row key={fieldCode} className={styles.fieldRow} type="flex" align="middle">
                <Col span={7}>
                  <div className={`${styles.fieldReadOnly} ${styles.fieldNameCol}`}>
                    {fieldName}
                    {required && (<span className={styles.required}>*</span>)}
                  </div>
                </Col>
                <Col span={8}>
                  <Tooltip title={sourceValue}>
                    <div className={styles.fieldReadOnly}>{sourceValue}</div>
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
